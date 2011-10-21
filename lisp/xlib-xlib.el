;;; xlib-xlib.el --- Xlib next generation.

;; Copyright (C) 2005,2006 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Sun Aug 28 02:20:07 MSD 2005
;; Keywords: xlib, ffi

;; This file is part of XWEM.

;; XWEM is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XWEM is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; 

;;; Code:

(require 'ffi-xlib)
(require 'xlib-const)
(require 'xlib-common)

;;{{{ `-- Predefined atoms

(defconst XA-AnyPropertyType 0)
(defconst XA-primary 1)
(defconst XA-secondary 2)
(defconst XA-arc 3)
(defconst XA-atom 4)
(defconst XA-bitmap 5)
(defconst XA-cardinal 6)
(defconst XA-colormap 7)
(defconst XA-cursor 8)
(defconst XA-cut-buffer0 9)
(defconst XA-cut-buffer1 10)
(defconst XA-cut-buffer2 11)
(defconst XA-cut-buffer3 12)
(defconst XA-cut-buffer4 13)
(defconst XA-cut-buffer5 14)
(defconst XA-cut-buffer6 15)
(defconst XA-cut-buffer7 16)
(defconst XA-drawable 17)
(defconst XA-font 18)
(defconst XA-integer 19)
(defconst XA-pixmap 20)
(defconst XA-point 21)
(defconst XA-rectangle 22)
(defconst XA-resource-manager 23)
(defconst XA-rgb-color-map 24)
(defconst XA-rgb-best-map 25)
(defconst XA-rgb-blue-map 26)
(defconst XA-rgb-default-map 27)
(defconst XA-rgb-gray-map 28)
(defconst XA-rgb-green-map 29)
(defconst XA-rgb-red-map 30)
(defconst XA-string 31)
(defconst XA-visualid 32)
(defconst XA-window 33)
(defconst XA-wm-command 34)
(defconst XA-wm-hints 35)
(defconst XA-wm-client-machine 36)
(defconst XA-wm-icon-name 37)
(defconst XA-wm-icon-size 38)
(defconst XA-wm-name 39)
(defconst XA-wm-normal-hints 40)
(defconst XA-wm-size-hints 41)
(defconst XA-wm-zoom-hints 42)
(defconst XA-min-space 43)
(defconst XA-norm-space 44)
(defconst XA-max-space 45)
(defconst XA-end-space 46)
(defconst XA-superscript-x 47)
(defconst XA-superscript-y 48)
(defconst XA-subscript-x 49)
(defconst XA-subscript-y 50)
(defconst XA-underline-position 51)
(defconst XA-underline-thickness 52)
(defconst XA-strikeout-ascent 53)
(defconst XA-strikeout-descent 54)
(defconst XA-italic-angle 55)
(defconst XA-x-height 56)
(defconst XA-quad-width 57)
(defconst XA-weight 58)
(defconst XA-point-size 59)
(defconst XA-resolution 60)
(defconst XA-copyright 61)
(defconst XA-notice 62)
(defconst XA-font-name 63)
(defconst XA-family-name 64)
(defconst XA-full-name 65)
(defconst XA-cap-height 66)
(defconst XA-wm-class 67)
(defconst XA-wm-transient-for 68)

;;}}}

;;{{{ `-- Setting up Display

(defvar X:Dpy-list nil)

(defun X:Dpy-find (proc)
  "Find X display by its PROC."
  (find proc X:Dpy-list
        :test #'(lambda (i1 i2)
                  (eq i1 (get i2 'emacs-process)))))

(defun X:Dpy-setup (dpy &rest properties)
  "For display DPY setup its PROPERTIES."
  (while properties
    (put dpy (car properties) (cadr properties))
    (setq properties (cddr properties)))
    
  (push dpy X:Dpy-list)
  (let ((dp (connect-file-descriptor
             (X:DisplayString dpy) nil (Display->fd dpy) (Display->fd dpy))))
    (put dpy 'emacs-process dp)
    (when (fboundp 'set-process-coding-system)
      (set-process-coding-system dp 'binary 'binary))
    (set-process-filter dp 'X:Dpy-filter t)))

;;}}}
;;{{{ `-- Display logging facilities

;; Supported routines are:
;;   x-display  - display related
;;   x-error    - X Errors related
;;   x-event    - X Event related
;;   x-tray     - X tray related
;;   x-misc     - Misc stuff
;;   x-record   - RECORD extension
(defmacro X:Dpy-log-buffer (xdpy)
  `(get ,xdpy :log-buffer))
(defsetf X:Dpy-log-buffer (xdpy) (log-buf)
  `(put ,xdpy :log-buffer ,log-buf))

(defmacro X-Dpy-set-log-routines (xdpy routines)
  `(put ,xdpy :log-routines ,routines))

(defmacro X:Dpy-has-log-routine-p (xdpy routine)
  "Return non-nil if XDPY has log ROUTINE."
  `(memq ,routine (get ,xdpy :log-routines)))

(defun X-Dpy-log (xdpy routine &rest args)
  "To the XDPY's log buffer put a ROUTINE's message.
If XDPY is nil, then put into current buffer.  Log additional ARGS as well."
  (when (and (X:Dpy-log-buffer xdpy)
             (X:Dpy-has-log-routine-p xdpy routine)
	     (bufferp (get-buffer-create (X:Dpy-log-buffer xdpy))))
    (with-current-buffer (get-buffer-create (X:Dpy-log-buffer xdpy))
      (save-excursion
        (goto-char (point-min))
        (insert (format "%d %S: " (nth 1 (current-time)) routine))
        (insert (apply 'format (mapcar 'eval args)))
        (insert "\n")))))

(defun X-Dpy-log-verbatim (xdpy arg)
  "To XDPY's log buffer output ARG without formatting."
  (when (bufferp (X:Dpy-log-buffer xdpy))
    (with-current-buffer (X:Dpy-log-buffer xdpy)
      (goto-char (point-min))
      (insert "[" arg "]" "\n"))))

(defmacro X-Dpy-put-property (xdpy prop val)
  `(put ,xdpy ,prop ,val))
(defmacro X-Dpy-get-property (xdpy prop)
  `(get ,xdpy ,prop))
(defmacro X-Dpy-rem-property (xdpy prop)
  `(remprop xdpy ,prop))

(defmacro X-Dpy-event-handlers (xdpy)
  `(get ,xdpy :event-handlers))
(defsetf X-Dpy-event-handlers (xdpy) (evhs)
  `(put ,xdpy :event-handlers ,evhs))

(defmacro X-Dpy-error-hooks (xdpy)
  `(get ,xdpy :error-hooks))
(defsetf X-Dpy-error-hooks (xdpy) (hooks)
  `(put ,xdpy :error-hooks ,hooks))

(defsubst X-Dpy-EventHandler-add (dpy handler &optional priority evtypes-list)
  "To DPY's event handlers list add HANDLER.
PRIORITY specifies handler's priority.
EVTYPES-LIST specifies list of event types to handle."
  (setf (X-Dpy-event-handlers dpy)
	(X-EventHandler-add (X-Dpy-event-handlers dpy)
                            handler priority evtypes-list)))

(defsubst X-Dpy-EventHandler-isset (dpy handler &optional priority evtypes-list)
  "Return non-nil if on DPY event HANDLER is set."
  (X-EventHandler-isset
   (X-Dpy-event-handlers dpy) handler priority evtypes-list))

(defsubst X-Dpy-EventHandler-rem (dpy handler &optional priority evtypes-list)
  "From DPY's event handlers list, remove HANDLER."
  (setf (X-Dpy-event-handlers dpy)
	(X-EventHandler-rem
         (X-Dpy-event-handlers dpy) handler priority evtypes-list)))

(defsubst X-Dpy-EventHandler-enable (dpy handler
                                         &optional priority evtypes-list)
  "In DPY's list of event handlers activate HANDLER."
  (X-EventHandler-enable
   (X-Dpy-event-handlers dpy) handler priority evtypes-list))

(defsubst X-Dpy-EventHandler-disable (dpy handler
                                          &optional priority evtypes-list)
  "In DPY's list of event handlers disable HANDLER."
  (X-EventHandler-disable
   (X-Dpy-event-handlers dpy) handler priority evtypes-list))

(defsubst X-Dpy-EventHandler-runall (dpy xev)
  "Run all DPY's event handlers on XEV.
Signal `X-Events-stop' to stop events processing."
  (X-EventHandler-runall (X-Dpy-event-handlers dpy) xev))

(defun X-Dpy-p (xdpy &optional sig)
  "Return non-nil if XDPY is actually X display.
SIG is not used."
  (and (ffi-object-p xdpy)
       (equal (ffi-object-type xdpy) '(pointer Display))))

(defun X-Dpy-windows (xdpy)
  (get xdpy :windows))
(defsetf X-Dpy-windows (xdpy) (windows)
  `(put ,xdpy :windows ,windows))

(defmacro X-Dpy-evq (xdpy)
  `(get ,xdpy :events-queue))
(defsetf X-Dpy-evq (xdpy) (events)
  `(put ,xdpy :events-queue ,events))

(defconst X-default-timeout 60)

(defun X-invalidate-cl-struct (cl-x)
  "Invalidate CL-X, after `X-invalidate-cl-struct' it won't be struct anymore.
NOTE: works only if CL-X is vector."
  (if (vectorp cl-x)
      (let ((i (length cl-x)))
        (while (>= (setq i (1- i)) 0)
          (aset cl-x i nil))
        t)))

(defun X-Win-invalidate (xdpy win)
  "For XDPY display remove WIN from its window list and invalidate WIN."
  (add-timeout X-default-timeout
               #'(lambda (xdpy-win)
                   (setf (X-Dpy-windows (car xdpy-win))
                         (delq (cdr xdpy-win) (X-Dpy-windows (car xdpy-win))))
                   (X-invalidate-cl-struct (cdr xdpy-win)))
               (cons xdpy win)))

(defun X-Win-find (xdpy wid)
  "On XDPY find window by WID."
  (let ((wl (X-Dpy-windows xdpy)))
    (while (and wl (not (= (X-Win-id (car wl)) wid)))
      (setq wl (cdr wl)))
    (car wl)))

(defun X-Win-find-or-make (xdpy wid)
  "On XDPY find X-Win with id WID, or make new one if not found."
  (or (X-Win-find xdpy wid)
      (let ((xwin (make-X-Win :dpy xdpy :id wid)))
        (X-Dpy-log xdpy 'x-event "XDPY Adding new window: %S" 'wid)
        (push xwin (X-Dpy-windows xdpy))
        xwin)))

;;}}}
;;{{{ `-- Dispatching incoming events

(defun X-Event-dpy (xev)
  (get xev :display))
(defsetf X-Event-dpy (xev) (dpy)
  `(put ,xev :display ,dpy))

(defun X:Dpy-event-dispatch (xev)
  "Dispatch event XEV."
  (let* ((dpy (X-Event-dpy xev))
         (wev (X-Event-win-event xev)))
    (X-Dpy-log dpy 'x-event "XEvent %S/%S: win=0x%x/0x%x"
               '(XAnyEvent->type xev) '(X-Event-name xev) '(X-Win-id wev)
               '(X-Win-id (X-Event-win xev)))

    ;; First run display handlers
    (when (X-Dpy-event-handlers dpy)
      (X-Dpy-EventHandler-runall dpy xev))

    ;; Then run WIN specific handlers
    (when (X-Win-event-handlers wev)
      ;; WIN has its own event handlers
      (X-Win-EventHandler-runall wev xev))))

(defalias 'X-Dpy-event-dispatch 'X:Dpy-event-dispatch)

(defun X:Dpy-filter (proc out)
  (let ((dpy (X:Dpy-find proc)))
    (when dpy
      (X:Flush dpy)
      (while (X:Pending dpy)
        (let ((xev (make-ffi-object 'XEvent)))
          (X:NextEvent dpy (ffi-address-of xev))
          ;; We explicitely set display since `XEventAny->display'
          ;; does not keeps properties.
          (setf (X-Event-dpy xev) dpy)
          (enqueue-eval-event 'X:Dpy-event-dispatch xev)))))
  0)

;    (let ((dpy (X:Dpy-find proc))
;          evd wev)
;      (when dpy
;        ;; Enqueue X events
;        (while (>= (length out) 32)
;          (setq evd (substring out 0 32)
;                out (substring out 32)
;                wev (make-ffi-object '(pointer void) 32))
;          (ffi-store wev 0 'c-string evd)
;          (X:Enq dpy wev))
    
;        ;; Shedule dispatching of all pending X events
;        (X:Flush dpy)
;        (while (> (Display->qlen dpy) 0)
;          (let ((xev (make-ffi-object 'XEvent)))
;            (X:NextEvent dpy (ffi-address-of xev))
;            ;; We explicitely set display since `XEventAny->display'
;            ;; does not keeps properties.
;            (setf (X-Event-dpy xev) dpy)
;            (enqueue-eval-event 'X:Dpy-event-dispatch xev))))))

;;}}}

(defconst X:Event-names-alist
  (list
   (cons X-KeyPress :X-KeyPress)
   (cons X-KeyRelease :X-KeyRelease)
   (cons X-ButtonPress :X-ButtonPress)
   (cons X-ButtonRelease :X-ButtonRelease)
   (cons X-MotionNotify :X-MotionNotify)
   (cons X-EnterNotify :X-EnterNotify)
   (cons X-LeaveNotify :X-LeaveNotify)
   (cons X-FocusIn :X-FocusIn)
   (cons X-FocusOut :X-FocusOut)
   (cons X-KeymapNotify :X-KeymapNotify)
   (cons X-Expose :X-Expose)
   (cons X-GraphicsExpose :X-GraphicsExpose)
   (cons X-NoExpose :X-NoExpose)
   (cons X-VisibilityNotify :X-VisibilityNotify)
   (cons X-CreateNotify :X-CreateNotify)
   (cons X-DestroyNotify :X-DestroyNotify)
   (cons X-UnmapNotify :X-UnmapNotify)
   (cons X-MapNotify :X-MapNotify)
   (cons X-MapRequest :X-MapRequest)
   (cons X-ReparentNotify :X-ReparentNotify)
   (cons X-ConfigureRequest :X-ConfigureRequest)
   (cons X-ConfigureNotify :X-ConfigureNotify)
   (cons X-GravityNotify :X-GravityNotify)
   (cons X-ResizeRequest :X-ResizeRequest)
   (cons X-CirculateNotify :X-CirculateNotify)
   (cons X-CirculateRequest :X-CirculateRequest)
   (cons X-PropertyNotify :X-PropertyNotify)
   (cons X-SelectionClear :X-SelectionClear)
   (cons X-SelectionRequest :X-SelectionRequest)
   (cons X-SelectionNotify :X-SelectionNotify)
   (cons X-ColormapNotify :X-ColormapNotify)
   (cons X-ClientMessage :X-ClientMessage)
   (cons X-MappingNotify :X-MappingNotify))
  "Alist of event names.")

(defun X-Event-name (xev)
  "Convert XEV type to symbolic name, return keyword."
  (or (cdr (assq (XAnyEvent->type xev) X:Event-names-alist))
      :X-Unknown))

(defun X-Event-put-property (xev prop val)
  "Put property PROP with value VAL in XEV's properties list."
  (put xev prop val))

(defun X-Event-get-property (xev prop)
  "Get property PROP from XEV's properties list."
  (get xev prop))

(defun X-Event-rem-property (xev prop)
  "Remove property PROP from XEV's properties list."
  (remprop xev prop))

(defmacro X-Event-CASE (xev &rest body)
  "Run event case. BODY in form (EVTYPE FORMS) (EVTYPE FORMS) ..
EVTYPE is one of :X-KeyPress, :X-KeyRelease etc."
  `(case (X-Event-name ,xev)
     ,@body))
(put 'X-Event-CASE 'lisp-indent-function 1)

(defun X-Event-p (xev)
  "Return non-nil if XEV is X-Event."
  (and (ffi-object-p xev)
       (member (ffi-object-type xev)
               '(XEvent (pointer XEvent)))))

(defalias 'X-Event-type 'XAnyEvent->type)
(defalias 'X-Event-seq 'XAnyEvent->serial)
(defun X-Event-synth-p (xev)
  (not (zerop (XAnyEvent->send_event xev))))

(defmacro defalias-xev-win (o n)
  `(defun ,o (xev)
     (X-Win-find-or-make (X-Event-dpy xev) (,n xev))))

(defun X-Event-win (xev)
  (let ((evt (X-Event-type xev)))
    (cond ((= evt X-DestroyNotify) (X-Event-xdestroywindow-window xev))
          ((= evt X-UnmapNotify) (X-Event-xunmap-window xev))
          ((= evt X-MapNotify) (X-Event-xmap-window xev))
          ((= evt X-MapRequest) (X-Event-xmaprequest-window xev))
          ((= evt X-ReparentNotify) (X-Event-xreparent-window xev))
          ((= evt X-ConfigureNotify) (X-Event-xconfigure-window xev))
          ((= evt X-ConfigureRequest) (X-Event-xconfigurerequest-window xev))
          ((= evt X-GravityNotify) (X-Event-xgravity-window xev))
          ((= evt X-CirculateNotify) (X-Event-xcirculate-window xev))
          (t (X-Win-find-or-make (X-Event-dpy xev) (XAnyEvent->window xev))))))

(defun X-Event-win-event (xev)
  (let ((evt (X-Event-type xev)))
    (cond ((= evt X-DestroyNotify) (X-Event-xdestroywindow-event xev))
          ((= evt X-UnmapNotify) (X-Event-xunmap-event xev))
          ((= evt X-MapNotify) (X-Event-xmap-event xev))
          ((= evt X-MapRequest) (X-Event-xmaprequest-parent xev))
          ((= evt X-ReparentNotify) (X-Event-xreparent-event xev))
          ((= evt X-ConfigureNotify) (X-Event-xconfigure-event xev))
          ((= evt X-ConfigureRequest) (X-Event-xconfigurerequest-parent xev))
          ((= evt X-GravityNotify) (X-Event-xgravity-event xev))
          ((= evt X-CirculateNotify) (X-Event-xcirculate-event xev))
          (t (X-Event-win xev)))))

(defalias 'X-Event-xkey-event-x 'XKeyEvent->x)
(defalias 'X-Event-xkey-event-y 'XKeyEvent->y)
(defalias 'X-Event-xkey-root-x 'XKeyEvent->x_root)
(defalias 'X-Event-xkey-root-y 'XKeyEvent->y_root)
(defalias 'X-Event-xkey-state 'XKeyEvent->state)
(defalias 'X-Event-xkey-keycode 'XKeyEvent->keycode)
(defalias 'X-Event-xkey-child 'XKeyEvent->subwindow)
(defalias-xev-win X-Event-xkey-event XKeyEvent->window)
(defalias-xev-win X-Event-xkey-root XKeyEvent->root)
(defalias 'X-Event-xkey-same-screen 'XKeyEvent->same_screen)
(defalias 'X-Event-xkey-time 'XKeyEvent->time)

(defalias 'X-Event-xbutton-button 'XButtonEvent->button)
(defalias 'X-Event-xbutton-time 'XButtonEvent->time)
(defalias-xev-win X-Event-xbutton-root XButtonEvent->root)
(defalias-xev-win X-Event-xbutton-event XButtonEvent->window)
(defalias-xev-win X-Event-xbutton-child XButtonEvent->subwindow)
(defalias 'X-Event-xbutton-root-x 'XButtonEvent->x_root)
(defalias 'X-Event-xbutton-root-y 'XButtonEvent->y_root)
(defalias 'X-Event-xbutton-event-x 'XButtonEvent->x)
(defalias 'X-Event-xbutton-event-y 'XButtonEvent->y)
(defalias 'X-Event-xbutton-state 'XButtonEvent->state)
(defalias 'X-Event-xbutton-same-screen 'XButtonEvent->same_screen)

(defalias 'X-Event-xmotion-time 'XMotionEvent->time)
(defalias-xev-win X-Event-xmotion-root XMotionEvent->root)
(defalias-xev-win X-Event-xmotion-event XMotionEvent->window)
(defalias-xev-win X-Event-xmotion-child XMotionEvent->subwindow)
(defalias 'X-Event-xmotion-root-x 'XMotionEvent->x_root)
(defalias 'X-Event-xmotion-root-y 'XMotionEvent->y_root)
(defalias 'X-Event-xmotion-event-x 'XMotionEvent->x)
(defalias 'X-Event-xmotion-event-y 'XMotionEvent->y)
(defalias 'X-Event-xmotion-state 'XMotionEvent->state)
(defalias 'X-Event-xmotion-same-screen 'XMotionEvent->same_screen)

(defalias 'X-Event-xcrossing-time 'XCrossingEvent->time)
(defalias-xev-win X-Event-xcrossing-root XCrossingEvent->root)
(defalias-xev-win X-Event-xcrossing-event XCrossingEvent->window)
(defalias-xev-win X-Event-xcrossing-child XCrossingEvent->subwindow)
(defalias 'X-Event-xcrossing-root-x 'XCrossingEvent->x_root)
(defalias 'X-Event-xcrossing-root-y 'XCrossingEvent->y_root)
(defalias 'X-Event-xcrossing-event-x 'XCrossingEvent->x)
(defalias 'X-Event-xcrossing-event-y 'XCrossingEvent->y)
(defalias 'X-Event-xcrossing-state 'XCrossingEvent->state)
(defalias 'X-Event-xcrossing-mode 'XCrossingEvent->mode)
(defalias 'X-Event-xcrossing-same-screen 'XCrossingEvent->same_screen)

(defalias-xev-win X-Event-xfocus-event XFocusChangeEvent->window)
(defalias 'X-Event-xfocus-mode 'XFocusChangeEvent->mode)

(defalias-xev-win X-Event-xexpose-window XExposeEvent->window)
(defalias 'X-Event-xexpose-x 'XExposeEvent->x)
(defalias 'X-Event-xexpose-y 'XExposeEvent->y)
(defalias 'X-Event-xexpose-width 'XExposeEvent->width)
(defalias 'X-Event-xexpose-height 'XExposeEvent->height)
(defalias 'X-Event-xexpose-count 'XExposeEvent->count)

(defalias 'X-Event-xgraphicsexpose-drawable 'XGraphicsExposeEvent->drawable)
(defalias 'X-Event-xgraphicsexpose-x 'XGraphicsExposeEvent->x)
(defalias 'X-Event-xgraphicsexpose-y 'XGraphicsExposeEvent->y)
(defalias 'X-Event-xgraphicsexpose-width 'XGraphicsExposeEvent->width)
(defalias 'X-Event-xgraphicsexpose-height 'XGraphicsExposeEvent->height)
(defalias 'X-Event-xgraphicsexpose-minor-event 'XGraphicsExposeEvent->minor_code)
(defalias 'X-Event-xgraphicsexpose-count 'XGraphicsExposeEvent->count)
(defalias 'X-Event-xgraphicsexpose-major-event 'XGraphicsExposeEvent->major_code)

(defalias 'X-Event-xnoexpose-drawable 'XNoExposeEvent->drawable)
(defalias 'X-Event-xnoexpose-minor-event 'XNoExposeEvent->minor_code)
(defalias 'X-Event-xnoexpose-major-event 'XNoExposeEvent->major_code)

(defalias-xev-win X-Event-xvisibility-window XVisibilityEvent->window)
(defalias 'X-Event-xvisibility-state 'XVisibilityEvent->state)

(defalias-xev-win X-Event-xcreatewindow-parent XCreateWindowEvent->parent)
(defalias-xev-win X-Event-xcreatewindow-window XCreateWindowEvent->window)
(defalias 'X-Event-xcreatewindow-x 'XCreateWindowEvent->x)
(defalias 'X-Event-xcreatewindow-y 'XCreateWindowEvent->y)
(defalias 'X-Event-xcreatewindow-width 'XCreateWindowEvent->width)
(defalias 'X-Event-xcreatewindow-height 'XCreateWindowEvent->height)
(defalias 'X-Event-xcreatewindow-border-width 'XCreateWindowEvent->border_width)
(defalias 'X-Event-xcreatewindow-override 'XCreateWindowEvent->override_redirect)

(defalias-xev-win X-Event-xdestroywindow-event XDestroyWindowEvent->event)
(defalias-xev-win X-Event-xdestroywindow-window XDestroyWindowEvent->window)

(defalias-xev-win X-Event-xunmap-event XUnmapEvent->event)
(defalias-xev-win X-Event-xunmap-window XUnmapEvent->window)
(defun X-Event-xunmap-from-configure (xev)
  (not (zerop (XUnmapEvent->from_configure xev))))

(defalias-xev-win X-Event-xmap-event XMapEvent->event)
(defalias-xev-win X-Event-xmap-window XMapEvent->window)
(defalias 'X-Event-xmap-override 'XMapEvent->override_redirect)

(defalias-xev-win X-Event-xmaprequest-parent XMapRequestEvent->parent)
(defalias-xev-win X-Event-xmaprequest-window XMapRequestEvent->window)

(defalias-xev-win X-Event-xreparent-event XReparentEvent->event)
(defalias-xev-win X-Event-xreparent-window XReparentEvent->window)
(defalias-xev-win X-Event-xreparent-parent XReparentEvent->parent)
(defalias 'X-Event-xreparent-x 'XReparentEvent->x)
(defalias 'X-Event-xreparent-y 'XReparentEvent->y)
(defalias 'X-Event-xreparent-override 'XReparentEvent->override_redirect)

(defalias-xev-win X-Event-xconfigure-event XConfigureEvent->event)
(defalias-xev-win X-Event-xconfigure-window XConfigureEvent->window)
(defalias-xev-win X-Event-xconfigure-above-sibling XConfigureEvent->above)
(defalias 'X-Event-xconfigure-x 'XConfigureEvent->x)
(defalias 'X-Event-xconfigure-y 'XConfigureEvent->y)
(defalias 'X-Event-xconfigure-width 'XConfigureEvent->width)
(defalias 'X-Event-xconfigure-height 'XConfigureEvent->height)
(defalias 'X-Event-xconfigure-border-width 'XConfigureEvent->border_width)
(defalias 'X-Event-xconfigure-override-redirect 'XConfigureEvent->override_redirect)

(defalias 'X-Event-xconfigurerequest-stackmode 'XConfigureRequestEvent->detail)
(defalias-xev-win X-Event-xconfigurerequest-parent XConfigureRequestEvent->parent)
(defalias-xev-win X-Event-xconfigurerequest-window XConfigureRequestEvent->window)
(defalias-xev-win X-Event-xconfigurerequest-sibling XConfigureRequestEvent->above)
(defalias 'X-Event-xconfigurerequest-x 'XConfigureRequestEvent->x)
(defalias 'X-Event-xconfigurerequest-y 'XConfigureRequestEvent->y)
(defalias 'X-Event-xconfigurerequest-width 'XConfigureRequestEvent->width)
(defalias 'X-Event-xconfigurerequest-height 'XConfigureRequestEvent->height)
(defalias 'X-Event-xconfigurerequest-border-width 'XConfigureRequestEvent->border_width)
(defalias 'X-Event-xconfigurerequest-value-mask 'XConfigureRequestEvent->value_mask)

(defalias-xev-win X-Event-xgravity-event XGravityEvent->event)
(defalias-xev-win X-Event-xgravity-window XGravityEvent->window)
(defalias 'X-Event-xgravity-x 'XGravityEvent->x)
(defalias 'X-Event-xgravity-y 'XGravityEvent->y)

(defalias-xev-win X-Event-xresizerequest-window XResizeRequestEvent->window)
(defalias 'X-Event-xresizerequest-width 'XResizeRequestEvent->width)
(defalias 'X-Event-xresizerequest-height 'XResizeRequestEvent->height)

(defalias-xev-win X-Event-xcirculate-event XCirculateEvent->event)
(defalias-xev-win X-Event-xcirculate-window XCirculateRequestEvent->window)
(defalias-xev-win X-Event-xcirculate-parent XCirculateRequestEvent->parent)
(defalias 'X-Event-xcirculate-place 'XCirculateRequestEvent->place)

(defalias-xev-win X-Event-xproperty-window XPropertyEvent->window)
(defalias 'X-Event-xproperty-atom 'XPropertyEvent->atom)
(defalias 'X-Event-xproperty-time 'XPropertyEvent->time)
(defalias 'X-Event-xproperty-state 'XPropertyEvent->state)

(defalias 'X-Event-xselectionclear-time 'XSelectionClearEvent->time)
(defalias-xev-win X-Event-xselectionclear-window XSelectionClearEvent->window)
(defalias 'X-Event-xselectionclear-atom 'XSelectionClearEvent->atom)

(defalias 'X-Event-xselectionrequest-time 'XSelectionRequestEvent->time)
(defalias-xev-win X-Event-xselectionrequest-owner XSelectionRequestEvent->owner)
(defalias-xev-win X-Event-xselectionrequest-requestor XSelectionRequestEvent->requestor)
(defalias-xev-win X-Event-xselectionrequest-selection XSelectionRequestEvent->selection)
(defalias 'X-Event-xselectionrequest-target 'XSelectionRequestEvent->target)
(defalias 'X-Event-xselectionrequest-property 'XSelectionRequestEvent->property)

(defalias 'X-Event-xselection-time 'XSelectionEvent->time)
(defalias-xev-win X-Event-xselection-requestor XSelectionEvent->requestor)
(defalias-xev-win X-Event-xselection-selection XSelectionEvent->selection)
(defalias 'X-Event-xselection-target 'XSelectionEvent->target)
(defalias 'X-Event-xselection-property 'XSelectionEvent->property)

(defalias-xev-win X-Event-xcolormap-window XColormapEvent->window)
(defalias 'X-Event-xcolormap-colormap 'XColormapEvent->colormap)
(defalias 'X-Event-xcolormap-new 'XColormapEvent->new)
(defalias 'X-Event-xcolormap-state 'XColormapEvent->state)

(defalias-xev-win X-Event-xclient-window XClientMessageEvent->window)
(defalias 'X-Event-xclient-atom 'XClientMessageEvent->message_type)
(defun X-Event-xclient-msg (xev)
  "Make it compatible with `X-Event-xclient-msg' from xlib."
  (mapcar #'(lambda (i)
              (list (ffi-fetch
                     xev (+ (ffi-slot-offset 'XClientMessageEvent 'data)
                            (* i 4)) 'long)))
          '(0 1 2 3 4)))

(defalias 'X-Event-xmapping-request 'XMappingEvent->request)
(defalias 'X-Event-xmapping-first-keycode 'XMappingEvent->first_keycode)
(defalias 'X-Event-xmapping-count 'XMappingEvent->count)

(defalias 'X-Event-xerror-code 'XErrorEvent->error_code)
(defalias 'X-Event-xerror-resourceid 'XErrorEvent->resourceid)
(defalias 'X-Event-xerror-min-op 'XErrorEvent->minor_code)
(defalias 'X-Event-xerror-maj-op 'XErrorEvent->request_code)

;; Aliases
(defalias 'X-Dpy-name 'Display->display_name)
(defalias 'X-Dpy-screens 'Display->screens)
(defalias 'X-Dpy-min-keycode 'Display->min_keycode)
(defalias 'X-Dpy-max-keycode 'Display->max_keycode)
(defalias 'X-Dpy-resource-base 'Display->private3)
(defalias 'X-Dpy-resource-id 'Display->private5)
(defalias 'X-Dpy-rseq-id 'Display->request)
(defalias 'X-Screen-root 'Screen->root)

(defmacro defun-x11 (funame args &rest body)
  "Macro to define x11 functions with auto-flushing.
Makes sense if function does not receives from X server."
  `(defun ,funame ,args
     (prog1
         (progn ,@body)
       (enqueue-eval-event #'X:Flush ,(car args)))))

(defun XOpenDisplay (name &rest props)
  (let ((xdpy (X:OpenDisplay name)))
    (when (ffi-null-p xdpy)
      (error "Can't open display" name))
    (apply #'X:Dpy-setup xdpy props)
    xdpy))

(defun XDefaultRootWindow (xdpy &optional scrnum)
  (X-Win-find-or-make xdpy (X:RootWindow xdpy (or scrnum 0))))
(defun XWhitePixel (xdpy &optional scrnum)
  (X:WhitePixel xdpy (or scrnum 0)))
(defun XBlackPixel (xdpy &optional scrnum)
  (X:BlackPixel xdpy (or scrnum 0)))
(defun XDefaultColormap (xdpy &optional scrnum)
  (X:DefaultColormap xdpy (or scrnum 0)))
(defun XDefaultVisual (xdpy &optional scrnum)
  (X:DefaultVisual xdpy (or scrnum 0)))
(defun XDefaultGC (xdpy &optional scrnum)
  (Screen->default_gc (X:ScreenOfDisplay xdpy (or scrnum 0))))
(defun XDefaultDepth (xdpy &optional scrnum)
  (X:DefaultDepth xdpy (or scrnum 0)))

(defun-x11 XCloseDisplay (xdpy)
  (X:CloseDisplay xdpy))

(defun-x11 XBell (xdpy percent)
  "Ring the bell on XDPY at PERCENT volume."
  (X:Bell xdpy percent))

(defun-x11 XGrabServer (dpy)
  (X:GrabServer dpy))

(defun-x11 XUngrabServer (dpy)
  (X:UngrabServer dpy))

(defalias 'XFlush 'X:Flush)

(defun XSync (dpy &optional discard)
  (X:Sync dpy (if discard 1 0)))
  
(defun X-Attr-override-redirect (attr)
  (not (zerop (XWindowAttributes->override_redirect attr))))
(defalias 'X-Attr-mapstate 'XWindowAttributes->map_state)
(defalias 'X-Attr-event-mask 'XWindowAttributes->your_event_mask)

(defun X:make-X-Attr (&rest attr)
  (let ((vmask 0)
        (foa (make-ffi-object 'XSetWindowAttributes)))
    (while attr
      (case (car attr)
        (:background-pixmap
         (setf (XSetWindowAttributes->background_pixmap foa) (cadr attr)
               vmask (Xmask-or vmask X-CWBackPixmap)))
        (:background-pixel
         (setf (XSetWindowAttributes->background_pixel foa) (cadr attr)
               vmask (Xmask-or vmask X-CWBackPixel)))
        (:border-pixmap
         (setf (XSetWindowAttributes->border_pixmap foa) (cadr attr)
               vmask (Xmask-or vmask X-CWBorderPixmap)))
        (:border-pixel
         (setf (XSetWindowAttributes->border_pixel foa) (cadr attr)
               vmask (Xmask-or vmask X-CWBorderPixel)))
        (:bit-gravity
         (setf (XSetWindowAttributes->bit_gravity foa) (cadr attr)
               vmask (Xmask-or vmask X-CWBitGravity)))
        (:win-gravity
         (setf (XSetWindowAttributes->win_gravity foa) (cadr attr)
               vmask (Xmask-or vmask X-CWWinGravity)))
        (:backing-store
         (setf (XSetWindowAttributes->backing_store foa) (if (cadr attr) 1 0)
               vmask (Xmask-or vmask X-CWBackingStore)))
        (:backing-planes
         (setf (XSetWindowAttributes->backing_planes foa) (cadr attr)
               vmask (Xmask-or vmask X-CWBackingPlanes)))
        (:backing-pixel
         (setf (XSetWindowAttributes->backing_pixel foa) (cadr attr)
               vmask (Xmask-or vmask X-CWBackingPixel)))
        (:override-redirect
         (setf (XSetWindowAttributes->override_redirect foa) (if (cadr attr) 1 0)
               vmask (Xmask-or vmask X-CWOverrideRedirect)))
        (:save-under
         (setf (XSetWindowAttributes->save_under foa) (cadr attr)
               vmask (Xmask-or vmask X-CWSaveUnder)))
        (:event-mask
         (when (cadr attr)
           (setf (XSetWindowAttributes->event_mask foa) (cadr attr)
                 vmask (Xmask-or vmask X-CWEventMask))))
        (:do-not-propagate-mask
         (setf (XSetWindowAttributes->do_not_propagate_mask foa) (cadr attr)
               vmask (Xmask-or vmask X-CWDontPropagate)))
        (:colormap
         (setf (XSetWindowAttributes->colormap foa) (cadr attr)
               vmask (Xmask-or vmask X-CWColormap)))
        (:cursor
         (when (cadr attr)
           (setf (XSetWindowAttributes->cursor foa) (cadr attr)
                 vmask (Xmask-or vmask X-CWCursor)))))
      (setq attr (cddr attr)))
    (values foa vmask)))

(defun-x11 XCreateWindow (xdpy &optional parent x y width height border-width
                               depth class visual &rest attrs)
  (multiple-value-bind (foa vmask)
      (apply #'X:make-X-Attr attrs)
    (unless parent
      (setq parent (XDefaultRootWindow xdpy)))
    (unless depth
      (setq depth X-CopyFromParent))
    (unless class
      (setq class X-CopyFromParent))
    (when (or (not visual)
              (= visual X-CopyFromParent))
      (setq visual (ffi-null-pointer)))
    (X-Win-find-or-make
     xdpy (X:CreateWindow xdpy (X-Win-id parent) x y width height border-width
                          depth class visual vmask (ffi-address-of foa)))))

(defun-x11 XChangeWindowAttributes (xdpy win &rest attrs)
  (multiple-value-bind (foa vmask)
      (apply #'X:make-X-Attr attrs)
    (X:ChangeWindowAttributes xdpy (X-Win-id win) vmask (ffi-address-of foa))))

(defun-x11 XSetWindowBackground (dpy w p)
  (X:SetWindowBackground dpy (X-Win-id w) p))
(defun-x11 XSetWindowBackgroundPixmap (dpy w p)
  (X:SetWindowBackgroundPixmap dpy (X-Win-id w) (X-Pixmap-id p)))
(defun-x11 XSetWindowBorder (dpy w p)
  (X:SetWindowBorder dpy (X-Win-id w) p))
(defun-x11 XSetWindowBorderPixmap (dpy w p)
  (X:SetWindowBorderPixmap dpy (X-Win-id w) (X-Pixmap-id p)))
(defun-x11 XSetWindowColormap (dpy w c)
  (X:SetWindowColormap dpy (X-Win-id w) c))
(defun XSetWindowCursor (dpy w c)
  (XChangeWindowAttributes dpy w :cursor c))

(defun XGetWindowAttributes (dpy w)
  (let ((at (make-ffi-object 'XWindowAttributes)))
    (X:GetWindowAttributes dpy (X-Win-id w) (ffi-address-of at))
    at))

(defun XGetGeometry (dpy d)
  (let ((r-root (make-ffi-object 'Window))
        (r-x (make-ffi-object 'int))
        (r-y (make-ffi-object 'int))
        (r-w (make-ffi-object 'unsigned-int))
        (r-h (make-ffi-object 'unsigned-int))
        (r-bw (make-ffi-object 'unsigned-int))
        (r-depth (make-ffi-object 'unsigned-int)))
    (X:GetGeometry dpy (X-Drawable-id d) (ffi-address-of r-root)
                   (ffi-address-of r-x) (ffi-address-of r-y)
                   (ffi-address-of r-w) (ffi-address-of r-h)
                   (ffi-address-of r-bw) (ffi-address-of r-depth))
    (if (X-Win-p d)
        (X-Win-put-prop d 'xdepth (ffi-get r-depth))
      (X-Pixmap-put-prop d 'xdepth (ffi-get r-depth)))
    (make-X-Geom :x (ffi-get r-x) :y (ffi-get r-y) :width (ffi-get r-w)
                 :height (ffi-get r-h) :border-width (ffi-get r-bw))))

(defun XGetDepth (xdpy d)
  "On display xdpy return drawable's D depth."
  (or (if (X-Win-p d)
          (X-Win-get-prop d 'xdepth)
        (X-Pixmap-get-prop d 'xdepth))
      (progn
        (XGetGeometry xdpy d)
        (if (X-Win-p d)
            (X-Win-get-prop d 'xdepth)
          (X-Pixmap-get-prop d 'xdepth)))))

(defun X:make-X-Config (&rest config)
  (let ((foc (make-ffi-object 'XWindowChanges))
        (vmask 0))
    (while config
      (when (cadr config)
        (case (car config)
          (:x
           (setf (XWindowChanges->x foc) (cadr config)
                 vmask (Xmask-or vmask X-CWX)))
          (:y
           (setf (XWindowChanges->y foc) (cadr config)
                 vmask (Xmask-or vmask X-CWY)))
          (:width
           (setf (XWindowChanges->width foc) (cadr config)
                 vmask (Xmask-or vmask X-CWWidth)))
          (:height
           (setf (XWindowChanges->height foc) (cadr config)
                 vmask (Xmask-or vmask X-CWHeight)))
          (:border-width
           (setf (XWindowChanges->border_width foc) (cadr config)
                 vmask (Xmask-or vmask X-CWBorderWidth)))
          (:sibling
           (setf (XWindowChanges->sibling foc) (X-Win-id (cadr config))
                 vmask (Xmask-or vmask X-CWSibling)))
          (:stackmode
           (setf (XWindowChanges->stack_mode foc) (cadr config)
                 vmask (Xmask-or vmask X-CWStackMode)))))
      (setq config (cddr config)))
    (values foc vmask)))

(defun-x11 XConfigureWindow (dpy w &rest config)
  (multiple-value-bind (foc vmask)
      (apply #'X:make-X-Config config)
    (X:ConfigureWindow dpy (X-Win-id w) vmask (ffi-address-of foc))))

(defun-x11 XMoveWindow (dpy w x y)
  (X:MoveWindow dpy (X-Win-id w) x y))
(defun-x11 XResizeWindow (dpy w width height)
  (X:ResizeWindow dpy (X-Win-id w) width height))
(defun-x11 XMoveResizeWindow (dpy w x y width height)
  (X:MoveResizeWindow dpy (X-Win-id w) x y width height))
(defun-x11 XSetWindowBorderWidth (dpy w width)
  (X:SetWindowBorderWidth dpy (X-Win-id w) width))

(defun-x11 XMapWindow (dpy w)
  (X:MapWindow dpy (X-Win-id w)))
(defun-x11 XUnmapWindow (dpy w)
  (X:UnmapWindow dpy (X-Win-id w)))
(defun-x11 XDestroyWindow (dpy w)
  (X:DestroyWindow dpy (X-Win-id w)))
(defun-x11 XDestroySubwindows (dpy w)
  (X:DestroySubwindows dpy (X-Win-id w)))
(defun XQueryTree (dpy w)
  (let ((rw (make-ffi-object 'Window))
        (pw (make-ffi-object 'Window))
        (cl (make-ffi-object '(pointer Window)))
        (cc (make-ffi-object 'unsigned-int))
        cil-w)
    (X:QueryTree dpy (X-Win-id w) (ffi-address-of rw) (ffi-address-of pw)
                 (ffi-address-of cl) (ffi-address-of cc))
    (setq rw (ffi-get rw)
          pw (ffi-get pw)
          cc (ffi-get cc))
    (dotimes (idx cc)
      (push (X-Win-find-or-make dpy (ffi-aref cl idx)) cil-w))
    (X:Free cl)
    (nconc '(t 0 rw pw) (nreverse cil-w))))

(defun X-Atom-p (atom)
  (numberp atom))

(defalias 'X-Atom-id 'identity)
(defalias 'X-Atom-find 'identity)
(defalias 'X-Atom-equal '=)

(defun-x11 XInternAtom (xdpy name &optional only-if-exists)
  (X:InternAtom xdpy name (if only-if-exists 1 0)))
(defalias 'X-Atom-find-by-name 'XInternAtom)

(defun XGetAtomName (xdpy atom)
  (let ((name (X:GetAtomName xdpy atom)))
    (unless (ffi-null-p name)
      (unwind-protect
          (ffi-fetch name 0 'c-string)
        (X:Free name)))))
(defalias 'X-Atom-name 'XGetAtomName)

(defun-x11 XChangeProperty (xdpy win property type format mode data)
  (let (fod vd)
    (if (and (= format 8) (stringp data))
        (setq fod (ffi-create-fo 'c-string data))
      (ecase format
        (8 (setq fod (make-ffi-object '(pointer byte) (length data))))
        (16 (setq fod (make-ffi-object
                       '(pointer short) (* (ffi-size-of-type 'short)
                                           (length data)))))
        (32 (setq fod (make-ffi-object
                       '(pointer int) (* (ffi-size-of-type 'int)
                                         (length data))))))
      (dotimes (idx (length data))
        (setq vd (nth idx data))
        (when (X-Drawable-p vd)
          (setq vd (X-Drawable-id vd)))
        (ffi-aset fod idx vd)))
    (X:ChangeProperty xdpy (X-Win-id win) property type format mode
                      fod (length data))))

(defun-x11 XDeleteProperty (xdpy win prop)
  (X:DeleteProperty xdpy (X-Win-id win) prop))

(defun XSetPropertyString (xdpy win atom string &optional mode)
  "On display XDPY and window WIN set ATOM property to STRING."
  (XChangeProperty xdpy win atom XA-string X-format-8
                   (or mode X-PropModeReplace) string))

(defun XGetWindowProperty (xdpy win property
                                &optional offset length delete required-type)
  (unless offset (setq offset 0))
  (unless length (setq length 1024))
  (unless required-type (setq required-type XA-AnyPropertyType))
  (let ((aa-ret (make-ffi-object 'Atom))
        (af-ret (make-ffi-object 'int))
        (ni-ret (make-ffi-object 'unsigned-long))
        (ba-ret (make-ffi-object 'unsigned-long))
        (pv-ret (make-ffi-object 'c-string (1+ length))))
    (when (zerop (X:GetWindowProperty
                  xdpy (X-Win-id win) property offset length
                  (if delete 1 0) required-type (ffi-address-of aa-ret)
                  (ffi-address-of af-ret) (ffi-address-of ni-ret)
                  (ffi-address-of ba-ret) (ffi-address-of pv-ret)))
      (prog1
          (list (ffi-get aa-ret) (ffi-get ba-ret)
                (cond ((or (= (ffi-get af-ret) 8)
                           (eq required-type XA-string))
                       (if (ffi-null-p pv-ret) "" (ffi-get pv-ret)))
                      ((eq required-type XA-atom) )
                      ((eq required-type XA-window) )
                      ((eq required-type XA-rectangle) )
                      ((= (ffi-get af-ret) 0) "")
                      (t (let (ntype rt)
                           (ecase (ffi-get af-ret)
                             (16 (setq ntype 'short))
                             (32 (setq ntype 'int)))
                           (dotimes (idx (ffi-get ni-ret))
                             (push (ffi-fetch
                                    pv-ret (* (ffi-size-of-type ntype) idx)
                                    ntype) rt))
                           (nreverse rt)))))
        (X:Free pv-ret)))))

(defun XDecodeCompoundText (dpy text)
  "Decode compound TEXT, to native string.
Evil hack, invent something better."
  (let ((mtp (make-ffi-object 'XTextProperty))
        (ppp (make-ffi-object '(pointer c-string)))
        (pl (make-ffi-object 'int)))

    ;; Contstruct text property
    (setf (XTextProperty->encoding mtp)
          (XInternAtom dpy "COMPOUND_TEXT"))
    (setf (XTextProperty->format mtp) 8)
    (setf (XTextProperty->value mtp)
          (make-ffi-object '(pointer char) (1+ (length text))))
    (setf (XTextProperty->nitems mtp)
          (length text))

    (ffi-store (XTextProperty->value mtp) 0
               'c-string text)

    (when (zerop (XmbTextPropertyToTextList
                  dpy (ffi-address-of mtp)
                  (ffi-address-of ppp)
                  (ffi-address-of pl)))
      (unwind-protect
          (let ((rstr (ffi-get (ffi-get ppp) :type 'c-string)))
            (decode-coding-string rstr 'utf-8))
        (X:FreeStringList ppp)))))
  
;  (decode-coding-string text 'ctext))

;   (if (string-match "\x1b\x25\x2f\x31\\(.\\)\\(.\\)\\(.*?\\)\x02" text)
;       (let ((len (+ (* (- (char-to-int (string-to-char (match-string 1 text)))
;                           128) 128)
;                     (- (char-to-int (string-to-char (match-string 2 text)))
;                        128))))
;         (let ((seq-beg (match-beginning 0))
;               (data-beg (match-end 0))
;               (data-end (+ len (match-beginning 3)))
;               (cs (intern (match-string 3 text))))
;           (concat (substring text 0 seq-beg)
;                   (if (fboundp 'decode-coding-string)
;                       (decode-coding-string
;                        (substring text data-beg data-end) cs)
;                     (substring text data-beg data-end))
;                   (XDecodeCompoundText (substring text data-end)))))
;     text))

(defun XGetPropertyString (xdpy win atom)
  "On display XDPY, and window XWIN, get string property of type ATOM."
  (let ((propdata (XGetWindowProperty xdpy win atom 0 1024))
        (tdata nil)
	(retstring ""))
    (when (and propdata (setq tdata (nth 2 propdata)))
      (setq retstring tdata)
      (when (= (car propdata)
               (X-Atom-id (XInternAtom xdpy "COMPOUND_TEXT")))
        ;; Adjust RETSTRING in case of COMPOUND_TEXT
        (setq retstring (XDecodeCompoundText xdpy retstring)))

      (when (> (nth 1 propdata) 0.0)
        (setq propdata
              (XGetWindowProperty xdpy win atom
                                  1024 (nth 0 propdata)))
        (when (and propdata (setq tdata (nth 2 propdata)))
          (if (= (car propdata)
                 (X-Atom-id (XInternAtom xdpy "COMPOUND_TEXT")))
              (setq retstring
                    (concat retstring (XDecodeCompoundText xdpy tdata)))
            (setq retstring (concat retstring tdata))))))
    retstring))

(defun* make-X-Color (&key red green blue flags)
  (let ((col (make-ffi-object '(pointer XColor) (ffi-size-of-type 'XColor))))
    (when red
      (setf (XColor->red col) red))
    (when green
      (setf (XColor->green col) green))
    (when blue 
      (setf (XColor->blue col) blue))
    (when flags
      (setf (XColor->flags col) flags))
    col))

(defmacro X-Color-name (col)
  `(get ,col :color-name))
(defsetf X-Color-name (col) (name)
  `(put ,col :color-name ,name))

(defun XAllocNamedColor (dpy cmap name &rest unused)
  (let ((fc (make-ffi-object 'XColor)))
    (X:AllocNamedColor dpy cmap name (ffi-address-of fc) (ffi-address-of fc))
    (XColor->pixel fc)))

(defun XAllocColor (dpy cmap color)
  (X:AllocColor dpy cmap color)
  (XColor->pixel color))

(defun XQueryColors (dpy cmap colors)
  (error "Not implemented"))

(defmacro define-X-Gc-method (method mask &optional accessor)
  (unless accessor
    (setq accessor method))
  (let ((ms (intern (concat "X-Gc-" (symbol-name method))))
        (vs (intern (concat "XGCValues->" (symbol-name accessor)))))
    `(defun ,ms (gc)
       (let ((gv (make-ffi-object 'XGCValues)))
         (X:GetGCValues (get gc :display) gc ,mask (ffi-address-of gv))
         (,vs gv)))))

(defun X-Gc-p (gc) t)

(define-X-Gc-method function 1)
(define-X-Gc-method plane-mask 2 plane_mask)
(define-X-Gc-method foreground 4)
(define-X-Gc-method background 8)
(define-X-Gc-method line-width 16 line_width)
(define-X-Gc-method line-style 32 line_style)
(define-X-Gc-method cap-style 64 cap_style)
(define-X-Gc-method join-style 128 join_style)
(define-X-Gc-method fill-style 256 fill_style)
(define-X-Gc-method fill-rule 512 fill_rule)
(define-X-Gc-method tile 1024)
(define-X-Gc-method stipple 2048)
;(define-X-Gc-method font 16384)
(defun X-Gc-font (gc)
  (let ((gv (make-ffi-object 'XGCValues)))
    (X:GetGCValues (get gc :display) gc 16384 (ffi-address-of gv))
    (XGCValues->font gv)))
;    (X:QueryFont (get gc :display) (XGCValues->font gv))))
  
;; TODO: others
(defmacro X:make-gc-v (field vm)
  (let ((sym (intern (format "XGCValues->%S" field))))
    `(when (cadr gc)
       (setf (,sym fogc) (eval (cadr gc))
             vmask (Xmask-or vmask ,vm)))))
     
(defun X:make-X-Gc (&rest gc)
  (let ((fogc (make-ffi-object 'XGCValues))
        (vmask 0))
    (while gc
      (case (car gc)
        (:function (X:make-gc-v function 1))
        (:plane-mask (X:make-gc-v plane_mask 2))
        (:foreground (X:make-gc-v foreground 4))
        (:background (X:make-gc-v background 8))
        (:line-width (X:make-gc-v line_width 16))
        (:line-style (X:make-gc-v line_style 32))
        (:cap-style (X:make-gc-v cap_style 64))
        (:join-style (X:make-gc-v join_style 128))
        (:fill-style (X:make-gc-v fill_style 256))
        (:fill-rule (X:make-gc-v fill_rule 512))
        (:tile (X:make-gc-v tile 1024))
        (:stipple (X:make-gc-v stipple 2048))
        (:tile-stipple-x-origin
         (setf (XGCValues->ts_x_origin fogc) (cadr gc)
               vmask (Xmask-or vmask 4096)))
        (:tile-stipple-y-origin
         (setf (XGCValues->ts_y_origin fogc) (cadr gc)
               vmask (Xmask-or vmask 8192)))
        (:font
         (setf (XGCValues->font fogc) (cadr gc)
               vmask (Xmask-or vmask 16384)))
        (:subwindow-mode (X:make-gc-v subwindow_mode 32768))
        (:graphical-exposures
         (setf (XGCValues->graphics_exposures fogc) (cadr gc)
               vmask (Xmask-or vmask 65536)))
        (:clip-x-origin
         (setf (XGCValues->clip_x_origin fogc) (cadr gc)
               vmask (Xmask-or vmask 131072)))
        (:clip-y-origin
         (setf (XGCValues->clip_y_origin fogc) (cadr gc)
               vmask (Xmask-or vmask 262144)))
        (:clip-mask
         (setf (XGCValues->clip_mask fogc) (if (X-Pixmap-p (cadr gc))
                                               (X-Pixmap-id (cadr gc))
                                             (cadr gc))
               vmask (Xmask-or vmask 524288)))
        (:dash-offset
         (setf (XGCValues->dash_offset fogc) (cadr gc)
               vmask (Xmask-or vmask 1048576)))
        (:dashes
         (setf (XGCValues->dashes fogc) (cadr gc)
               vmask (Xmask-or vmask 2097152)))
        (:arc-mode
         (setf (XGCValues->arc_mode fogc) (cadr gc)
               vmask (Xmask-or vmask 4194304))))
      (setq gc (cddr gc)))
    (values fogc vmask)))

(defun XCreateGC (dpy d &rest gc-params)
  (multiple-value-bind (fogc vmask)
      (apply #'X:make-X-Gc gc-params)
    (let ((rgc (X:CreateGC dpy (X-Drawable-id d) vmask (ffi-address-of fogc))))
      (put rgc :display dpy)
      rgc)))

(defun-x11 XChangeGC (dpy gc &rest gc-params)
  (multiple-value-bind (fogc vmask)
      (apply #'X:make-X-Gc gc-params)
    (X:ChangeGC dpy gc vmask (ffi-address-of fogc))))

(defun-x11 XFreeGC (dpy gc)
  (X:FreeGC dpy gc))

(defun-x11 XRaiseWindow (dpy w)
  (X:RaiseWindow dpy (X-Win-id w)))

(defun-x11 XLowerWindow (dpy w)
  (X:LowerWindow dpy (X-Win-id w)))

(defun-x11 XReparentWindow (dpy win parwin x y)
  (X:ReparentWindow dpy (X-Win-id win) (X-Win-id parwin) x y))

(defun-x11 XDrawLine (dpy d gc x1 y1 x2 y2)
  (X:DrawLine dpy (X-Drawable-id d) gc x1 y1 x2 y2))

(defun X:make-points (points)
  (let ((fop-list (make-ffi-object `(array XPoint ,(length points))))
        (off 0))
    (while points
      (ffi-store fop-list off 'int (X-Point-x (car points)))
      (setq off (+ off (ffi-size-of-type 'short)))
      (ffi-store fop-list off 'int (X-Point-y (car points)))
      (setq off (+ off (ffi-size-of-type 'short)))
      (setq points (cdr points)))
    fop-list))

(defun-x11 XDrawLines (dpy d gc points &optional mode)
  (unless mode
    (setq mode X-Origin))
  (X:DrawLines dpy (X-Drawable-id d) gc
               (X:make-points points) (length points) mode))

(defun-x11 XDrawPoint (dpy d gc x y)
  (X:DrawPoint dpy (X-Drawable-id d) gc x y))

(defun-x11 XDrawPoints (dpy d gc points &optional mode)
  (unless mode
    (setq mode X-Origin))
  (X:DrawPoints dpy (X-Drawable-id d) gc (X:make-points points)
                (length points) mode))

(defun X:make-rectangles (rects)
  (let* ((frects (make-ffi-object `(array XRectangle ,(length rects))))
         (off 0))
    (while rects
      (ffi-store frects off 'short (X-Rect-x (car rects)))
      (setq off (+ off (ffi-size-of-type 'short)))
      (ffi-store frects off 'short (X-Rect-y (car rects)))
      (setq off (+ off (ffi-size-of-type 'short)))
      (ffi-store frects off 'unsigned-short (X-Rect-width (car rects)))
      (setq off (+ off (ffi-size-of-type 'unsigned-short)))
      (ffi-store frects off 'unsigned-short (X-Rect-height (car rects)))
      (setq off (+ off (ffi-size-of-type 'unsigned-short)))
      (setq rects (cdr rects)))
    frects))

(defun-x11 XDrawRectangle (dpy d gc x y width height)
  (X:DrawRectangle dpy (X-Drawable-id d) gc x y width height))

(defun-x11 XDrawRectangles (dpy d gc rects &optional fill)
  (if fill
      (XFillRectangles dpy d gc rects)
    (let ((rrects (and rects (X:make-rectangles rects))))
      (when rrects
        (X:DrawRectangles dpy (X-Drawable-id d) gc rrects (length rects))))))

(defun-x11 XFillRectangle (dpy d gc x y width height)
  (X:FillRectangle dpy (X-Drawable-id d) gc x y width height))

(defun-x11 XFillRectangles (dpy d gc rects)
  (let ((rrects (and rects (X:make-rectangles rects))))
    (when rrects
      (X:FillRectangles dpy (X-Drawable-id d) gc rrects (length rects)))))

(defun-x11 XClearArea (dpy win x y w h exposures)
  (X:ClearArea dpy (X-Win-id win) x y w h (if exposures 1 0)))

(defun-x11 XDrawString (dpy d gc x y str &optional len)
  (when (or (and len (>= len 255))
	    (>= (length str) 255))
    (setq str (substring str 0 254)))
  (unless len
      (setq len (length str)))
  (X:DrawString dpy (X-Drawable-id d) gc x y str len))

(defun-x11 XImageString (dpy d gc x y str &optional len)
  (unless len
    (setq len (length str)))
  (X:DrawImageString dpy (X-Drawable-id d) gc x y str len))

(defun-x11 XDrawSegments (dpy d gc segments)
  (let* ((slen (length segments))
         (fos (make-ffi-object `(array XSegment ,slen)))
         (off 0))
    (while segments
      (ffi-store fos off 'short (X-Point-x (caar segments)))
      (setq off (+ off (ffi-size-of-type 'short)))
      (ffi-store fos off 'short (X-Point-y (caar segments)))
      (setq off (+ off (ffi-size-of-type 'short)))
      (ffi-store fos off 'short (X-Point-x (cdar segments)))
      (setq off (+ off (ffi-size-of-type 'short)))
      (ffi-store fos off 'short (X-Point-y (cdar segments)))
      (setq off (+ off (ffi-size-of-type 'short)))
      (setq segments (cdr segments)))
    (X:DrawSegments dpy (X-Drawable-id d) gc fos slen)))

(defun-x11 XDrawArc (dpy d gc x y w h a1 a2)
  (X:DrawArc dpy (X-Drawable-id d) gc x y w h (* 64 a1) (* 64 a2)))

(defun X:make-arcs (xarcs)
  (let* ((arcs (make-ffi-object `(array XArc ,(length xarcs))))
         (off 0))
    (while xarcs
      (ffi-store arcs off 'short (X-Arc-x (car xarcs)))
      (setq off (+ off (ffi-size-of-type 'short)))
      (ffi-store arcs off 'short (X-Arc-y (car xarcs)))
      (setq off (+ off (ffi-size-of-type 'short)))
      (ffi-store arcs off 'unsigned-short (X-Arc-width (car xarcs)))
      (setq off (+ off (ffi-size-of-type 'unsigned-short)))
      (ffi-store arcs off 'unsigned-short (X-Arc-height (car xarcs)))
      (setq off (+ off (ffi-size-of-type 'unsigned-short)))
      (ffi-store arcs off 'short (* 64 (X-Arc-angle1 (car xarcs))))
      (setq off (+ off (ffi-size-of-type 'short)))
      (ffi-store arcs off 'short (* 64 (X-Arc-angle2 (car xarcs))))
      (setq off (+ off (ffi-size-of-type 'short)))
      (setq xarcs (cdr xarcs)))
    arcs))

(defun-x11 XDrawArcs (dpy d gc xarcs &optional fill mode)
  (funcall (if fill #'X:FillArcs #'X:DrawArcs)
           dpy (X-Drawable-id d) gc (X:make-arcs xarcs) (length xarcs)))

(defun-x11 XFillArc (dpy d gc x y w h a1 a2)
  (X:FillArc dpy (X-Drawable-id d) gc x y w h (* 64 a1) (* 64 a2)))

(defun-x11 XFillArcs (dpy d gc xarcs &optional mode)
  (X:FillArcs dpy (X-Drawable-id gc) gc (X:make-arcs xarcs) (length xarcs)))

(defun-x11 XFillPoly (dpy d gc pts &optional shape mode)
  (X:FillPolygon dpy (X-Drawable-id d) gc (X:make-points pts) (length pts)
                 (if shape shape X-Nonconvex) (if mode mode X-Origin)))

(defun-x11 XSelectInput (dpy w mask)
  (X:SelectInput dpy (X-Win-id w) mask))

(defun-x11 XOpenFont (dpy name)
  (X:LoadFont dpy name))

(defun XQueryFont (dpy font)
  (X:QueryFont dpy font))

(defmacro X-Dpy-fonts (dpy)
  `(get ,dpy :fonts))

(defun X-Font-p (font &optional sig)
  (or (and (ffi-object-p font)
           (not (ffi-null-p font))
           (equal (ffi-object-type font) '(pointer XFontStruct)))
      (and sig (error "Non-font struct: " sig))))

(defun X-Font-get (dpy font-name)
  (X:LoadFont dpy font-name))

;   (let ((fs (cdr (assoc font-name (X-Dpy-fonts dpy)))))
;     (unless fs
;       (setq fs (X:LoadQueryFont dpy font-name))
;       (push (cons font-name fs) (X-Dpy-fonts dpy)))
;     fs))

(defalias 'X-Font-fontdescent 'XFontStruct->descent)
(defalias 'X-Font-fontascent 'XFontStruct->ascent)

(defun X-Font-name (xdpy font-id)
  (let ((aval (make-ffi-object 'unsigned-long))
        (font (X:QueryFont xdpy font-id)))
    (unwind-protect
        (progn
          (X:GetFontProperty font XA-font (ffi-address-of aval))
          (XGetAtomName xdpy (ffi-get aval)))
      (X:FreeFontInfo (ffi-null-pointer) font 1))))

(defun X-Text-ascent (dpy font-id text &optional font-asc)
  "Return overall TEXT's ascent.
If FONT-ASC is non-nil, return FONT's ascent."
  (let ((font (X:QueryFont dpy font-id)))
    (when (ffi-null-p font)
      (error "font error:"))
      
    (unwind-protect
        (XFontStruct->ascent font)
      (X:FreeFontInfo (ffi-null-pointer) font 1))))

(defun X-Text-descent (dpy font-id text &optional font-desc)
  "Return overall TEXT's descent.
If FONT-DESC is non-nil, return FONT's descent."
  (let ((font (X:QueryFont dpy font-id)))
    (unwind-protect
        (XFontStruct->descent font)
    (X:FreeFontInfo (ffi-null-pointer) font 1))))

(defun X-Text-width (dpy font-id text)
  (let ((font (X:QueryFont dpy font-id)))
    (unwind-protect
        (X:TextWidth font text (length text))
      (X:FreeFontInfo (ffi-null-pointer) font 1))))

(defun X-Text-height (dpy font-id text)
  (let ((font (X:QueryFont dpy font-id)))
    (unwind-protect
        (+ (XFontStruct->ascent font)
           (XFontStruct->descent font))
      (X:FreeFontInfo (ffi-null-pointer) font 1))))

(defun-x11 XCreatePixmap (dpy d depth width height)
  (X-Pixmap-find-or-make
   dpy (X:CreatePixmap dpy (X-Drawable-id d) width height depth)))

(defun-x11 XFreePixmap (dpy p)
  (X:FreePixmap dpy (X-Pixmap-id p)))

(defalias 'X-Image-width 'XImage->width)
(defalias 'X-Image-height 'XImage->height)

(defun-x11 XImagePut (dpy gc d x y img &optional src_x src_y width height)
  (X:PutImage dpy (X-Drawable-id d) gc img 0 0 x y (XImage->width img)
              (XImage->height img)))

(defun-x11 XImageGet (dpy d x y width height)
  (X:GetImage dpy (X-Drawable-id d) x y width height X-AllPlanes X-ZPixmap))

(defalias 'XDestroyImage 'X:DestroyImage)

(defun-x11 XSetDashes (dpy gc dash-offset dashes)
  (let ((fd (make-ffi-object `(array char ,(length dashes)))))
    (dotimes (idx (length dashes))
      (ffi-aset fd idx (int-to-char (nth idx dashes))))
    (X:SetDashes dpy gc dash-offset fd (length dashes))))

(defun-x11 XSetClipRectangles (dpy gc clip-x-origin clip-y-origin rectangels
                                   &optional order)
  (unless order
    (setq order X-UnSorted))
  (X:SetClipRectangles
   dpy gc clip-x-origin clip-y-origin
   (X:make-rectangles rectangels) (length rectangels) order))

(defun-x11 XGrabKeyboard (dpy grab-win &optional owe pmode kmode time)
  (X:GrabKeyboard dpy (X-Win-id grab-win) (if owe 1 0)
                  (or pmode X-GrabModeAsync) (or kmode X-GrabModeAsync)
                  (or time X-CurrentTime)))

(defun-x11 XUngrabKeyboard (dpy &optional time)
  (X:UngrabKeyboard dpy (or time X-CurrentTime)))

(defun-x11 XGrabPointer (dpy grab-win ev-mask
                             &optional cursor owe pmode kmode confto-win time)
  (X:GrabPointer dpy (X-Win-id grab-win) (if owe 1 0)
                 ev-mask (or pmode X-GrabModeAsync) (or kmode X-GrabModeAsync)
                 (if confto-win (X-Win-id confto-win) X-None)
                 (or cursor X-None)
                 (or time X-CurrentTime)))

(defun-x11 XUngrabPointer (dpy &optional time)
  (X:UngrabPointer dpy (or time X-CurrentTime)))

(defun-x11 XChangeActivePointerGrab (xdpy cursor ev-mask &optional time)
  (X:ChangeActivePointerGrab
   xdpy (or cursor X-None) ev-mask (or time X-CurrentTime)))

(defun-x11 XQueryPointer (dpy win)
  (let ((r-root (make-ffi-object 'Window))
        (r-child (make-ffi-object 'Window))
        (r-rx (make-ffi-object 'int))
        (r-ry (make-ffi-object 'int))
        (r-wx (make-ffi-object 'int))
        (r-wy (make-ffi-object 'int))
        (r-m (make-ffi-object 'unsigned-int)))
    (X:QueryPointer dpy (X-Win-id win) (ffi-address-of r-root)
                    (ffi-address-of r-child) (ffi-address-of r-rx)
                    (ffi-address-of r-ry) (ffi-address-of r-wx)
                    (ffi-address-of r-wy) (ffi-address-of r-m))
    (list t 1 nil (X-Win-find-or-make dpy (ffi-get r-root))
          (X-Win-find-or-make dpy (ffi-get r-child))
          (ffi-get r-rx) (ffi-get r-ry)
          (ffi-get r-wx) (ffi-get r-wy)
          (ffi-get r-m))))

(defun-x11 XWarpPointer (dpy src-win dst-win src-x src-y
                             src-width src-height dest-x dest-y)
  (X:WarpPointer dpy (X-Win-id src-win) (X-Win-id dst-win)
                 src-x src-y src-width src-height dest-x dest-y))

(defun-x11 XTranslateCoordinates (dpy src-win dst-win src-x src-y)
  (let ((dest-x-ret (make-ffi-object 'int))
        (dest-y-ret (make-ffi-object 'int))
        (child-ret (make-ffi-object 'Window)))
    (X:TranslateCoordinates
     dpy (X-Win-id src-win) (X-Win-id dst-win) src-x src-y
     (ffi-address-of dest-x-ret) (ffi-address-of dest-y-ret)
     (ffi-address-of child-ret))
    (cons (cons (ffi-get dest-x-ret) (ffi-get dest-y-ret))
          (X-Win-find-or-make dpy (ffi-get child-ret)))))

(defun XCreateFontCursor (dpy shape)
  (X:CreateFontCursor dpy shape))

(defun XCreateGlyphCursor (xdpy &rest cursor-attrs)
  (let ((src (plist-get cursor-attrs :source))
        (mask (plist-get cursor-attrs :mask))
        (src-chr (plist-get cursor-attrs :src-char))
        (msk-chr (plist-get cursor-attrs :msk-char))
        (fg (make-X-Color :red (plist-get cursor-attrs :fgred)
                          :green (plist-get cursor-attrs :fggreen)
                          :blue (plist-get cursor-attrs :fgblue)))
        (bg (make-X-Color :red (plist-get cursor-attrs :bgred)
                          :green (plist-get cursor-attrs :bggren)
                          :blue (plist-get cursor-attrs :bgblue))))
    (X:CreateGlyphCursor xdpy src mask src-chr msk-chr fg bg)))

(defun-x11 XRecolorCursor  (dpy cursor fore-red fore-green fore-blue
                                &optional back-red back-green back-blue)
  (let ((fg (make-X-Color :red fore-red :green fore-green :blue fore-blue))
        (bg (make-X-Color :red back-red :green back-green :blue back-blue)))
  (X:RecolorCursor dpy cursor fg bg)))

(defun-x11 XFreeCursor (dpy cursor)
  (X:FreeCursor dpy cursor))

(defun-x11 XChangeSaveSet (dpy win change-mode)
  (X:ChangeSaveSet dpy (X-Win-id win) change-mode))

(defun-x11 XAddToSaveSet (dpy win)
  (X:AddToSaveSet dpy (X-Win-id win)))

(defun-x11 XRemoveFromSaveSet (dpy win)
  (X:RemoveFromSaveSet dpy (X-Win-id win)))

(defun-x11 XSetCloseDownMode (dpy mode)
  (X:SetCloseDownMode dpy mode))

(defun-x11 XKillClient (dpy res-id)
  (X:KillClient dpy res-id))


;;; Hints stuff
(defalias 'X-WMSize-flags 'XSizeHints->flags)
(defalias 'X-WMSize-x 'XSizeHints->x)
(defalias 'X-WMSize-y 'XSizeHints->y)
(defalias 'X-WMSize-width 'XSizeHints->width)
(defalias 'X-WMSize-height 'XSizeHints->height)
(defalias 'X-WMSize-min-width 'XSizeHints->min_width)
(defalias 'X-WMSize-min-height 'XSizeHints->min_height)
(defalias 'X-WMSize-max-width 'XSizeHints->max_width)
(defalias 'X-WMSize-max-height 'XSizeHints->max_height)
(defalias 'X-WMSize-width-inc 'XSizeHints->width_inc)
(defalias 'X-WMSize-height-inc 'XSizeHints->height_inc)
(defalias 'X-WMSize-min-aspect-x 'XSizeHints->min_aspect_x)
(defalias 'X-WMSize-min-aspect-y 'XSizeHints->min_aspect_y)
(defalias 'X-WMSize-max-aspect-x 'XSizeHints->max_aspect_x)
(defalias 'X-WMSize-max-aspect-y 'XSizeHints->max_aspect_y)
(defalias 'X-WMSize-base-width 'XSizeHints->base_width)
(defalias 'X-WMSize-base-height 'XSizeHints->base_height)
(defalias 'X-WMSize-gravity 'XSizeHints->win_gravity)

(defsubst X-WMSize-p (wms &optional sig)
  (and (ffi-object-p wms)
       (or (equal (ffi-object-type wms) '(pointer XSizeHints))
           (equal (ffi-object-type wms) 'XSizeHints))))

(defsubst X-WMSize-uspos-p (wms)
  "Return non-nil if WMS have user specified x, y."
  (Xtest (X-WMSize-flags wms) 1))

(defsubst X-WMSize-ussize-p (wms)
  "Return non-nil if WMS have user specified width, height."
  (Xtest (X-WMSize-flags wms) 2))

(defsubst X-WMSize-ppos-p (wms)
  "Return non-nil if WMS have program specified position."
  (Xtest (X-WMSize-flags wms) 4))

(defsubst X-WMSize-psize-p (wms)
  "Return non-nil if WMS have program specified size."
  (Xtest (X-WMSize-flags wms) 8))

(defsubst X-WMSize-pminsize-p (wms)
  "Return non-nil if WMS have program specified minimum size."
  (Xtest (X-WMSize-flags wms) 16))

(defsubst X-WMSize-pmaxsize-p (wms)
  "Return non-nil if WMS have program specified maximum size."
  (Xtest (X-WMSize-flags wms) 32))

(defsubst X-WMSize-presizeinc-p (wms)
  "Return non-nil if WMS have program specified resize increments."
  (Xtest (X-WMSize-flags wms) 64))

(defsubst X-WMSize-paspect-p (wms)
  "Return non-nil if WMS have program specified min and max aspect ratios."
  (Xtest (X-WMSize-flags wms) 128))

(defsubst X-WMSize-pbasesize-p (wms)
  "Return non-nil if WMS have program specified base for incrementing."
  (Xtest (X-WMSize-flags wms) 256))

(defsubst X-WMSize-pgravity-p (wms)
  "Return non-nil if WMS have program specified window graivty."
  (Xtest (X-WMSize-flags wms) 512))

(defun XGetWMNormalHints (dpy w)
  (let ((nh (make-ffi-object 'XSizeHints))
        (rl (make-ffi-object 'long)))
    (X:GetWMNormalHints
     dpy (X-Win-id w) (ffi-address-of nh) (ffi-address-of rl))
    nh))

(defun X:make-XSizeHints (&rest hints)
  (let ((nh (make-ffi-object 'XSizeHints)))
    (while hints
      (case (car hints)
        (:flags (setf (XSizeHints->flags nh) (cadr hints)))
        (:x (setf (XSizeHints->x nh) (cadr hints)))
        (:y (setf (XSizeHints->y nh) (cadr hints)))
        (:width (setf (XSizeHints->width nh) (cadr hints)))
        (:height (setf (XSizeHints->height nh) (cadr hints)))
        (:min-width (setf (XSizeHints->min_width nh) (cadr hints)))
        (:min-height (setf (XSizeHints->min_height nh) (cadr hints)))
        (:max-width (setf (XSizeHints->max_width nh) (cadr hints)))
        (:max-height (setf (XSizeHints->max_height nh) (cadr hints)))
        (:width-inc (setf (XSizeHints->width_inc nh) (cadr hints)))
        (:height-inc (setf (XSizeHints->height_inc nh) (cadr hints)))
        (:min-aspect-x (setf (XSizeHints->min_aspect_x nh) (cadr hints)))
        (:min-aspect-y (setf (XSizeHints->min_aspect_y nh) (cadr hints)))
        (:max-aspect-x (setf (XSizeHints->max_aspect_x nh) (cadr hints)))
        (:max-aspect-y (setf (XSizeHints->max_aspect_y nh) (cadr hints)))
        (:base-width (setf (XSizeHints->base_width nh) (cadr hints)))
        (:base-height (setf (XSizeHints->base_height nh) (cadr hints)))
        (:gravity (setf (XSizeHints->win_gravity nh) (cadr hints))))
      (setq hints (cddr hints)))
    nh))

(defalias 'make-X-WMSize 'X:make-XSizeHints)

(defun XSetWMNormalHints (dpy w &rest hints)
  (let ((hn (apply #'X:make-XSizeHints hints)))
    (X:SetWMNormalHints dpy (X-Win-id w) (ffi-address-of hn))))

(defalias 'X-WMHints-flags 'XWMHints->flags)
(defalias 'X-WMHints-input 'XWMHints->input)
(defalias 'X-WMHints-initial-state 'XWMHints->initial_state)
(defalias 'X-WMHints-icon-pixmap 'XWMHints->icon_pixmap)
(defalias 'X-WMHints-icon-window 'XWMHints->icon_window)
(defalias 'X-WMHints-icon-x 'XWMHints->icon_x)
(defalias 'X-WMHints-icon-y 'XWMHints->icon_y)
(defalias 'X-WMHints-icon-mask 'XWMHints->icon_mask)
(defalias 'X-WMHints-window-group 'XWMHints->window_group)

(defsubst X-WMHints-input-p (wmh)
  "Return non-nil if WMH have InputHint."
  (Xtest (X-WMHints-flags wmh) 1))

(defsubst X-WMHints-state-p (wmh)
  "Return non-nil if WMH have StateHint."
  (Xtest (X-WMHints-flags wmh) 2))

(defsubst X-WMHints-iconpixmap-p (wmh)
  "Return non-nil if WMH have IconPixmapHint."
  (Xtest (X-WMHints-flags wmh) 4))

(defsubst X-WMHints-iconwindow-p (wmh)
  "Return non-nil if WMH have IconWindowHint."
  (Xtest (X-WMHints-flags wmh) 8))

(defsubst X-WMHints-iconpos-p (wmh)
  "Return non-nil if WMH have IconPositionHint."
  (Xtest (X-WMHints-flags wmh) 16))

(defsubst X-WMHints-iconmask-p (wmh)
  "Return non-nil if WMH have IconMaskHint."
  (Xtest (X-WMHints-flags wmh) 32))

(defsubst X-WMHints-wingroup-p (wmh)
  "Return non-nil if WMH have WindowGroupHint."
  (Xtest (X-WMHints-flags wmh) 64))

(defsubst X-WMHints-urgency-p (wmh)
  "Return non-nil if WMH have UrgencyHint."
  (Xtest (X-WMHints-flags wmh) 256))

(defun XGetWMHints (dpy w)
  (let ((wmhp (X:GetWMHints dpy (X-Win-id w))))
    (unless (ffi-null-p wmhp)
      wmhp)))

(defun X:make-XWMHints (&rest params)
  (let ((wmh (make-ffi-object 'XWMHints)))
    (while params
      (case (car params)
        (:flags (setf (XWMHints->flags wmh) (cadr params)))
        (:input (setf (XWMHints->input wmh) (if (cadr params) 1 0)))
        (:initial-state (setf (XWMHints->initial_state wmh) (cadr params)))
        (:icon-pixmap (setf (XWMHints->icon_pixmap wmh) (cadr params)))
        (:icon-window (setf (XWMHints->icon_window wmh) (cadr params)))
        (:icon-x (setf (XWMHints->icon_x wmh) (cadr params)))
        (:icon-y (setf (XWMHints->icon_y wmh) (cadr params)))
        (:icon-mask (setf (XWMHints->icon_mask wmh) (cadr params)))
        (:window-group (setf (XWMHints->window_group wmh) (cadr params))))
      (setq params (cddr params)))
    wmh))
(defalias 'make-X-WMHints 'X:make-XWMHints)

(defun-x11 XSetWMHints (dpy w &rest params)
  (X:SetWMHints
   dpy (X-Win-id w) (ffi-address-of (apply #'X:make-XWMHints params))))

(defun XGetWMProtocols (dpy win)
  (let ((rp (make-ffi-object '(pointer Atom)))
        (rc (make-ffi-object 'int))
        ret-list)
    ;; From XGetWMProtocols(3)
    ;;  XGetWMProtocols sets the
    ;;  protocols_return argument to a list of atoms, sets the count_return
    ;;  argument to the number of elements in the list, and returns a nonzero
    ;;  status.  Otherwise, it sets neither of the return arguments and returns
    ;;  a zero status.  To release the list of atoms, use XFree.
    (unless (zerop (X:GetWMProtocols
                    dpy (X-Win-id win) (ffi-address-of rp) (ffi-address-of rc)))
      (dotimes (idx (ffi-get rc))
        (push (ffi-aref rp idx) ret-list))
      (nreverse ret-list))))

(defun XWMProtocol-set-p (xdpy wmprotos name)
  "Return non-nil when atom with NAME is in WM_PROTOCOLS WMPROTO."
  (member* (XInternAtom xdpy name t) wmprotos :test 'X-Atom-equal))
      
(defun-x11 XSetWMProtocols (dpy win atoms)
  (let* ((natoms (length atoms))
         (fas (make-ffi-object `(array Atom ,natoms))))
    (dotimes (idx natoms)
      (ffi-aset fas idx (nth idx atoms)))
    (X:SetWMProtocols dpy (X-Win-id win) fas natoms)))

(defun XGetWMName (dpy win)
;  (let ((fnp (make-ffi-object 'XTextProperty)))
;    (X:GetWMName dpy (X-Win-id win) (ffi-address-of fnp))
;    (if (not (ffi-null-p fnp))
;        (unwind-protect
;            (let ((sn (ffi-fetch (XTextProperty->value fnp)
;                                 (XTextProperty->nitems fnp) 'c-string))
;                  (ea (XTextProperty->encoding fnp)))
;              (xwem-message 'debug "XA: %s" (XGetAtomName (xwem-dpy) ea))
;              sn)
;          nil)
;;          (X:Free (XTextProperty->value fnp)))
;      (XGetPropertyString dpy win XA-wm-name))))
  (let ((fn (make-ffi-object 'c-string)))
    (X:FetchName dpy (X-Win-id win) (ffi-address-of fn))
    (if (not (ffi-null-p fn))
        (prog1 (ffi-get fn) (X:Free fn))
      (XGetPropertyString dpy win XA-wm-name))))

(defun-x11 XSetWMName (dpy win wm-name)
  (let ((wn-cs (ffi-create-fo 'c-string wm-name)))
    (X:StoreName dpy (X-Win-id win) wn-cs)))

(defun-x11 XSetWMClass (dpy win wm-class)
  (let ((rn (ffi-create-fo 'c-string (first wm-class)))
        (rc (ffi-create-fo 'c-string (second wm-class)))
        (ch (make-ffi-object 'XClassHint)))
    (setf (XClassHint->res_name ch) rn
          (XClassHint->res_class ch) rc)
    (X:SetClassHint dpy (X-Win-id win) (ffi-address-of ch))))

(defun XGetWMClass (dpy win)
  (let ((ch (make-ffi-object 'XClassHint)))
    (setf (XClassHint->res_name ch) (ffi-null-pointer)
          (XClassHint->res_class ch) (ffi-null-pointer))
    (X:GetClassHint dpy (X-Win-id win) (ffi-address-of ch))
    (list (or (and (not (ffi-null-p (XClassHint->res_name ch)))
                   (ffi-get (XClassHint->res_name ch) :type 'c-string))
              "")
          (or (and (not (ffi-null-p (XClassHint->res_class ch)))
                   (ffi-get (XClassHint->res_class ch) :type 'c-string))
              ""))))

(defun XGetWMCommand (dpy win)
  "On display XDPY, get window's WIN WM_COMMAND."
   (XGetPropertyString dpy win XA-wm-command))

(defun XSetWMCommand (dpy win cmd)
  (error "not implemented yet"))

(defun XGetWMTransientFor (dpy win)
  "On DPY return transient window for given WIN."
  (let ((wid (make-ffi-object 'Window)))
    (X:GetTransientForHint dpy (X-Win-id win) (ffi-address-of wid))
    (unless (= (ffi-get wid) X-None)
      (X-Win-find-or-make dpy (ffi-get wid)))))

(defun XSetWMTransientFor (dpy win tf)
  (X:SetTransientForHint dpy (X-Win-id win) (X-Win-id tf)))

(defun XGetWMState (xdpy win)
  "On display XDPY get WM_STATE property for WIN."
  (let ((wmsa (XInternAtom xdpy "WM_STATE" nil)))
    (nth 2 (XGetWindowProperty xdpy win wmsa 0 2 wmsa))))

(defun XSetWMState (xdpy win wm-state &optional icon-id)
  "On display XDPY, set window's WIN state to WM-STATE.
WM-STATE is one of `X-WithdrawnState', `X-NormalState' or `X-IconicState'."
  (let ((wmsa (XInternAtom xdpy "WM_STATE" nil)))
    (XChangeProperty xdpy win wmsa wmsa X-format-32 X-PropModeReplace
                     (list wm-state (or icon-id 0.0)))))
  
(defun-x11 XNextEvent (dpy &optional timeout predict)
  (let ((xev (make-ffi-object 'XEvent)))
    (X:NextEvent dpy (ffi-address-of xev))
    xev))

(defun-x11 XSendEvent (dpy win propogate ev_mask xevent)
  (X:SendEvent dpy (X-Win-id win) (if propogate 1 0) ev_mask xevent))

(defun-x11 XAllowEvents (dpy mode &optional time)
  (X:AllowEvents dpy mode (or time X-CurrentTime)))

(defun-x11 XSetInputFocus (dpy win-or-val rev-to &optional time)
  (X:SetInputFocus dpy (if (X-Win-p win-or-val)
                           (X-Win-id win-or-val)
                         win-or-val) rev-to (or time X-CurrentTime)))

(defun-x11 XGrabButton
  (dpy button mods grab-win ev-mask &optional cursor owe pmode kmode conf-to)
  (X:GrabButton dpy button mods (X-Win-id grab-win) (if owe 1 0) ev-mask
                (or pmode X-GrabModeAsync) (or kmode X-GrabModeAsync)
                (if conf-to (X-Win-id conf-to) X-None) (or cursor X-None)))

(defun-x11 XUngrabButton (dpy button mods grab-win)
  (X:UngrabButton dpy button mods (X-Win-id grab-win)))

(defun-x11 XGrabKey (dpy keycode mods grab-win &optional owe pmode kmode)
  (X:GrabKey dpy keycode mods (X-Win-id grab-win) (if owe 1 0)
             (or pmode X-GrabModeAsync) (or kmode X-GrabModeAsync)))

(defun-x11 XUngrabKey (dpy key mods grab-win)
  (X:UngrabKey dpy key mods (X-Win-id grab-win)))

(defun XGetModifierMapping (dpy)
  (let ((mkm (X:GetModifierMapping dpy))
        max-kpm modmap)
    (setq max-kpm (XModifierKeymap->max_keypermod mkm)
          modmap (XModifierKeymap->modifiermap mkm))
    (let ((retval nil))
      (dotimes (mi 8)
        (push
         (let ((kmlist nil))
           (dotimes (mk max-kpm (nreverse kmlist))
             (push (char-to-int
                    (ffi-fetch modmap (+ (* mi max-kpm) mk) 'KeyCode))
                   kmlist)))
         retval))
      (prog1
          (nconc '(t 0 0 0 0 0 0 0) (list (nreverse retval)))
        (X:FreeModifiermap mkm)))))

(defun XGetKeyboardMapping (dpy keycode count)
  (let* ((kpkr (make-ffi-object 'int))
         (ksyms (X:GetKeyboardMapping
                 dpy (int-to-char keycode) count (ffi-address-of kpkr)))
         (ksyms-per-kcode (ffi-get kpkr))
         kcsyms kclist)
    (while (> count 0)
      (dotimes (idx ksyms-per-kcode)
        (push (ffi-aref ksyms (+ (* (1- count) ksyms-per-kcode) idx))
              kcsyms))
      (push (nreverse kcsyms) kclist)
      (setq kcsyms nil)
      (decf count))
    (prog1
        (list t 0 kclist)
      (X:Free ksyms))))

(defun XGetInputFocus (dpy)
  (let ((rw (make-ffi-object 'Window))
        (rev-to (make-ffi-object 'int)))
    (X:GetInputFocus dpy (ffi-address-of rw) (ffi-address-of rev-to))
    (X-Win-find-or-make dpy (ffi-get rw))))

(defun-x11 XCopyArea (dpy src-d dst-d gc src-x src-y width height dst-x dst-y)
  (X:CopyArea dpy (X-Drawable-id src-d) (X-Drawable-id dst-d)
              gc src-x src-y width height dst-x dst-y))

(defun XQueryExtension (dpy name)
  (let* ((mop (make-ffi-object 'int))
         (fev (make-ffi-object 'int))
         (fer (make-ffi-object 'int)))
    (unless (zerop (X:QueryExtension
                    dpy (ffi-create-fo 'c-string name) (ffi-address-of mop)
                    (ffi-address-of fev) (ffi-address-of fer)))
      (list t nil nil nil (ffi-get mop)))))

(defun X-Dpy-get-extension (xdpy extname &optional sig)
  "On display XDPY get extension with EXTNAME.
If SIG, then signal an error if extension is not available."
  (let ((ext (XQueryExtension xdpy extname)))
    (if (and (null ext) sig)
	(signal 'search-failed (list sig 'X-Dpy-get-extension extname))
      ext)))

(defun XSetSelectionOwner (dpy selection-atom &optional owner-win time)
  "Set SELECTION-ATOM to be owned by OWNER-WIN."
  (X:SetSelectionOwner
   dpy selection-atom (if owner-win (X-Win-id owner-win) X-None)
   (or time X-CurrentTime)))

(defun XGetSelectionOwner (dpy selection-atom)
  "Get owner of SELECTION-ATOM on display DPY.
Returns nil or X-Win structure."
  (X-Win-find-or-make dpy (X:GetSelectionOwner dpy selection-atom)))

(defun XConvertSelection (dpy selection target prop requestor &optional time)
  "ConvertSelection."
  (ffi-get (X:ConvertSelection
            dpy selection target prop
            (if requestor (X-Win-id requestor) X-None)
            (or time X-CurrentTime))))

(defun XSynchronize (dpy onoff)
  "For non-nil ONOFF turn on synchronous behaviour."
  (X:Synchronize dpy (if onoff 1 0)))

;;; Various compat macros/functions
(defmacro XCharacter (val)
  "Convert VAL (a float) into a truncated character value."
  (if (fboundp 'int-to-char)
      (list 'int-to-char
	    (list 'logand (list 'truncate (list 'mod val 65536)) 255))
    (if (>= emacs-major-version 20)
	(list 'logand (list 'truncate (list 'mod val 65536)) 255)
      (list 'truncate (list 'mod val 65536)))))

(defsetf X-Dpy-snd-queue (dpy) (q))
(defsetf X-Dpy-message-buffer (dpy) (mb))

(provide 'xlib-xlib)

;;; xlib-xlib.el ends here
