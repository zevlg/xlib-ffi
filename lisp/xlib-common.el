;;; xlib-common.el --- Code grabed from non-ffi xlib.

;; Copyright (C) 2005 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Sun Aug 28 02:19:51 MSD 2005
;; Keywords: ffi,xlib

;; This file is part of SXEmacs.

;; SXEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; SXEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; 

;;; Code:

(define-error 'X-Error
  "X Server error.")

(define-error 'X-Events-stop
  "Error used to stop X events processing.")

(defvar xlib-opcodes-alist
  '((104 . XBell)
    (1 . XCreateWindow)
    (2 . XChangeWindowAttributes)
    (3 . XGetWindowAttributes)
    (12 . XConfigureWindow)
    (8 . XMapWindow)
    (10 . XUnmapWindow)
    (4 . XDestroyWindow)
    (5 . XDestroySubwindows)
    (15 . XQueryTree)
    (16 . XInternAtom)
    (17 . XGetAtomName)
    (18 . XChangeProperty)
    (20 . XGetWindowProperty)
    (78 . XCreateColormap)
    (79 . XFreeColormap)
    (84 . XAllocColor)
    (85 . XAllocNamedColor)
    (86 . XAllocColorCells)
    (89 . XStoreColors)
    (88 . XFreeColors)
    (91 . XQueryColors)
    (55 . XCreateGC)
    (56 . XChangeGC)
    (58 . XSetDashes)
    (59 . XSetClipRectangles)
    (60 . XFreeGC)
    (61 . XClearArea)
    (62 . XCopyArea)
    (63 . XCopyPlane)
    (64 . XDrawPoints)
    (65 . XDrawLines)
    (69 . XFillPoly)
    (66 . XDrawSegments)
    (67 . XDrawRectangles)
    (70 . XDrawRectangles)
    (68 . XDrawArcs)
    (71 . XDrawArcs)
    (74 . XDrawString)
    (76 . XImageString)
    (72 . XPutImage)
    (73 . XGetImage)
    (22 . XSetSelectionOwner)
    (23 . XGetSelectionOwner)
    (24 . XConvertSelection)
    (41 . XWarpPointer)
    (36 . XGrabServer)
    (37 . XUngrabServer)
    (38 . XQueryPointer)
    (31 . XGrabKeyboard)
    (32 . XUngrabKeyboard)
    (26 . XGrabPointer)
    (27 . XUngrabPointer)
    (28 . XGrabButton)
    (29 . XUngrabButton)
    (33 . XGrabKey)
    (34 . XUngrabKey)
    (43 . XGetInputFocus)
    (42 . XSetInputFocus)
    (7 . XReparentWindow)
    (14 . XGetGeometry)
    (40 . XTranslateCoordinates)
    (6 . XChangeSaveSet)
    (25 . XSendEvent)
    (44 . XQueryKeymap)
    (101 . XGetKeyboardMapping)
    (119 . XGetModifierMapping)
    (45 . XOpenFont)
    (47 . XQueryFont)
    (48 . XQueryTextExtents)
    (53 . XCreatePixmap)
    (54 . XFreePixmap)
    (93 . XCreateCursor)
    (94 . XCreateGlyphCursor)
    (95 . XFreeCursor)
    (96 . XRecolorCursor)
    (30 . XChangeActivePointerGrab)
    (98 . XQueryExtension)
    (107 . XSetScreenSaver)
    (108 . XGetScreenSaver)
    (113 . XKillClient)
    (115 . XForceScreenSaver))
  "Alist of X opcodes in form (OPCODE . FUNCTION).
This is only informative variable.")

;;; Math stuff
(defmacro Xtruncate (val)
  "Do a safe truncate of VAL that might be larger than MAXINT."
  `(truncate (if ,val (mod ,val 65536) 0)))

(defun Xmask (pos)
  "Create a mask with a bit set in position POS.
This routine will not work for position 32 and up because we sim
4 bytes of info"
  (if (< pos 16)
      (float (lsh 1 pos))		; put in first byte
    (setq pos (- pos  16))		; divide pos by 16
    (* (float (lsh 1 pos)) (float 65536)))) ; push into high byte

(defun Xmask-or (val &rest args)
  "Logically or VAL and MASK together.
They are floats to be broken down into two two byte ints.
MASK is stored in ARGS which is a list of *fill in when I remember*"
  (while args
    (let ((mask (car args)))
      (setq args (cdr args))
      (let ((lv (logand (Xtruncate val) 65535))
	    (hv (Xtruncate (/ val (float 65536))))
	    (lm (logand (Xtruncate mask) 65535))
	    (hm (Xtruncate (/ mask (float 65536)))))
	(setq val (+ (float (logior lv lm))
		     (* (float (logior hv hm)) 65536))))))
  val)

(defun Xmask-and (val &rest args)
  "Logically `and' VAL and MASK together.
They are floats to be broken down into two two byte ints.
MASK is stored in ARGS which is a list of *fill in when I remember*"
  (while args
    (let ((mask (car args)))
      (setq args (cdr args))
      (let ((lv (logand (Xtruncate val) 65535))
	    (hv (Xtruncate (/ val (float 65536))))
	    (lm (logand (Xtruncate mask) 65535))
	    (hm (Xtruncate (/ mask (float 65536)))))
	(setq val (+ (float (logand lv lm))
		     (* (float (logand hv hm)) 65536))))))
  val)

(defun Xtest (val flag)
  "Test value of bytes VAL for presence of FLAG.
Return t if it exists, nil otherwise."
  (if (= (Xmask-and val flag) 0) nil t))


(defstruct (X-Point (:predicate X-Point-ispoint-p))
  xx yy)

(defsubst X-Point-p (xpnt &optional sig)
  "Return non-nil if XPNT is point."
  (let ((ispnt (or (consp xpnt) (X-Point-ispoint-p xpnt))))
    (if (and sig (not ispnt))
	(signal 'wrong-type-argument (list sig 'X-Point-p xpnt))
      ispnt)))

(defmacro X-Point-x (xpnt)
  `(if (consp ,xpnt)
       (car ,xpnt)
     (X-Point-xx ,xpnt)))

(defmacro X-Point-y (xpnt)
  `(if (consp ,xpnt)
       (cdr ,xpnt)
     (X-Point-yy ,xpnt)))

(defsetf X-Point-x (xpnt) (val)
  `(if (consp ,xpnt)
       (setcar ,xpnt ,val)
     (setf (X-Point-xx ,xpnt) ,val)))

(defsetf X-Point-y (xpnt) (val)
  `(if (consp ,xpnt)
       (setcdr ,xpnt ,val)
     (setf (X-Point-yy ,xpnt) ,val)))


(defstruct (X-Rect (:predicate X-Rect-isrect-p)
                   (:type vector) :named)
  x y width height)

(defsubst X-Rect-p (xrect &optional sig)
  "Return non-nil if XRECT is X-Rect structure."
  (X-Generic-p 'X-Rect 'X-Rect-isrect-p xrect sig))

(defun X-Rect-internal-intersect-p (xrect1 xrect2)
  "Return non-nil if two rectangles XRECT1 and XRECT2 have common part."
  (let ((minx (min (X-Rect-x xrect1) (X-Rect-x xrect2)))
	(maxx (max (+ (X-Rect-x xrect1) (X-Rect-width xrect1))
		   (+ (X-Rect-x xrect2) (X-Rect-width xrect2))))
	(miny (min (X-Rect-y xrect1) (X-Rect-y xrect2)))
	(maxy (max (+ (X-Rect-y xrect1) (X-Rect-height xrect1))
		   (+ (X-Rect-y xrect2) (X-Rect-height xrect2)))))

  (and (> (+ (X-Rect-width xrect1) (X-Rect-width xrect2))
	  (- maxx minx))

       (> (+ (X-Rect-height xrect1) (X-Rect-height xrect2))
	  (- maxy miny)))))

(defun X-Rect-intersect-p (&rest xrects)
  "Return non-nil if rectangles in XRECTS are intersects."
  (while (and xrects
              (every #'(lambda (r)
                         (not (X-Rect-internal-intersect-p (car xrects) r)))
                     (cdr xrects)))
    (setq xrects (cdr xrects)))
  xrects)


(defstruct (X-Geom (:type vector) :named
                   (:include X-Rect)
		   (:predicate X-Geom-isgeom-p))
  (border-width 0))

(defun X-Geom-p (geom &optional sig)
  "Return non-nil if GEOM is X-Geom structure.
If SIG is gived and GEOM is not X-Geom structure, SIG will be signaled."
  (X-Generic-p 'X-Geom 'X-Geom-isgeom-p geom sig))

(defun X-Geom-apply (fn geom1 geom2)
  "Apply function FN to each element of GEOM1 and GEOM2.
Return new geom."
  (X-Geom-p geom1 'X-Geom-apply)
  (X-Geom-p geom2 'X-Geom-apply)

  (make-X-Geom :x (funcall fn (X-Geom-x geom1) (X-Geom-x geom2))
	       :y (funcall fn (X-Geom-y geom1) (X-Geom-y geom2))
	       :width (funcall fn (X-Geom-width geom1) (X-Geom-width geom2))
	       :height (funcall fn (X-Geom-height geom1) (X-Geom-height geom2))
	       :border-width (funcall fn (X-Geom-border-width geom1) (X-Geom-border-width geom2))))

(defun X-Geom-sum (geom1 geom2)
  "Create new geometry which elements is sum of corresponded elements of GEOM1 and GEOM2."
  (X-Geom-apply '+ geom1 geom2))

(defun X-Geom-sub (geom1 geom2)
  (X-Geom-apply '- geom1 geom2))

(defun X-Geom-width-with-borders (geom)
  "Return GEOM width including border's width."
  (+ (X-Geom-width geom)
     (* 2 (X-Geom-border-width geom))))

(defun X-Geom-height-with-borders (geom)
  "Return GEOM height including border's width."
  (+ (X-Geom-height geom)
     (* 2 (X-Geom-border-width geom))))

;;; X-Geom <--> X-Rect conversation
(defun X-Geom-to-X-Rect (xgeom)
  "Convert XGEOM to X-Rect."
  (make-X-Rect :x (X-Geom-x xgeom)
	       :y (X-Geom-y xgeom)
	       :width (X-Geom-width xgeom)
	       :height (X-Geom-height xgeom)))

(defun X-Rect-to-X-Geom (xrect)
  "Convert XRECT to X-Geom."
  (make-X-Geom :x (X-Rect-x xrect)
	       :y (X-Rect-y xrect)
	       :width (X-Rect-width xrect)
	       :height (X-Rect-height xrect)))


(defstruct (X-Arc (:type vector) :named
                  (:include X-Rect)
		  (:predicate X-Arc-isarc-p))
  angle1 angle2)

(defsubst X-Arc-p (xarc &optional sig)
  "Return non-nil xf XARC is X-Arc structure."
  (X-Generic-p 'X-Arc 'X-Arc-isarc-p xarc sig))


(defstruct (X-Cursor (:predicate X-Cursor-iscursor-p))
  dpy id
  source mask
  src-char msk-char
  fgred fggreen fgblue
  bgred bggreen bgblue)

(defun X-Cursor-find-or-make (dpy id)
  (make-X-Cursor :dpy dpy :id id))


(defstruct X-EventHandler
  priority
  evtypes-list                          ; list of event types
  handler                               ; function to call
  (active t)                            ; Non-nil mean event handler activated

  plist)                                ; user defined plist

;;;###autoload
(defun X-EventHandler-add (evhlist handler &optional priority evtypes-list)
  "To event handlers list EVHLIST add event HANDLER.

HANDLER is function which should accept three arguments - xdpy(X-Dpy),
xwin(X-Win) and xev(X-Event).  Only events with type that in
EVTYPES-LIST are passed to HANDLER. By default all events passed.
PRIORITY is place in events handler list, i.e. when HANDLER will be
called. Higher priorities runs first.

Return new list, use it like `(setq lst (X-EventHandler-add lst 'handler))'."
  (unless priority
    (setq priority 0))

  (let ((xeh (make-X-EventHandler :priority priority
                                  :evtypes-list evtypes-list
                                  :handler handler)))

    ;; Insert new event handler and sort event handlers by priority.
    (sort (cons xeh evhlist)
          #'(lambda (xeh1 xeh2)
              (> (X-EventHandler-priority xeh1)
                 (X-EventHandler-priority xeh2))))))

;;;###autoload
(defun X-EventHandler-isset (evhlist handler &optional prioritiy evtypes-list)
  "Examine EVHLIST and return X-EventHandler with HANDLER, PRIORITY and EVTYPES-LIST.
If you does not specify PRIORITY and EVTYPES-LIST, only matching with HANDLER occurs.
If event handler not found - nil will be returned."
  (let ((evhs evhlist))
    ;; Find appopriate handler
    (while (and evhs
                (not (and (eq (X-EventHandler-handler (car evhs)) handler)
                          (if prioritiy (equal prioritiy (X-EventHandler-priority (car evhs))) t)
                          (if evtypes-list (equal evtypes-list (X-EventHandler-evtypes-list (car evhs))) t))))
      (setq evhs (cdr evhs)))

    (car evhs)))

;;;###autoload
(defun X-EventHandler-rem (evhlist handler &optional prioritiy evtypes-list)
  "From EVHLIST remove event HANDLER with PRIORITY and EVTYPES-LIST.
If you does not specify PRIORITY and EVTYPES-LIST, only matching with HANDLER occurs.
Return new list, use it like `(setq lst (X-EventHandler-rem lst 'handler))'."
  (let ((xeh (X-EventHandler-isset evhlist handler prioritiy evtypes-list)))
    (when xeh
      (setq evhlist (delete xeh evhlist)))
    evhlist))

;;;###autoload
(defun X-EventHandler-enable (evhlist handler &optional prioritiy evtypes-list)
  "In event handlers list EVHLIST mark HANDLER with PRIORITY and EVTYPES-LIST as active."
  (let ((xeh (X-EventHandler-isset evhlist handler prioritiy evtypes-list)))
    (when xeh
      (setf (X-EventHandler-active xeh) t))))

;;;###autoload
(defun X-EventHandler-disable (evhlist handler &optional prioritiy evtypes-list)
  "In event handlers list EVHLIST mark HANDLER with PRIORITY and EVTYPES-LIST as inactive."
  (let ((xeh (X-EventHandler-isset evhlist handler prioritiy evtypes-list)))
    (when xeh
      (setf (X-EventHandler-active xeh) nil))))

;;;###autoload
(defun X-EventHandler-runall (evhlist xev)
  "Run all event handlers in EVHLIST on XEV.
Signal `X-Events-stop' to stop events processing."
  (let ((evhs evhlist))                 ; EVHS should be already sorted by priority
    (condition-case nil
        (while evhs
          ;; Check is there appopriate event handler to handle XEV event.
          (when (and (X-EventHandler-active (car evhs))
                     (or (null (X-EventHandler-evtypes-list (car evhs)))
                         (memq (X-Event-type xev) (X-EventHandler-evtypes-list (car evhs)))))
            (funcall (X-EventHandler-handler (car evhs)) (X-Event-dpy xev) (X-Event-win xev) xev))
          (setq evhs (cdr evhs)))
      (X-Events-stop nil))))


(defstruct (X-Win (:predicate X-Win-iswin-p)
                  (:print-function (lambda (xwin s pl)
                                     (princ (format "#<X-Win id=0x%x>"
                                                    (X-Win-id xwin)) s))))
  dpy id
  event-handlers			; list of X-EventHandler
  plist)				; user defined plist

;; Properties list operations
(defsubst X-Win-put-prop (win prop val)
  (setf (X-Win-plist win) (plist-put (X-Win-plist win) prop val)))

(defsubst X-Win-get-prop (win prop)
  (plist-get (X-Win-plist win) prop))

(defsubst X-Win-rem-prop (win prop)
  (setf (X-Win-plist win) (plist-remprop (X-Win-plist win) prop)))

(defsubst X-Win-equal (win1 win2)
  "Return non-nil if id's of WIN1 and WIN2 are equal."
  (equal (X-Win-id win1) (X-Win-id win2)))

(defsubst X-Win-EventHandler-add (win handler &optional priority evtypes-list)
  "To X-Win add events HANDLER.

HANDLER is function which should accept three arguments - xdpy(X-Dpy),
xwin(X-Win) and xev(X-Event).  Only events with type that in
EVTYPES-LIST are passed to HANDLER. By default all events passed.
PRIORITY is place in events handler list, i.e. when HANDLER will be
called. Higher priorities runs first."
  (setf (X-Win-event-handlers win)
	(X-EventHandler-add (X-Win-event-handlers win) handler priority evtypes-list)))

(defsubst X-Win-EventHandler-isset (win handler &optional priority evtypes-list)
  "For WIN's event handlers return X-EventHandler with HANDLER, PRIORITY and EVTYPES-LIST.
If you does not specify PRIORITY and EVTYPES-LIST, only matching with HANDLER occurs.
If event handler not found - nil will be returned."
  (X-EventHandler-isset (X-Win-event-handlers win) handler priority evtypes-list))

(defsubst X-Win-EventHandler-add-new (win handler &optional priority evtypes-list)
  "To X-Win add events HANDLER, only if no such handler already installed.

HANDLER is function which should accept three arguments - xdpy(X-Dpy),
xwin(X-Win) and xev(X-Event).  Only events with type that in
EVTYPES-LIST are passed to HANDLER. By default all events passed.
PRIORITY is place in events handler list, i.e. when HANDLER will be
called. Higher priorities runs first."
  (unless (X-Win-EventHandler-isset win handler priority evtypes-list)
    (setf (X-Win-event-handlers win)
	  (X-EventHandler-add (X-Win-event-handlers win) handler priority evtypes-list))))

(defsubst X-Win-EventHandler-rem (win handler &optional priority evtypes-list)
  "From WIN's events handlers remove event HANDLER with PRIORITY and EVTYPES-LIST.
If you does not specify PRIORITY and EVTYPES-LIST, only matching with HANDLER occurs."
  (setf (X-Win-event-handlers win)
	(X-EventHandler-rem (X-Win-event-handlers win) handler priority evtypes-list)))

(defsubst X-Win-EventHandler-enable (win handler &optional priority evtypes-list)
  "In WIN's event handlers list mark HANDLER with PRIORITY and EVTYPES-LIST as active."
  (X-EventHandler-enable (X-Win-event-handlers win) handler priority evtypes-list))

(defsubst X-Win-EventHandler-disable (win handler &optional priority evtypes-list)
  "In WIN's event handlers list mark HANDLER with PRIORITY and EVTYPES-LIST as inactive."
  (X-EventHandler-disable (X-Win-event-handlers win) handler priority evtypes-list))

(defsubst X-Win-EventHandler-runall (win xev)
  "Run all WIN's event handlers on XEV.
Signal `X-Events-stop' to stop events processing."
  (X-EventHandler-runall (X-Win-event-handlers win) xev))


(defstruct (X-Pixmap (:predicate X-Pixmap-ispixmap-p))
  dpy id d
  plist)				; User defined plist

(defun X-Pixmap-find-or-make (dpy id)
  (make-X-Pixmap :dpy dpy :id id))

;; Properties list operations
(defsubst X-Pixmap-put-prop (pixmap prop val)
  (setf (X-Pixmap-plist pixmap) (plist-put (X-Pixmap-plist pixmap) prop val)))

(defsubst X-Pixmap-get-prop (pixmap prop)
  (plist-get (X-Pixmap-plist pixmap) prop))

(defsubst X-Pixmap-rem-prop (pixmap prop)
  (setf (X-Pixmap-plist pixmap) (plist-remprop (X-Pixmap-plist pixmap) prop)))

(defsubst X-Pixmap-width (pixmap)
  "Return PIXMAP's width."
  (or (X-Pixmap-get-prop pixmap 'width)
      (let ((gm (XGetGeometry (X-Pixmap-dpy pixmap) pixmap)))
        (X-Pixmap-put-prop pixmap 'width (X-Geom-width gm))
        (X-Pixmap-put-prop pixmap 'height (X-Geom-height gm))
        (X-Geom-width gm))))

(defsetf X-Pixmap-width (pixmap) (nw)
  `(X-Pixmap-put-prop ,pixmap 'width ,nw))

(defsubst X-Pixmap-height (pixmap)
  "Return PIXMAP's height."
  (or (X-Pixmap-get-prop pixmap 'height))
      (let ((gm (XGetGeometry (X-Pixmap-dpy pixmap) pixmap)))
        (X-Pixmap-put-prop pixmap 'width (X-Geom-width gm))
        (X-Pixmap-put-prop pixmap 'height (X-Geom-height gm))
        (X-Geom-height gm)))

(defsetf X-Pixmap-height (pixmap) (nh)
  `(X-Pixmap-put-prop ,pixmap 'height ,nh))

;;;
;; DRAWABLE stuff.  A drawable is something you can draw to,
;; therefore, the only fn we need, is a drawable-p function.
;;
;; Each time we make a new drawable surface, add that to the list
;; of checks here!
;;
(defun X-Drawable-p (d &optional sig)
  "Return non-nil if D is drawable.
If SIG, then signal on error."
  (let ((isdp (or (X-Win-p d) (X-Pixmap-p d))))
    (if (and sig (not isdp))
	(signal 'wrong-type-argument (list sig 'X-Drawable-p d))
      isdp)))

(defun X-Drawable-id (d)
  "Return id of drawable D."
  (X-Drawable-p d 'X-Drawable-id)

  (if (X-Win-p d)
      (X-Win-id d)
    (X-Pixmap-id d)))

(defun X-Drawable-dpy (d)
  "Return dpy of drawable D."
  (X-Drawable-p d 'X-Drawable-dpy)

  (if (X-Win-p d)
      (X-Win-dpy d)
    (X-Pixmap-dpy d)))

;; Common predicates
(defun X-Generic-p (type pfunc thing &optional sig)
  "Returns non-nil if THING is of TYPE, using predicate PFUNC.
If SIG is given, then signal if error."

  (let ((isit (funcall pfunc thing)))
    (if (and (not isit) sig)
	(signal 'wrong-type-argument (list sig type thing))
      isit)))

(defun X-Win-p (win &optional sig)
  "Return non-nil if WIN is X-Win structure.
If SIG is given and WIN is not X-Win structure, SIG will
be signaled."
  (X-Generic-p 'X-Win 'X-Win-iswin-p win sig))

(defun X-Pixmap-p (pixmap &optional sig)
  "Return non-nil if PIXMAP is X-Pixmap structure.
If SIG is given and PIXMAP is not X-Pixmap structure, SIG will be signaled."
  (X-Generic-p 'X-Pixmap 'X-Pixmap-ispixmap-p pixmap sig))


(provide 'xlib-common)

;;; xlib-common.el ends here
