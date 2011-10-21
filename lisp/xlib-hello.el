;;; xlib-hello.el --- Hello world example using new xlib.

;; Copyright (C) 2003-2005 by XWEM Org.

;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;; Modified: Zajcev Evgeny <zevlg@yandex.ru>
;; Keywords: xlib
;; X-CVS: $Id: xlib-hello.el,v 1.5 2004/11/29 19:48:18 lg Exp $

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

;; This program very close to first hello program that Eric Ludlam
;; wrote at least arcs, text and lines drawing did not changed, only
;; buttons added.

;; Many hard codes, do not use as real example, this hello world
;; application needed only to show that xlib works, not as
;; recommendation how to write applications that uses xlib.

;;; Code:

(require 'xlib-xlib)
(require 'xlib-xpm)

(defconst XH-event-mask
  (Xmask-or XM-Exposure XM-StructureNotify XM-KeyPress XM-KeyRelease
            XM-ButtonPress XM-ButtonRelease))

(defvar XH-gc-1 nil)
(defvar XH-gc-2 nil)
(defvar XH-win nil)

(defvar XH-buttons nil "List of buttons.")
(defvar XH-close-buttons nil "List of buttons to dismiss.")

(defun XX-Hello (dname)
  (interactive "sDisplay: ")

  (let ((xdpy (XOpenDisplay dname)))
    (if (not (X-Dpy-p xdpy))
	(message "Can't open display '%s'" dname)

      (X-Dpy-set-log-routines
       xdpy '(x-misc x-event x-tray x-error x-record))
      (setf (X:Dpy-log-buffer xdpy) "Xlog")
      (let ((w (XCreateWindow xdpy nil 20 20 100 100 4 nil nil nil
			      :override-redirect t
                              :background-pixel (XWhitePixel xdpy)
                              :border-pixel (XBlackPixel xdpy)
                              :event-mask XH-event-mask)))
	(if (not (X-Win-p w))
	    (progn
	      (message "Can't create window.")
	      (XBell xdpy 100))

	  (setq XH-gc-1 (XCreateGC xdpy w
                                   :foreground (XAllocNamedColor xdpy (XDefaultColormap xdpy) "Red")
                                   :background (XWhitePixel xdpy)
                                   :line-style X-LineSolid
                                   :line-width 1)
                XH-gc-2 (XCreateGC xdpy w
                                   :foreground (XAllocNamedColor xdpy (XDefaultColormap xdpy) "Green")
                                   :background (XWhitePixel xdpy)
                                   :line-style X-LineDoubleDash
                                   :line-width 2))

	  ;; Setup events handling
	  (X-Win-EventHandler-add w 'XH-events-handler)

	  (XMapWindow xdpy w)
          (X:Flush xdpy)
	  (setq XH-win w))))))

(defun XH-button-press (xdpy win xev)
  "Button press event."
  (let ((x (X-Event-xbutton-event-x xev))
	(y (X-Event-xbutton-event-y xev)))
    (cond ((and (>= x 20)
                (<= x (+ 20 (X-Image-width (nth 1 XH-buttons))))
                (>= y 20)
                (<= y (+ 20 (X-Image-height (nth 1 XH-buttons)))))
           (XH-show-button xdpy win 20 20 1)
           (message "Hellow world!"))

          ((and (>= x 20)
                (<= x (+ 20 (X-Image-width (nth 1 XH-close-buttons))))
                (>= y 60)
                (<= y (+ 60 (X-Image-height (nth 1 XH-close-buttons)))))

           (XH-show-close-button xdpy win 20 60 1)
           (message "XH: Exiting  ..")
           (XSelectInput xdpy win 0)
           (XDestroyWindow xdpy win))
          )
    ))

(defun XH-button-release (xdpy win xev)
  "Button release event."
  (XH-show-button xdpy win 20 20 0))

(defun XH-expose (xdpy w xev)
  "Expose."
  (XDrawLine xdpy w XH-gc-2 5 5 100 50)
  (XDrawPoint xdpy w XH-gc-1 20 5)
  (XFillRectangle xdpy w XH-gc-2 2 38 38 15)
  (XDrawRectangle xdpy w XH-gc-1 2 38 38 15)
  (XDrawString xdpy w XH-gc-1 5 50 "HELLO!")
  (XDrawSegments xdpy w XH-gc-2 (list (cons '(100 . 0) '(50 . 10))
				      (cons '(100 . 100) '(50 . 90))))
  (XDrawArc xdpy w XH-gc-1 50 50 20 20 0 360)
  (XFillArc xdpy w XH-gc-2 55 55 10 10 0 360)
  
  ;; Show 'Press me' button
  (XH-show-button xdpy w 20 20 0)

  ;; Show 'Dismiss' button
  (XH-show-close-button xdpy w 20 60 0)
  )

(defun XH-events-handler (xdpy w xev)
  "X hello events dispatcher."
  (X-Event-CASE xev
    (:X-Expose
     (message "XH got Exposure event ..")
     (XH-expose xdpy w xev))

    (:X-KeyPress
     (message "XH got KeyPress event .."))

    (:X-KeyRelease
     (message "XH got KeyRelease event .."))

    (:X-ButtonPress
     (message "XH got ButtonPress event ..")
     (XH-button-press xdpy w xev))

    (:X-ButtonRelease
     (message "XH got ButtonRelease event ..")
     (XH-button-release xdpy w xev))

    (:X-DestroyNotify
     (XCloseDisplay xdpy)
     (setq XH-win nil
           XH-buttons nil
           XH-close-buttons nil))

    (t (message "XH Got event: %d" (X-Event-type xev)))))

(defun XH-show-button (dpy win x y &optional state)
  "Show 'Press Me' button."
  (unless XH-buttons
    ;; Fill
    (require 'xpm-button)
    (let ((buts (xpm-button-create "Press Me" 2 "green4" "#a0d0a0")))
      (setq XH-buttons
	    (mapcar (lambda (but)
		      (X:xpm-img-from-data dpy (aref but 2)))
		    buts))))
  
  (unless state
    (setq state 0))

  (XImagePut dpy (XDefaultGC dpy) win x y (nth state XH-buttons)))

(defun XH-show-close-button (dpy win x y &optional state)
  "Show 'Dismiss' button."
  (unless XH-close-buttons
    (require 'xpm-button)
    (let ((buts (xpm-button-create "Dismiss" 4 "Red4" "gray80")))
      (setq XH-close-buttons
            (mapcar (lambda (but)
                      (X:xpm-img-from-data dpy (aref but 2)))
                    buts))))

  (unless state
    (setq state 0))
  
  (XImagePut dpy (XDefaultGC dpy) win x y (nth state XH-close-buttons)))


(provide 'xlib-hello)

;;; xlib-hello.el ends here
