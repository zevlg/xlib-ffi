;;; xlib-test.el --- Bindings to XTEST extension calls.

;; Copyright (C) 2005 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Fri Jan 13 02:48:19 UTC 2006
;; Keywords: xlib

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

;; Only KeyPress/KeyRelease events emulation supported.

;;; Code:
(require 'xlib-xlib)

(or (ffi-load-library "/usr/X11/lib/libXtst")
    (ffi-load "libXtst"))

(defconst X-XTest-op-GetVersion	0)
(defconst X-XTest-op-CompareCursor 1)
(defconst X-XTest-op-FakeInput 2)
(defconst X-XTest-op-GrabControl 3)

(defconst X-Xtest-KeyPress 2)
(defconst X-Xtest-KeyRelease 3)
(defconst X-Xtest-ButtonPress 4)
(defconst X-Xtest-ButtonRelease 5)
(defconst X-Xtest-MotionNotify 6)

(define-xlib-function X:TestFakeKeyEvent void
  (dpy (pointer Display))
  (keycode unsigned-int)
  (is_press Bool)
  (delay unsigned-long))

(define-xlib-function X:TestFakeButtonEvent void
  (dpy (pointer Display))
  (button unsigned-int)
  (is_press Bool)
  (delay unsigned-long))

(defun-x11 X-XTest-FakeInput (xdpy evtype detail &optional root rootx rooty time)
  "On display XDPY send fake event of EVTYPE with DETAIL after TIME delay.
If TIME is X-CurrentTime - fake the event with no delay.
EVTYPE is one of X-Xtest-KeyPress or X-Xtest-KeyRelease, other event
types are not supported."
  (X-Dpy-p xdpy 'X-XTest-FakeInput)
  (unless time (setq time X-CurrentTime))
  (cond ((memq evtype (list X-Xtest-KeyPress X-Xtest-KeyRelease))
         (X:TestFakeKeyEvent xdpy detail (if (= evtype X-Xtest-KeyPress)
                                             X-True X-False)
                             time))
        ((memq evtype (list X-Xtest-ButtonPress X-Xtest-ButtonRelease))
         (X:TestFakeButtonEvent xdpy detail (if (= evtype X-Xtest-ButtonPress)
                                                X-True X-False)
                                time))
        (t (error "Unsupported event type" evtype))))

(provide 'xlib-xtest)

;;; xlib-test.el ends here
