;;; xlib-xinerama.el --- Bindings to XINERAMA extension calls.

;; Copyright (C) 2005-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Fri Jan 13 03:05:19 UTC 2006
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

;; 

;;; Code:

(require 'xlib-xlib)
(or (ffi-load-library "/usr/X11/lib/libXinerama")
    (ffi-load "libXinerama"))

(define-ffi-struct XineramaScreenInfo
  (screen_number int)
  (x_org short)
  (y_org short)
  (width short)
  (height short))

(define-xlib-function X:ineramaQueryVersion int
  (dpy (pointer Display))
  (major (pointer int))
  (minor (pointer int)))

(define-xlib-function X:ineramaIsActive Bool
  (dpy (pointer Display)))

(define-xlib-function X:ineramaQueryScreens (pointer XineramaScreenInfo)
  (dpy (pointer Display))
  (number (pointer int)))

(defun X-XIneramaQueryVersion (xdpy &optional major minor)
  "On display XDPY query for version of XInerama extension."
  (let ((mav (make-ffi-object 'int))
        (miv (make-ffi-object 'int)))
    (unless (zerop (X:ineramaQueryVersion
                    xdpy (ffi-address-of mav) (ffi-address-of miv)))
      (list t nil (ffi-get mav) (ffi-get miv)))))

(defun X-XIneramaIsActive (xdpy)
  "Return non-nil if XINERAMA is active."
  (not (zerop (X:ineramaIsActive xdpy))))

(defun X-XIneramaQueryScreens (xdpy)
  "On display XDPY query for XINERAMA screens."
  (let* ((sn (make-ffi-object 'int))
         (xsip (X:ineramaQueryScreens xdpy (ffi-address-of sn))))
    (unless (ffi-null-p xsip)
      (cons t (prog1
                  (loop for id from 0 below (ffi-get sn)
                    collect (let ((off (+ (* id (ffi-size-of-type 'XineramaScreenInfo))
                                          (ffi-size-of-type 'int))))
                              (make-X-Rect :x (ffi-fetch xsip off 'short)
                                           :y (ffi-fetch xsip (+ off (ffi-size-of-type 'short)) 'short)
                                           :width (ffi-fetch xsip (+ off (* 2 (ffi-size-of-type 'short))) 'short)
                                           :height (ffi-fetch xsip (+ off (* 3 (ffi-size-of-type 'short))) 'short))))
                (X:Free xsip))))))

(provide 'xlib-xinerama)

;;; xlib-xinerama.el ends here
