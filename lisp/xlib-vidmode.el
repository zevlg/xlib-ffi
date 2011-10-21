;;; xlib-vidmode.el --- VidMode extension.

;; Copyright (C) 2005-2006 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Sep  8 00:20:15 MSD 2005
;; Keywords: ffi, xlib

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
(or (ffi-load-library "/usr/X11/lib/libXxf86vm")
    (ffi-load "libXxf86vm"))

(define-ffi-struct XF86VidModeGamma
  (red float)
  (green float)
  (blue float))

(define-xlib-function X:F86VidModeQueryVersion int
  (dpy (pointer Display))
  (major (pointer int))
  (minor (pointer int)))

(define-xlib-function X:F86VidModeGetGamma Bool
  (dpy (pointer Display))
  (screen int)
  (gamma (pointer XF86VidModeGamma)))

(define-xlib-function X:F86VidModeSetGamma Bool
  (dpy (pointer Display))
  (screen int)
  (gamma (pointer XF86VidModeGamma)))

(defun X-XF86VidModeQueryVersion (xdpy)
  "On display XDPY query for version of XF86VidMode extension."
  (let ((mav (make-ffi-object 'int))
        (miv (make-ffi-object 'int)))
    (unless (zerop (X:F86VidModeQueryVersion
                    xdpy (ffi-address-of mav) (ffi-address-of miv)))
      (list t nil (ffi-get mav) (ffi-get miv)))))

(defun X-XF86VidModeGetGamma (xdpy &optional screen-num)
  "On display XDPY using XF86VidMode extension fetch current gamma."
  (let ((gamma (make-ffi-object 'XF86VidModeGamma)))
    (unless (zerop (X:F86VidModeGetGamma
                    xdpy (or screen-num (Display->default_screen xdpy))
                    (ffi-address-of gamma)))
      (list t nil (XF86VidModeGamma->red gamma) (XF86VidModeGamma->green gamma)
            (XF86VidModeGamma->blue gamma)))))

(defun X-XF86VidModeSetGamma (xdpy r g b &optional screen-num)
  "On display XDPY using XF86VidMode extension fetch current gamma."
  (let ((gamma (make-ffi-object 'XF86VidModeGamma)))
    (setf (ffi-slot gamma 'red) r
          (ffi-slot gamma 'green) g
          (ffi-slot gamma 'blue) b)
    (X:F86VidModeSetGamma xdpy (or screen-num (Display->default_screen xdpy))
                          (ffi-address-of gamma))))

(provide 'xlib-vidmode)

;;; xlib-vidmode.el ends here
