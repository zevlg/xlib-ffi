;;; xlib-xpm.el --- FFI for libXpm.

;; Copyright (C) 2005-2007 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Sun Aug 28 02:20:45 MSD 2005
;; Keywords: xlib, ffi

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
;; along with SXEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; Features:
;;   - X:xpm-img-from-file
;;   - X:xpm-pixmap-from-file
;;   - X:xpm-img-from-data
;;   - X:xpm-pixmap-from-data
;;   - X:xpm-color-symbols

;;; Code:

(require 'xlib-xlib)
(or (ffi-load-library "/usr/X11/lib/libXpm")
    (ffi-load "libXpm"))

(defvar X:xpm-color-symbols nil
  "Same as `xpm-color-symbols', but for xlib.")

(define-ffi-struct XpmColorSymbol
  (name (pointer char))
  (value (pointer char))
  (pixel unsigned-long))

(define-ffi-struct XpmExtension
  (name (pointer char))
  (nlines unsigned-int)
  (lines (pointer (pointer char))))

(define-ffi-struct XpmColor
  (string (pointer char))
  (symbolic (pointer char))
  (m (pointer char))
  (g4 (pointer char))
  (g (pointer char))
  (c (pointer char)))

(define-ffi-struct XpmAttributes
  (valuemask unsigned-long)
  (visual (pointer Visual))
  (colormap Colormap)
  (depth unsigned-int)
  (width unsigned-int)
  (height unsigned-int)
  (x_hotspot unsigned-int)
  (y_hotspot unsigned-int)
  (cpp unsigned-int)
  (pixels (pointer Pixel))
  (npixels unsigned-int)
  (colorsymbols (pointer XpmColorSymbol))
  (numsymbols unsigned-int)

  (rgb_fname (pointer char))
  (nextensions unsigned-int)
  (extensions (pointer XpmExtension))
  (ncolors unsigned-int)
  (colorTable (pointer XpmColor))
  (hints_cmt (pointer char))
  (colors_cmt (pointer char))
  (pixels_cmt (pointer char))
  (mask_pixel unsigned-int)
  (exactColors Bool)
  (closeness unsigned-int)
  (red_closeness unsigned-int)
  (green_closeness unsigned-int)
  (blue_closeness unsigned-int)
  (color_key int)
  (alloc_pixels (pointer Pixel))
  (nalloc_pixels int)
  (alloc_close_colors Bool)
  (bitmap_format int)
  (alloc_color (pointer void))
  (free_colors (pointer void))
  (color_closure (pointer void))
  )

(defconst X:pmColorSymbols 64)

(define-xlib-function X:pmReadFileToImage int
  (dpy (pointer Display))
  (filename c-string)
  (image_ret (pointer (pointer XImage)))
  (shapeimage_ret (pointer (pointer XImage)))
  (attributes (pointer XpmAttributes)))

(define-xlib-function X:pmReadFileToPixmap int
  (dpy (pointer Display))
  (d Drawable)
  (filename c-string)
  (pixmap_ret (pointer Pixmap))
  (shapepixmap_ret (pointer Pixmap))
  (attributes (pointer XpmAttributes)))

(define-xlib-function X:pmWriteFileFromImage int
  (dpy (pointer Display))
  (filename c-string)
  (image_ret (pointer (pointer XImage)))
  (shapeimage_ret (pointer (pointer XImage)))
  (attributes (pointer XpmAttributes)))

(define-xlib-function X:pmCreateImageFromData int
  (dpy (pointer Display))
  (data (pointer (pointer char)))
  (image_ret (pointer (pointer XImage)))
  (shapeimage_ret (pointer (pointer XImage)))
  (attributes (pointer XpmAttributes)))

(define-xlib-function X:pmCreatePixmapFromData int
  (display (pointer Display))
  (d Drawable)
  (data (pointer (pointer char)))
  (pixmap_return (pointer Pixmap))
  (shapemask_return (pointer Pixmap))
  (attributes (pointer XpmAttributes)))

(define-xlib-function X:pmFreeAttributes void
  (attributes (pointer XpmAttributes)))

(define-xlib-function X:pmFree void
  (ptr (pointer void)))

;; Same as in non-ffi xlib-xpm.el, should sheep to xlib-common.el?
(defun X:xpm-get-symcolor (symc-name &optional tag-set)
  "Get SYMC-NAME color from `X:xpm-color-symbols' list.
TAGS-SET is a list of tags, directly passed to `specifier-spec-list'."
  (let ((xcs (eval (cadr (assoc symc-name X:xpm-color-symbols)))))
    (cond ((null xcs) nil)
          ((stringp xcs) xcs)
          ((specifierp xcs)
           (let ((sspec (specifier-spec-list xcs nil tag-set)))
             (cdr (car (cdr (car sspec))))))
          (t (error "Invalid element in `X:xpm-color-symbols'")))))

(defun X:xpm-allocate-attributes (xdpy)
  "Allocate xpm attributes with `X:xpm-color-symbols'."
  (when X:xpm-color-symbols
    (let ((attrs (make-ffi-object 'XpmAttributes))
          (symcols (make-ffi-object
                    `(array XpmColorSymbol ,(length X:xpm-color-symbols)))))
      ;; Fill up symcols
      (loop for it from 0 below (length X:xpm-color-symbols)
        do
        (let* ((col (nth it X:xpm-color-symbols))
               (col-name (car col))
               (col-val (X:xpm-get-symcolor col-name)))
          (ffi-store symcols (+ (* it (ffi-size-of-type 'XpmColorSymbol))
                                (ffi-slot-offset 'XpmColorSymbol 'name))
                     '(pointer char)
                     (ffi-create-fo 'c-string col-name))
          (ffi-store symcols (+ (* it (ffi-size-of-type 'XpmColorSymbol))
                                (ffi-slot-offset 'XpmColorSymbol 'value))
                     '(pointer char)
                     (ffi-null-pointer))
          (when col-val
            (ffi-store symcols (+ (* it (ffi-size-of-type 'XpmColorSymbol))
                                  (ffi-slot-offset 'XpmColorSymbol 'pixel))
                       'unsigned-long
                       (XAllocNamedColor
                        xdpy (XDefaultColormap xdpy) col-val)))
          ))

      ;; Setup attributes
      (setf (XpmAttributes->valuemask attrs) X:pmColorSymbols
            (XpmAttributes->numsymbols attrs) (length X:xpm-color-symbols)
            (XpmAttributes->colorsymbols attrs) symcols)
      attrs)))

(defun X:xpm-img-from-file (xdpy file &optional shape tag-set)
  (let* ((xi (make-ffi-object '(pointer XImage)))
         (xmi (make-ffi-object '(pointer XImage)))
         (aatr (X:xpm-allocate-attributes xdpy))
         (attrs (if aatr (ffi-address-of aatr) (ffi-null-pointer))))
    (X:pmReadFileToImage
     xdpy file (ffi-address-of xi) (ffi-address-of xmi) attrs)
    (unless (ffi-null-p attrs)
      (X:pmFreeAttributes attrs))
    (if shape xmi xi)))


(defun X:xpm-pixmap-from-file (xdpy d file &optional shape tag-set)
  (let* ((xp (make-ffi-object 'Pixmap))
         (xmp (make-ffi-object 'Pixmap))
         (aatr (X:xpm-allocate-attributes xdpy))
         (attrs (if aatr (ffi-address-of aatr) (ffi-null-pointer))))
    (X:pmReadFileToPixmap
     xdpy (X-Drawable-id d) file
     (ffi-address-of xp) (ffi-address-of xmp) attrs)
    (unless (ffi-null-p attrs)
      (X:pmFreeAttributes attrs))
    (X-Pixmap-find-or-make xdpy (ffi-get (if shape xmp xp)))))

(defun X:extract-data-internal (data-string)
  (let* ((ds (delq
              nil
              (mapcar #'(lambda (ss)
                          (when (string-match
                                 "^[ \t]*\"\\([^\"]*\\)\"[ \t]*,?[ \t]*$" ss)
                            (substring ss (match-beginning 1) (match-end 1))))
                      (split-string data-string "\n" t))))
         (fd (make-ffi-object '(pointer (pointer char))
                              (* (1+ (length ds))
                                 (ffi-size-of-type '(pointer char)))))
         (idx 0))
    (while ds
      (ffi-aset fd idx (ffi-create-fo 'c-string (car ds)))
      (incf idx)
      (setq ds (cdr ds)))
    (ffi-aset fd idx (ffi-null-pointer))
    fd))
  
(defun X:xpm-img-from-data (xdpy data &optional shape tag-set)
  (let* ((xi (make-ffi-object '(pointer XImage)))
         (xmi (make-ffi-object '(pointer XImage)))
         (aatr (X:xpm-allocate-attributes xdpy))
         (attrs (if aatr (ffi-address-of aatr) (ffi-null-pointer))))
    (X:pmCreateImageFromData
     xdpy (X:extract-data-internal data)
     (ffi-address-of xi) (ffi-address-of xmi) attrs)
    (unless (ffi-null-p attrs)
      (X:pmFreeAttributes attrs))
    (if shape xmi xi)))

(defun X:xpm-pixmap-from-data (xdpy d data &optional shape tag-set)
  (let* ((xp (make-ffi-object 'Pixmap))
         (xmp (make-ffi-object 'Pixmap))
         (aatr (X:xpm-allocate-attributes xdpy))
         (attrs (if aatr (ffi-address-of aatr) (ffi-null-pointer))))
    (X:pmCreatePixmapFromData
     xdpy (X-Drawable-id d) (X:extract-data-internal data)
     (ffi-address-of xp) (ffi-address-of xmp) attrs)
    (unless (ffi-null-p attrs)
      (X:pmFreeAttributes attrs))
    (X-Pixmap-find-or-make xdpy (ffi-get (if shape xmp xp)))))

(provide 'xlib-xpm)

;;; xlib-xpm.el ends here
