;;; xlib-xshape.el --- Bindings to SHAPE extension calls.

;; Copyright (C) 2005-2010 by Zajcev Evgeny <zevlg@yandex.ru>.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Sep  8 00:16:48 MSD 2005
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

;; ops
(defconst X-XShapeSet 0)
(defconst X-XShapeUnion 1)
(defconst X-XShapeIntersect 2)
(defconst X-XShapeSubtract 3)
(defconst X-XShapeInvert 4)

;; kinds
(defconst X-XShape-Bounding 0)
(defconst X-XShape-Clip 1)

;; events
(defconst X-ShapeNotify 0)		; actuallly (0 + extension event base)

(define-xlib-function X:ShapeQueryVersion int
  (dpy (pointer Display))
  (major (pointer int))
  (minor (pointer int)))

(define-xlib-function X:ShapeCombineRectangles void
  (dpy (pointer Display))
  (dest XID)
  (destKind int)
  (xOff int)
  (yOff int)
  (rects (pointer XRectangle))
  (n_rects int)
  (op int)
  (ordering int))

(define-xlib-function X:ShapeCombineMask void
  (dpy (pointer Display))
  (dest XID)
  (destKind int)
  (xOff int)
  (yOff int)
  (src Pixmap)
  (op int))

(define-xlib-function X:ShapeCombineShape void
  (dpy (pointer Display))
  (dest XID)
  (destKind int)
  (xOff int)
  (yOff int)
  (src Pixmap)
  (srcKind int)
  (op int))

(define-xlib-function X:ShapeOffsetShape void
  (dpy (pointer Display))
  (dest XID)
  (destKind int)
  (xOff int)
  (yOff int))

(define-xlib-function X:ShapeQueryExtents int
  (dpy (pointer Display))
  (window (pointer Window))
  (bShaped (pointer int))
  (xbs (pointer int))
  (ybs (pointer int))
  (wbs (pointer unsigned-int))
  (hbs (pointer unsigned-int))
  (cShaped (pointer int))
  (xcs (pointer int))
  (ycs (pointer int))
  (wcs (pointer unsigned-int))
  (hcs (pointer unsigned-int)))

(define-xlib-function X:ShapeSelectInput void
  (dpy (pointer Display))
  (window Window)
  (longmask unsigned-int))

(define-xlib-function X:ShapeInputSelected unsigned-long
  (dpy (pointer Display))
  (window Window))

(define-xlib-function X:ShapeGetRectangles (pointer XRectangle)
  (dpy (pointer Display))
  (window Window)
  (kind int)
  (count (pointer int))
  (ordering (pointer int)))


(defun X-XShapeQueryVersion (xdpy)
  (let ((mav (make-ffi-object 'int))
        (miv (make-ffi-object 'int)))
    (when (zerop (ffi-get (X:ShapeQueryVersion xdpy (ffi-address-of mav) (ffi-address-of miv))))
      (list t nil (ffi-get mav) (ffi-get miv)))))

(defun X-XShapeRectangles (xdpy dest-win dest-kind op x-off y-off rectangles &optional ordering)
  "This request specifies an array of rectangles, relative to the
origin of the window DEST-WIN plus the specified offset \\(X-OFF and
y-OFF\\) that together define a region.  This region is combined \\(as
specified by the operator OP\\) with the existing client region
\\(specified by KIND\) of the destination window DEST-WIN, and the
result is stored as the specified client region of the destination
window.  Note that the list of rectangles can be empty, specifying an
empty region; this is not the same as passing `X-None' to
`X-XShapeMask'. If known by the client, ordering relations on the
rectangles can be specified with the ordering argument.  This may
provide faster operation by the server.  The meanings of the ordering
values are the same as in the core protocol `XSetClipRectangles'
request.  If an incorrect ordering is specified, the server may
generate a Match error, but it is not required to do so.  If no error
is generated, the graphics results are undefined. Except for
`X-UnSorted', the rectangles should be nonintersecting, or the
resulting region will be undefined.  `X-UnSorted' means that the
rectangles are in arbitrary order.  `X-YSorted' means that the
rectangles are nondecreasing in their Y origin.  `X-YXSorted'
additionally constrains `X-YSorted' order in that all rectangles with
an equal Y origin are nondecreasing in their X origin.  `X-YXBanded'
additionally constrains `X-YXSorted' by requiring that, for every
possible Y scanline, all rectangles that include that scanline have
identical Y origins and Y extents."
  (let ((fo-recs (make-ffi-object `(array XRectangle ,(length rectangles))))
        (idx 0))
    (while (< idx (length rectangles))
      (ffi-aset fo-recs idx (nth idx rectangles))
      (incf idx))
    (X:ShapeCombineRectangles
     xdpy (if (X-Win-p dest-win) (X-Win-id dest-win) X-None)
     dest-kind x-off y-off fo-recs (length rectangles) op (or ordering X-UnSorted))))

(defun X-XShapeMask (xdpy dest-win dest-kind op x-off y-off src)
  "The SRC in this request is a 1-bit deep pixmap, or `X-None'.  If
SRC is `X-None', the specified client region is removed from the
window, causing the effective region to revert to the default region.
The `X-ShapeNotify' event generated by this request and subsequent
ShapeQueryExtents will report that a client shape has not been
specified.  If a valid pixmap is specified, it is converted to a
region, with bits set to one included in the region and bits set to
zero excluded, and an offset from the window origin as specified by
X-OFF and Y-OFF.  The resulting region is then combined \\(as
specified by the operator OP\\) with the existing client region
\\(indicated by DEST-KIND\\) of the destination window, and the result
is stored as the specified client region of the destination window.
The source pixmap and destination window must have been created on the
same screen, or else a Match error results."
  (X:ShapeCombineMask
   xdpy (if (X-Win-p dest-win) (X-Win-id dest-win) X-None)
   dest-kind x-off y-off (or (and src (X-Pixmap-id src)) 0) op))

(defun X-XShapeCombine (xdpy dest-win dest-kind op x-off y-off src src-kind)
  "The client region, indicated by SRC-KIND, of the source window SRC
is offset from the window DEST-WIN origin by X-OFF and Y-OFF and
combined with the client region, indicated by DEST-KIND, of the
destination window DEST-WIN.  The result is stored as the specified
client region of the destination window.  The source and destination
windows must be on the same screen, or else a Match error results."
  (X:ShapeCombineShape
   xdpy (if (X-Win-p dest-win) (X-Win-id dest-win) X-None)
   dest-kind x-off y-off (or (and src (X-Pixmap-id src)) 0) src-kind op))

(defun X-XShapeOffset (xdpy dest-win dest-kind x-off y-off)
  "The client region, indicated by DEST-KIND, is moved relative
to its current position by the amounts X-OFF and Y-OFF."
  (X:ShapeOffsetShape
   xdpy (if (X-Win-p dest-win) (X-Win-id dest-win))
   dest-kind x-off y-off))

(defun X-XShapeQueryExtents (xdpy dest-win)
  "The boundingShaped and clipShaped results are True if the
corresponding client regions have been specified, else they
are False.  The x, y, width, and height values define the
extents of the client regions, when a client region has not
been specified, the extents of the corresponding default
region are reported."
  ;; Not used, not implemented
  )

(defun X-XShapeSelectInput (xdpy dest-win enable)
  "Specifying enable as T causes the server to send the requesting
client a `X-ShapeNotify' event whenever the bounding or clip region of
the specified window is altered by any client.  Specifying enable as
NIL causes the server to stop sending such events."
  (X:ShapeSelectInput
   xdpy (if (X-Win-p dest-win) (X-Win-id dest-win) X-None)
   (if enable 1 0)))

(defun X-XShapeInputSelected (xdpy dest-win)
  "Return non-nil if on display XDPY DEST-WIN is enabled to receive
`X-ShapeNotify' events."
  (ffi-get
   (X:ShapeInputSelected
    xdpy (if (X-Win-p dest-win) (X-Win-id dest-win) X-None))))

(defun X-XShapeGetRectangles (xdpy dest-win dest-kind)
  "A list of rectangles describing the region indicated by DEST-KIND,
and the ordering of those rectangles, is returned.  The meaning of the
ordering values is the same as in the `X-XShapeRectangles' request."
  (let ((ret-cnt (make-ffi-object 'int))
        (ret-order (make-ffi-object 'int)))
    (when (zerop (ffi-get (X:ShapeGetRectangles
                           xdpy (if (X-Win-p dest-win) (X-Win-id dest-win) X-None)
                           dest-kind (ffi-address-of ret-cnt) (ffi-address-of ret-order))))
      (list t nil (ffi-get ret-cnt ret-order)))))

(provide 'xlib-xshape)

;;; xlib-xshape.el ends here
