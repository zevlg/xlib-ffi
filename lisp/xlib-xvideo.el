;;; xlib-xvideo.el --- FFI to Xv extension.
        
;; Copyright (C) 2008 by Zajcev Evegny.
        
;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Tue Nov 25 06:44:34 2008
;; Keywords: xlib, Xvideo
         
;; This file is part of SXEmacs.

;; SXEmacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; SXEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Synched up with: Not in FSF

;;; Commentary:

;; 

;;; Code:
(require 'xlib-xlib)
(require 'xlib-shm)

(or (ffi-load-library "/usr/X11/lib/libXv")
    (ffi-load "libXv"))

;;; Constants
(defconst XvInputMask 1)
(defconst XvOutputMask 2)
(defconst XvVideoMask 4)
(defconst XvStillMask 8)
(defconst XvImageMask 16)

(defconst XvGettable 1)
(defconst XvSettable 2)

(defconst XvRGB 0)
(defconst XvYUV 1)

(defconst XvPacked 0)
(defconst XvPlanar 1)

(defconst XvTopToBottom 0)
(defconst XvBottomToTop 1)

;;; Types
(define-ffi-type XvPortID XID)
(define-ffi-type XvEncodingID XID)

(define-ffi-struct XvFormat
  (depth byte)
  (visual-id unsigned-long))

(define-ffi-struct XvRational
  (numerator int)
  (denominator int))

(define-ffi-struct XvAdaptorInfo
  (base-id XvPortID)
  (num-ports unsigned-long)
  (type byte)
  (name (pointer char))
  (num-formats unsigned-long)
  (formats (pointer XvFormat))
  (num-adaptors unsigned-long))

(define-ffi-struct XvEncodingInfo
  (encoding-id XvEncodingID)
  (name (pointer char))
  (width unsigned-long)
  (height unsigned-long)
  (rate XvRational)
  (num-encodings unsigned-long))

(define-ffi-struct XvAttribute
  (flags int)                           ; XvGettable, XvSettable
  (min-value int)
  (max-value int)
  (name (pointer char)))

(define-ffi-enum XvImageFormatType
  (XvRGB 0)
  (XvYUV 1))

(define-ffi-enum XvImageFormatFormat
  (XvPacked 0)
  (XvPlanar 1))

(define-ffi-enum XvImageFormatScanLine
  (XvTopToBottom 0)
  (XvBottomToTop 1))

(define-ffi-struct XvImageFormatValues
  (id int)                              ; unique descriptor for the format
  (type XvImageFormatType)              ; XvRGB, XvYUV
  (byte-order int)                      ; LSBFirst, MSBFirst
  (guid (array char 16))                ; Globally unique identifier
  (bits-per-pixel int)
  (format XvImageFormatFormat)          ; XvPacked, XvPlanar
  (num-planes int)

  ;; for RGB formats only
  (depth int)
  (red-mask unsigned-int)
  (green-mask unsigned-int)
  (blue-mask unsigned-int)

  ;; for YUV formats only
  (y_sample_bits unsigned-int)
  (u_sample_bits unsigned-int)
  (v_sample_bits unsigned-int)
  (horz_y_period unsigned-int)
  (horz_u_period unsigned-int)
  (horz_v_period unsigned-int)
  (vert_y_period unsigned-int)
  (vert_u_period unsigned-int)
  (vert_v_period unsigned-int)
  (component-order (array char 32))     ; eg UYVY
  (scanline-order XvImageFormatScanLine))

(define-ffi-struct XvImage
  (id int)
  (width int)
  (height int)
  (data-size int)
  (num-planes int)
  (pitches (pointer int))
  (offsets (pointer int))
  (data (pointer byte))
  (obdata XPointer))

;;; Functions
(cffi:defcfun ("XvQueryExtension" X:xvQueryExtension) int
  (dpy (pointer Display))
  (pversion (pointer unsigned-int))
  (prevision (pointer unsigned-int))
  (preqbase (pointer unsigned-int))
  (peventbase (pointer unsigned-int))
  (perrbase (pointer unsigned-int)))

(defun XvQueryExtension (dpy)
  "Return list of extension values.
Values are - version, revision, request base, event base, error-base."
  (let ((pver (make-ffi-object 'unsigned-int))
        (prev (make-ffi-object 'unsigned-int))
        (preqbase (make-ffi-object 'unsigned-int))
        (peventbase (make-ffi-object 'unsigned-int))
        (perrbase (make-ffi-object 'unsigned-int)))
    (when (zerop (X:xvQueryExtension
                  dpy (ffi-address-of pver) (ffi-address-of prev)
                  (ffi-address-of preqbase) (ffi-address-of peventbase)
                  (ffi-address-of perrbase)))
      (mapcar #'ffi-get (list pver prev preqbase peventbase perrbase)))))

(cffi:defcfun ("XvQueryAdaptors" X:xvQueryAdaptors) int
  (dpy (pointer Display)) (d Drawable)
  (nadaptors (pointer int)) (adaptor-info (pointer (pointer XvAdaptorInfo))))

(cffi:defcfun ("XvFreeAdaptorInfo" X:xvFreAdaptorInfo) void
  (adaptors (pointer XvAdaptorInfo)))

(defun XvQueryAdaptors (dpy &optional d)
  "Return list of Xv adapers info on display DPY for drawable D.
If D is ommited then XDefaultRootWindow is used.
Use `XvFreeAdaptors' when you are done with this list."
  (let* ((nads (make-ffi-object 'int))
         (ai (make-ffi-object '(pointer XvAdaptorInfo)))
         (ret (X:xvQueryAdaptors
               dpy (X-Drawable-id (or d (XDefaultRootWindow dpy)))
               (ffi-address-of nads) (ffi-address-of ai))))
    (when (zerop ret)
      (loop for i from 0 below (ffi-get nads)
        collect (cffi:inc-pointer
                 ai (* i (ffi-size-of-type 'XvAdaptorInfo)))))))

(defun XvFreeAdaptors (adaptors)
  "Release resources associated with ADAPTORS."
  (X:xvFreAdaptorInfo (car adaptors)))

(defun XvAdaptorType (adaptor)
  "Return list of capabilities supported by ADAPTOR.
Types are: input, output, video, still, image."
  (let ((mnames `((,XvInputMask . input)
                  (,XvOutputMask . output)
                  (,XvVideoMask . video)
                  (,XvStillMask . still)
                  (,XvImageMask . image))))
    (loop for mask in (list XvInputMask XvOutputMask XvVideoMask
                            XvStillMask XvImageMask)
      when (not (zerop (logand (XvAdaptorInfo->type adaptor) mask)))
      collect (cdr (assq mask mnames)))))

(defun XvAdaptorFormats (adaptor)
  "Return list of formats supported by ADAPTOR."
  (loop for i from 0 below (XvAdaptorInfo->num-formats adaptor)
    collect (cffi:inc-pointer
             (XvAdaptorInfo->formats adaptor)
             (* i (ffi-size-of-type 'XvFormat)))))

(cffi:defcfun ("XvQueryEncodings" X:xvQueryEncodings) int
  (dpy (pointer Display))
  (port XvPortID)
  (n-encodings (pointer int))
  (encodings (pointer (pointer XvEncodingInfo))))

(cffi:defcfun ("XvFreeEncodingInfo" X:xvFreeEncodingInfo) void
  (encodings (pointer XvEncodingInfo)))

(defun XvQueryEncodings (dpy port-id)
  "Return list of encodings for DPY and PORT-ID."
  (let ((n-encodings (make-ffi-object 'int))
        (encodings (make-ffi-object '(pointer XvEncodingInfo))))
    (when (zerop (X:xvQueryEncodings
                  dpy port-id (ffi-address-of n-encodings)
                  (ffi-address-of encodings)))
      (loop for i from 0 below (ffi-get n-encodings)
        collect (cffi:inc-pointer
                 encodings
                 (* i (ffi-size-of-type 'XvEncodingInfo)))))))

(defun XvFreeEncodings (encs)
  "Free resources associated with ENCS."
  (X:xvFreeEncodingInfo (car encs)))

(cffi:defcfun ("XvQueryPortAttributes" X:xvQueryPortAttributes)
  (pointer XvAttribute)
  (dpy (pointer Display))
  (port XvPortID)
  (nattrs (pointer int)))

(defun XvQueryPortAttributes (dpy port-id)
  "On display DPY query for attributes of PORT-ID."
  (let* ((nattrs (make-ffi-object 'int))
         (ret (X:xvQueryPortAttributes dpy port-id (ffi-address-of nattrs))))
    (when (and (not (ffi-null-p ret))
               (positivep (ffi-get nattrs)))
      (loop for i from 0 below (ffi-get nattrs)
        collect (cffi:inc-pointer
                 ret
                 (* i (ffi-size-of-type 'XvAttribute)))))))

(defun XvFreeAttributes (attrs)
  "Release resources associated with ATTRS."
  (X:Free (car attrs)))

(cffi:defcfun ("XvListImageFormats" X:xvListImageFormats)
  (pointer XvImageFormatValues)
  (dpy (pointer Display))
  (port-id XvPortID)
  (count-ret (pointer int)))

(defun XvListImageFormats (dpy port)
  "Return list of image formats supported by PORT."
  (let* ((cret (make-ffi-object 'int))
         (ret (X:xvListImageFormats dpy port (ffi-address-of cret))))
    (when (and (not (ffi-null-p ret))
               (positivep (ffi-get cret)))
      (loop for i from 0 below (ffi-get cret)
        collect (cffi:inc-pointer
                 ret
                 (* i (ffi-size-of-type 'XvImageFormatValues)))))))

(defun XvFreeImageFormats (ifo)
  (X:Free (car ifo)))

(cffi:defcfun ("XvShmCreateImage" X:xvShmCreateImage) (pointer XvImage)
  (dpy (pointer Display)) (port XvPortID) (id int)
  (data (pointer char)) (width int) (height int)
  (shminfo (pointer XShmSegmentInfo)))

(cffi:defcfun ("XvShmPutImage" X:xvShmPutImage) int
  (dpy (pointer Display)) (id XvPortID)
  (d Drawable) (gc GC) (image (pointer XvImage))
  (src-x int) (src-y int) (src-w unsigned-int) (src-h unsigned-int)
  (dst-x int) (dst-y int) (dst-w unsigned-int) (dst-h unsigned-int)
  (send-event Bool))

(defun XvShmPutImage (dpy port-id d gc image src-x src-y src-w src-h
                          dst-x dst-y dst-w dst-h send-event)
  (X:xvShmPutImage dpy port-id (X-Drawable-id d) gc image
                   src-x src-y src-w src-h
                   dst-x dst-y dst-w dst-h
                   (if send-event 1 0)))

(defun XvInfoProducer (dpy)
  (multiple-value-bind (ver rev reqb evb errb)
      (XvQueryExtension dpy)
    (insert "========================================\n")
    (insert "XvQueryExtension returned the following:\n")
    (insert (format "p_version      : %d\n" ver))
    (insert (format "p_release      : %d\n" rev))
    (insert (format "p_request_base : %d\n" reqb))
    (insert (format "p_event_base   : %d\n" evb))
    (insert (format "p_error_base   : %d\n" errb))
    (insert "========================================\n"))

  (let ((ai (XvQueryAdaptors dpy)))
    (insert "=======================================\n")
    (insert "XvQueryAdaptors returned the following:\n")
    (insert (format "%d adaptors available.\n" (length ai)))
    (dolist (ad ai)
      (insert (format " Name:        %s\n"
                      (ffi-get (XvAdaptorInfo->name ad)
                               :type 'c-string)))
      (insert (format " Type:        %s\n"
                      (mapconcat #'symbol-name
                                 (XvAdaptorType ad) " | ")))
      (insert (format " Ports:       %d\n"
                      (XvAdaptorInfo->num-ports ad)))
      (insert (format " First port:  %d\n"
                      (XvAdaptorInfo->base-id ad)))

      (insert " Format list:\n")
      (dolist (fmt (XvAdaptorFormats ad))
        (insert (format "   depth=%d, visual=%d\n"
                        (XvFormat->depth fmt)
                        (XvFormat->visual-id fmt))))

      (loop for port from (XvAdaptorInfo->base-id ad)
        below (+ (XvAdaptorInfo->base-id ad)
                 (XvAdaptorInfo->num-ports ad))
        do (insert (format " Port %d info:\n" port))
        do (let ((encs (XvQueryEncodings dpy port)))
             (when encs
               (insert "   Encodings:\n")
               (dolist (enc encs)
                 (insert
                  (format "     id=%d, name=%s, size=%dx%d, "
                          (XvEncodingInfo->encoding-id enc)
                          (ffi-get (XvEncodingInfo->name enc)
                                   :type 'c-string)
                          (XvEncodingInfo->width enc)
                          (XvEncodingInfo->height enc)))
                 (let ((rate (cffi:inc-pointer
                              enc (ffi-slot-offset 'XvEncodingInfo 'rate))))
                   (insert (format "numerator=%d, denominator=%d\n"
                                   (XvRational->numerator rate)
                                   (XvRational->denominator rate)))))
               (XvFreeEncodings encs)))
        do (let ((attrs (XvQueryPortAttributes dpy port)))
             (when attrs
               (insert "   Attributes:\n")
               (dolist (att attrs)
                 (insert (format "     name:       %s\n"
                                 (ffi-get (XvAttribute->name att)
                                          :type 'c-string)))
                 (insert (format "     flags:     %s%s\n"
                                 (if (zerop (logand (XvAttribute->flags att)
                                                    XvGettable))
                                     "" " get")
                                 (if (zerop (logand (XvAttribute->flags att)
                                                    XvSettable))
                                     "" " set")))
                 (insert (format "     min_color:  %d\n"
                                 (XvAttribute->min-value att)))
                 (insert (format "     max_color:  %d\n"
                                 (XvAttribute->max-value att))))
               (XvFreeAttributes attrs)))
        do (let ((ifo (XvListImageFormats dpy port)))
             (when ifo
               (insert "   Image Formats:\n")
               (dolist (fo ifo)
                 (insert (format "     0x%x (%s) %S, bpp=%d\n"
                                 ;; Assume id has 0 offset in
                                 ;; XvImageFormatValues
                                 (ffi-pointer-address
                                  (ffi-get fo :type 'pointer))
                                 (ffi-get fo :type '(c-data . 4))
                                 (XvImageFormatValues->format fo)
                                 (XvImageFormatValues->bits-per-pixel fo)
                                 )))
               (XvFreeImageFormats ifo)))
        ))
    (XvFreeAdaptors ai)))

(defun XvInfo ()
  "Print Xv extension info."
  (interactive)
  (let ((dpy (XOpenDisplay (device-connection (default-x-device)))))
    (with-displaying-help-buffer
     #'(lambda ()
         (set-buffer standard-output)
         (XvInfoProducer dpy))
     "*XvInfo*")))

(provide 'xlib-xvideo)

;;; xlib-xvideo.el ends here
