;;; test-xvideo.el --- Test Xvideo extension.
        
;; Copyright (C) 2008 by Zajcev Evegny.
        
;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Wed Nov 26 00:19:34 2008
;; Keywords: xlib, xv
         
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

;; Testing suite for Xvideo extension.

;;; Code:

(require 'ffi-libc)
(require 'xlib-xvideo)

(defconst test-xv-YV12-PLANAR 842094169)
(defconst test-xv-I420-PLANAR #x30323449)

(defun test-xv-get-xv-port (dpy)
  (let ((ai (XvQueryAdaptors dpy)))
    (unwind-protect
        (XvAdaptorInfo->base-id (car (last ai)))
      (XvFreeAdaptors ai))))

(defun test-xv-update-image (gl)
  (let ((dpy (get gl 'xv-dpy))
        (img (get gl 'xv-image)))
    (XvShmPutImage dpy (get gl 'xv-port) (get gl 'xv-win) (get gl 'xv-gc)
                   img 0 0 (XvImage->width img) (XvImage->height img)
                   0 0 (glyph-width gl) (glyph-height gl) t)
    (XFlush dpy)))

(defun test-xv-evh (dpy win xev)
;  (message "Evname=%S: xwin=%S, xev=%S" (X-Event-name xev) win xev)
  (X-Event-CASE xev
    (:X-MapNotify
     (let ((gl (X-Win-get-prop win 'xv-glyph)))
       (when (and (glyphp gl) (null (get gl 'xv-shminfo)))
         ;; First time map, setup the glyph
         (test-xv-setup-glyph dpy gl win)))
     )
    (:X-DestroyNotify
     (message "destroyed"))
    (:X-Expose
     (let ((gl (X-Win-get-prop win 'xv-glyph)))
       (when (and (glyphp gl) (get gl 'xv-shminfo))
         (test-xv-update-image gl)))
     )
    ))

(defun test-xv-glyph-attach (gl)
  (let* ((dpy (XOpenDisplay (device-connection (default-x-device))))
         (win (X-Win-find-or-make
               dpy (subwindow-xid (glyph-image-instance gl)))))
    (put gl 'xv-dpy dpy)
    (X-Win-put-prop win 'xv-glyph gl)
    (XSelectInput dpy win (Xmask-or XM-Exposure XM-StructureNotify))
    (X-Win-EventHandler-add win 'test-xv-evh)

    (XMapWindow dpy win)))

(defun test-xv-make-glyph (width height)
  (let ((gg (make-glyph [subwindow])))
    (resize-subwindow (glyph-image-instance gg) width height)
    (test-xv-glyph-attach gg)
    gg))

(defun test-xv-glyph-detach (gl)
  (let* ((dpy (get gl 'xv-dpy))
         (win (get gl 'xv-win))
         (img (get gl 'xv-image))
         (shminfo (get gl 'xv-shminfo))
         (gc (get gl 'xv-gc)))
    (XSelectInput dpy win 0)
    (X-Win-EventHandler-rem win 'test-xv-evh)
    (X-Win-rem-prop win 'xv-glyph)
    (remprop gl 'xv-win)

    ;; Detach shared segment
    (X:ShmDetach dpy (ffi-address-of shminfo))
    (c:shmdt (XShmSegmentInfo->shmid shminfo))
    (remprop gl 'xv-shminfo)

    (XFreeGC dpy gc)
    (remprop gl 'xv-gc)

    (X:Free img)
    (remprop gl 'xv-image)
    (remprop gl 'xv-port)

    (XCloseDisplay dpy)
    (remprop gl 'xv-dpy)
    ))

(defun test-xv-setup-glyph (dpy gl win &optional xv-port)
  (unless (= (XShmQueryExtension dpy) 1)
    (error "no Shm extension"))

  (unless xv-port (setq xv-port (test-xv-get-xv-port dpy)))
  (let* ((gc (XCreateGC dpy win))
         (yuv-width 128) ;320) ;128) ; (glyph-width gl))
         (yuv-height 80) ;256) ;80) ; (glyph-height gl))
         (yuv-shminfo (make-ffi-object 'XShmSegmentInfo))
         (yuv-image (X:xvShmCreateImage
                     dpy xv-port test-xv-I420-PLANAR ;test-xv-YV12-PLANAR
                     (ffi-null-pointer)
                     yuv-width yuv-height (ffi-address-of yuv-shminfo))))
    ;; Fill shminfo
    (setf (XShmSegmentInfo->shmid yuv-shminfo)
          (c:shmget c:IPC-PRIVATE (XvImage->data-size yuv-image)
                    (logior c:IPC-CREAT c:IPC-0777-MASK)))
    (setf (XShmSegmentInfo->shmaddr yuv-shminfo)
          (c:shmat (XShmSegmentInfo->shmid yuv-shminfo) (ffi-null-pointer) 0))
    (setf (XvImage->data yuv-image) (XShmSegmentInfo->shmaddr yuv-shminfo))
    (setf (XShmSegmentInfo->readOnly yuv-shminfo) X-False)

    (when (zerop (X:ShmAttach dpy (ffi-address-of yuv-shminfo)))
      (c:shmdt (XvImage->data yuv-image))
      (error "X:ShmAttach failed"))

    ;; put some properties
    (put gl 'xv-shminfo yuv-shminfo)
    (put gl 'xv-image yuv-image)
    (put gl 'xv-win win)
    (put gl 'xv-gc gc)
    (put gl 'xv-port xv-port)
    gl))

(defmacro xv-with-count-fps (frames &rest body)
  `(let ((start-time-dont-use-as-local-var (current-time)))
     ,@body
     (multiple-value-bind (nu secs usecs)
         (time-since start-time-dont-use-as-local-var)
       (/ ,frames (+ secs (/ usecs 1000000.0))))))

(defun test-xv-count-fps (start-time frames)
  (multiple-value-bind (nu secs usecs)
      (time-since start-time)
    (/ frames (+ secs (/ usecs 1000000.0)))))

(defun test-xv-random-rain (glyph &optional n)
  "Do random rain on the glyph N times.
Return FPS rate."
  (unless n (setq n 200))
  (let ((fr (c:fopen "/dev/urandom" "r"))
        (img (get glyph 'xv-image)))
    (unwind-protect
        (xv-with-count-fps n
          (dotimes (i n)
            (c:fread-1 (XvImage->data img)
                       1 (* (XvImage->width img) (XvImage->height img))
                       fr)
            (test-xv-update-image glyph)))
      (c:fclose fr))))

(defun test-xv-from-file (glyph file)
  "Play yuv data from FILE.
Return FPS rate."
  (let ((fr (c:fopen (expand-file-name file) "r"))
        (img (get glyph 'xv-image))
        (stime (current-time))
        (frames 0))
    (unwind-protect
        (while (positivep (c:fread-1 (XvImage->data img)
                                     1 (XvImage->data-size img)
                                     fr))
          (test-xv-update-image glyph)
          ;; Make a delay processing command events
          (let ((delay 25000)
                (stime (current-time)))
            (while (multiple-value-bind (h s ms) (time-since stime)
                     (and (zerop h) (zerop s) (< ms delay)))
              (if (input-pending-p)
                  (dispatch-event (next-command-event))
                (sit-for (multiple-value-bind (h s ms) (time-since stime)
                           (/ ms 1000000.0))))))
          (incf frames))
      (c:fclose fr))
    (test-xv-count-fps stime frames)))

(defun test-xv-picture (glyph &optional factor)
  "Generate picture for GLYPH."
  (let* ((img (get glyph 'xv-image))
         (imgd (XvImage->data img))
         (w (XvImage->width img))
         (h (XvImage->height img)))
    (dotimes (y h)
      (dotimes (x w)
        (ffi-aset imgd (+ (* y w) x)
                  (round (* x x y y (or factor 4))))))
    (test-xv-update-image glyph)))

(provide 'test-xvideo)

;;; test-xvideo.el ends here
