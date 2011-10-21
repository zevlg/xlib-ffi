;;; xlib-shm.el --- FFI to XShm extension.
        
;; Copyright (C) 2008 by Zajcev Evegny.
        
;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Tue Nov 25 07:50:28 2008
;; Keywords: xlib, shm
         
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

(define-ffi-type ShmSeg unsigned-long)

(defconst XShm-BadShmSeg 0)

(define-ffi-struct XShmSegmentInfo
  (shmseg ShmSeg)
  (shmid int)                           ; return value from shmget()
  (shmaddr pointer)                     ; return from shmat
  (readOnly Bool))

(cffi:defcfun ("XShmQueryExtension" XShmQueryExtension) Bool
  (dpy (pointer Display)))

(cffi:defcfun ("XShmAttach" X:ShmAttach) int
  (dpy (pointer Display)) (shminfo (pointer XShmSegmentInfo)))

(cffi:defcfun ("XShmDetach" X:ShmDetach) int
  (dpy (pointer Display)) (shminfo (pointer XShmSegmentInfo)))

(provide 'xlib-shm)

;;; xlib-shm.el ends here
