;;; genkeysymdef.el --- Generate keysymdef file.

;; Copyright (C) 2007 by Zajcev Evgeny

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Keywords: 

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
;; along with XWEM; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; 

;;; Code:

(defvar genksd-try-files
  '("/usr/include/X11/keysymdef.h"
    "/usr/local/include/X11/keysymdef.h"
    "/usr/X11R6/include/X11/keysymdef.h"))

(defvar genksd-ifdefs 
  '("XK_MISCELLANY" "XK_LATIN1" "XK_XKB_KEYS" "XK_CYRILLIC")
  "List of ifdefs to define.
Possible values are:
    XK_MISCELLANY XK_XKB_KEYS XK_3270 XK_LATIN1 XK_LATIN2
    XK_LATIN3 XK_LATIN4 XK_LATIN8 XK_LATIN9 XK_KATAKANA
    XK_ARABIC XK_CYRILLIC XK_GREEK XK_TECHNICAL XK_SPECIAL
    XK_PUBLISHING XK_APL XK_HEBREW XK_THAI XK_KOREAN XK_ARMENIAN
    XK_GEORGIAN XK_CAUCASUS XK_VIETNAMESE XK_CURRENCY
    XK_MATHEMATICAL XK_BRAILLE")

(defun genksd-rename (kname)
  (replace-in-string kname "_" "-"))

(defun genksd-generate (outfile)
  (let ((ifile (find nil genksd-try-files
                     :test #'(lambda (nu file)
                               (file-exists-p file))))
        (ofile (find-file-noselect outfile)))

    (with-temp-buffer
      (insert-file ifile)
      (goto-char (point-min))
      ;; Remove comments
      (while (search-forward "/*" nil t)
        (let ((spoint (match-beginning 0)))
          (when (search-forward "*/" nil t)
            (delete-region spoint (match-end 0)))))
      (goto-char (point-min))

      (erase-buffer ofile)
      (with-current-buffer ofile
        (insert ";;; Automatically generated file -- DO NOT EDIT OR DELETE\n"))

      (while (not (eobp))
        (cond ((looking-at
                "#define[ \t]+\\([^ \t]+\\)[ \t]+0x\\([0-9a-fa-F]+\\)")
               (let ((v (match-string 2))
                     (n (genksd-rename (match-string 1))))
                 (with-current-buffer ofile
                   (insert (format "(defconst %s #x%s)\n" n v)))))
              ((looking-at "#ifdef[ \t]+\\(.*\\)")
               (let ((mb (match-beginning 0))
                     (mt (match-string 1)))
                 (if (member mt genksd-ifdefs)
                     (with-current-buffer ofile
                       (insert (format ";; %s\n" mt)))
                   (when (search-forward "#endif" nil)
                     (delete-region mb (match-end 0))))))
              )
        (forward-line))
      (with-current-buffer ofile
        (insert (format "(provide '%s)\n"
                        (file-name-sans-extension
                         (file-name-nondirectory outfile))))
        (save-buffer)))))

(provide 'genkeysymdef)

;;; genkeysymdef.el ends here
