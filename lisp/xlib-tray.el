;;; xlib-tray.el --- System tray support.

;; Copyright (C) 2005 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Sep  8 00:17:39 MSD 2005
;; Keywords: ffi, xlib

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; 

;;; Code:

(require 'xlib-xlib)

;;; Constants
(defconst X-Tray-dock-req 0 "Dock place request.")
(defconst X-Tray-message 1 "Message from dock app.")
(defconst X-Tray-cancel-message 2 "Cancels message.")
(defconst X-Tray-run-lisp 3 "Evaluate emacs lisp string")


;;; Functions
(defun XTrayInit (xdpy win)
  "Initialize atoms for interaction with system tray."
  (XTrayFindDock xdpy win))

(defun XTrayFindDock (xdpy win)
  (let ((dock (XGetSelectionOwner xdpy (XInternAtom xdpy "_NET_SYSTEM_TRAY_S0"))))
    (unless (X-Win-p dock)
      (XSelectInput xdpy (XDefaultRootWindow xdpy) XM-StructureNotify))

    (when (X-Win-p dock)
      (XTraySendOpcode xdpy dock dock X-Tray-dock-req (X-Win-id win)))))

(defun XTraySendOpcode (xdpy dock win message &optional data1 data2 data3)
  (let ((xev (make-ffi-object 'XClientMessageEvent)))
    (setf (XClientMessageEvent->type xev) X-ClientMessage
          (XClientMessageEvent->format xev) X-format-32
          (XClientMessageEvent->window xev) (X-Win-id win)
          (XClientMessageEvent->message_type xev) (X-Atom-id (XInternAtom xdpy "_NET_SYSTEM_TRAY_OPCODE")))
    (ffi-store xev (ffi-slot-offset 'XClientMessageEvent 'data) 'Time X-CurrentTime)
    (ffi-store xev (+ (ffi-slot-offset 'XClientMessageEvent 'data) 4) 'INT32 message)
    (when data1
      (ffi-store xev (+ (ffi-slot-offset 'XClientMessageEvent 'data) 8) 'INT32 data1))
    (when data2
      (ffi-store xev (+ (ffi-slot-offset 'XClientMessageEvent 'data) 12) 'INT32 data2))
    (when data3
      (ffi-store xev (+ (ffi-slot-offset 'XClientMessageEvent 'data) 16) 'INT32 data3))
    (XSendEvent xdpy dock nil XM-NoEvent (ffi-address-of xev))))

(defun XTraySendMessageData (xdpy dock win data)
  (let ((xev (make-ffi-object 'XClientMessageEvent)))
    (setf (XClientMessageEvent->type xev) X-ClientMessage
          (XClientMessageEvent->format xev) X-format-8
          (XClientMessageEvent->window xev) (X-Win-id win)
          (XClientMessageEvent->message_type xev) (X-Atom-id (XInternAtom xdpy "_NET_SYSTEM_TRAY_MESSAGE_DATA")))
    (ffi-store xev (ffi-slot-offset 'XClientMessageEvent 'data) 'c-data data)

    (XSendEvent xdpy dock nil XM-NoEvent (ffi-address-of xev))
    (XSync xdpy)))

(defun XTraySetXembedInfo (xdpy win flags)
  (XChangeProperty xdpy win (XInternAtom xdpy "_XEMBED_INFO")
                   XA-cardinal X-format-32 X-PropModeReplace (list 1 flags)))

(defun XTrayMapWindow (xdpy win)
  (XTraySetXembedInfo xdpy win 1))

(defun XTrayUnmapWindow (xdpy win)
  (XTraySetXembedInfo xdpy win 0))

(defun XTrayHandleEvent (xdpy win xev)
  (X-Event-CASE xev
    (:X-ClientMessage
     (X-Dpy-log xdpy 'x-tray "got client message"))

    (:X-PropertyNotify
     (X-Dpy-log xdpy 'x-tray "got property notify"))))

(defun XTraySendMessage (xdpy dock win opcode msg)
  (let ((id 1234))			; TODO: should be unique
    (XTraySendOpcode xdpy dock win opcode 0 (length msg) id)

    (while (> (length msg) 20)
      (XTraySendMessageData xdpy dock win (substring msg 0 20))
      (setq msg (substring msg 20)))
    
    (when (> (length msg) 0)
      (XTraySendMessageData xdpy dock win msg))))
      
(defun XTrayInitSessionInfo (xdpy win cmd)
  ;; TODO: write me
  )

(provide 'xlib-tray)

;;; xlib-tray.el ends here
