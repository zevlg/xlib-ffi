;;; ffi-xlib.el --- Emacs interface to Xlib.

;; Copyright (C) 2005,2006 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Sun Aug 28 02:21:02 MSD 2005
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

;; 

;;; Code:

(require 'ffi)
(or (ffi-load-library "/usr/X11/lib/libX11")
    (ffi-load "libX11"))

(define-ffi-type CARD32 unsigned-long)
(define-ffi-type CARD16 unsigned-short)
(define-ffi-type CARD8 unsigned-char)

(define-ffi-type INT32 long)
(define-ffi-type INT16 short)
(define-ffi-type INT8 char)

(define-ffi-type BOOL CARD8)
(define-ffi-type BYTE CARD8)

(define-ffi-type XPointer (pointer char))
(define-ffi-type Bool int)
(define-ffi-type XID unsigned-long)
(define-ffi-type Mask unsigned-long)
(define-ffi-type Atom unsigned-long)
(define-ffi-type VisualID unsigned-long)
(define-ffi-type Time unsigned-long)
(define-ffi-type Pixmap XID)
(define-ffi-type Cursor XID)
(define-ffi-type Colormap XID)
(define-ffi-type GContext XID)
(define-ffi-type KeySym XID)

(define-ffi-type KeyCode unsigned-char)

(define-ffi-type Window XID)
(define-ffi-type Drawable XID)
(define-ffi-type Font XID)

(define-ffi-struct _XGC
  (ext_data (pointer void))
  (gid GContext))

(define-ffi-type GC (pointer _XGC))
  
;; Data structure for setting graphics context.
(define-ffi-struct XGCValues
  (function int)
  (plane_mask unsigned-long)
  (foreground unsigned-long)
  (background unsigned-long)
  (line_width int)
  (line_style int)
  (cap_style int)
  (join_style int)
  (fill_style int)
  (fill_rule int)
  (arc_mode int)
  (tile Pixmap)
  (stipple Pixmap)
  (ts_x_origin int)
  (ts_y_origin int)
  (font Font)
  (subwindow_mode int)
  (graphics_exposures Bool)
  (clip_x_origin int)
  (clip_y_origin int)
  (clip_mask Pixmap)
  (dash_offset int)
  (dashes char))

(define-ffi-struct Visual
  (ext_data (pointer void))
  (visualid VisualID)
  (class int)
  (red_mask unsigned-long) (green_mask unsigned-long) (blue_mask unsigned-long)
  (bits_per_pixel int)
  (map_entries int))

(define-ffi-struct Depth
  (depth int)
  (nvisuals int)
  (visuals (pointer Visual)))

(define-ffi-struct ScreenFormat
  (ext_data (pointer void))
  (depth int)
  (bits_per_pixel int)
  (scanline_pad int))

(define-ffi-struct XSetWindowAttributes
  (background_pixmap Pixmap)
  (background_pixel unsigned-long)
  (border_pixmap Pixmap)
  (border_pixel unsigned-long)
  (bit_gravity int)
  (win_gravity int)
  (backing_store int)
  (backing_planes unsigned-long)
  (backing_pixel unsigned-long)
  (save_under Bool)
  (event_mask long)
  (do_not_propagate_mask long)
  (override_redirect Bool)
  (colormap Colormap)
  (cursor Cursor))

(define-ffi-struct XWindowAttributes
  (x int) (y int)
  (width int) (height int)
  (border_width int)
  (depth int)
  (visual (pointer Visual))
  (root Window)
  (class int)
  (bit_gravity int)
  (win_gravity int)
  (backing_store int)
  (backing_planes unsigned-long)
  (backing_pixel unsigned-long)
  (save_under Bool)
  (colormap Colormap)
  (map_installed Bool)
  (map_state int)
  (all_event_masks long)
  (your_event_mask long)
  (do_not_propagate_mask long)
  (override_redirect Bool)
  (screen (pointer Screen)))

(define-ffi-struct XHostAddress
  (family int)
  (length int)
  (address c-string))

(define-ffi-struct XServerInterpretedAddress
  (typelength int)
  (valuelength int)
  (type c-string)
  (value c-string))

(define-ffi-struct XImage
  (width int)
  (height int)
  (xoffset int)
  (format int)
  (data (pointer char))
  (byte_order int)
  (bitmap_unit int)
  (bitmap_bit_order int)
  (bitmap_pad int)
  (depth int)
  (bytes_per_line int)
  (bits_per_pixel int)
  (red_mask unsigned-long)
  (green_mask unsigned-long)
  (blue_mask unsigned-long)
  (obdata XPointer)
  ;; XXX Functions
  )

;; Data structure for XReconfigureWindow
(define-ffi-struct XWindowChanges
  (x int)
  (y int)
  (width int)
  (height int)
  (border_width int)
  (sibling Window)
  (stack_mode int))

;; Data structure used by color operations
(define-ffi-struct XColor
  (pixel unsigned-long)
  (red unsigned-short)
  (green unsigned-short)
  (blue unsigned-short)
  (flags char)
  (pad char))

;; Data structures for graphics operations.  On most machines, these are
;; congruent with the wire protocol structures, so reformatting the data
;; can be avoided on these architectures.
(define-ffi-struct XSegment
  (x1 short) (y1 short) (x2 short) (y2 short))

(define-ffi-struct XPoint
  (x short) (y short))
    
(define-ffi-struct XRectangle
  (x short) (y short)
  (width unsigned-short)
  (height unsigned-short))
    
(define-ffi-struct XArc
  (x short)
  (y short)
  (width unsigned-short)
  (height unsigned-short)
  (angle1 short)
  (angle2 short))

;; Data structure for XChangeKeyboardControl
(define-ffi-struct XKeyboardControl
  (key_click_percent int)
  (bell_percent int)
  (bell_pitch int)
  (bell_duration int)
  (led int)
  (led_mode int)
  (key int)
  (auto_repeat_mode int))

; Data structure for XGetKeyboardControl
(define-ffi-struct XKeyboardState
  (key_click_percent int)
  (bell_percent int)
  (bell_pitch unsigned-int)
  (bell_duration unsigned-int)
  (led_mask unsigned-long)
  (global_auto_repeat int)
  (auto_repeats (array char 32)))

;; Data structure for XGetMotionEvents.
(define-ffi-struct XTimeCoord
  (time Time)
  (x short) (y short))

;; Data structure for X{Set,Get}ModifierMapping
(define-ffi-struct XModifierKeymap
  (max_keypermod int)
  (modifiermap (pointer KeyCode)))

(declare-ffi-type Display)

(define-ffi-struct Screen
  (ext_data (pointer void))
  (display (pointer Display))
  (root Window)
  (width int) (height int)
  (mwidth int) (mheight int)
  (ndepths int)
  (depths (pointer Depth))
  (root_depth int)
  (root_visual (pointer Visual))
  (default_gc GC)
  (cmap Colormap)
  (white_pixel unsigned-long)
  (black_pixel unsigned-long)
  (max_maps int) (min_maps int)
  (backing_store int)
  (save_unders Bool)
  (root_input_mask long))

(define-ffi-struct Display
  (ext_data (pointer void))
  (private1 (pointer void))
  (fd int)
  (private2 int)
  (proto_major_version int)
  (proto_minor_version int)
  (vendor c-string)
  (private3 XID)
  (private4 XID)
  (private5 XID)
  (private6 int)
  (resource_alloc (pointer void))       ; resource allocator
  (byte_order int)
  (bitmap_unit int)
  (bitmap_pad int)
  (bitmap_bit_order int)
  (nformats int)
  (pixmap_format (pointer ScreenFormat))
  (private8 int)
  (release int)
  (private9 (pointer void))
  (private10 (pointer void))
  (qlen int)
  (last_request_read unsigned-long)
  (request unsigned-long)
  (private11 XPointer)
  (private12 XPointer)
  (private13 XPointer)
  (private14 XPointer)
  (max_request_size unsigned-int)
  (db (pointer void))
  (private15 (pointer void))
  (display_name c-string)
  (default_screen int)
  (nscreens int)
  (screens (pointer Screen))
  (motion_buffer unsigned-long)
  (private16 unsigned-long)
  (min_keycode int)
  (max_keycode int)
  (private17 XPointer)
  (private18 XPointer)
  (private19 int)
  (xdefaults c-string))

(define-ffi-struct XKeyEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (window Window)
  (root Window)
  (subwindow Window)
  (time Time)
  (x int)
  (y int)
  (x_root int)
  (y_root int)
  (state unsigned-int)
  (keycode unsigned-int)
  (same_screen Bool))
(define-ffi-type XKeyPressedEvent XKeyEvent)
(define-ffi-type XKeyReleasedEvent XKeyEvent)

(define-ffi-struct XButtonEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (window Window)
  (root Window)
  (subwindow Window)
  (time Time)
  (x int)
  (y int)
  (x_root int)
  (y_root int)
  (state unsigned-int)
  (button unsigned-int)
  (same_screen Bool))
(define-ffi-type XButtonPressedEvent XButtonEvent)
(define-ffi-type XButtonReleasedEvent XButtonEvent)

(define-ffi-struct XMotionEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (window Window)
  (root Window)
  (subwindow Window)
  (time Time)
  (x int)
  (y int)
  (x_root int)
  (y_root int)
  (state unsigned-int)
  (is_hint char)
  (same_screen Bool))
(define-ffi-type XPointerMovedEvent XMotionEvent)

(define-ffi-struct XCrossingEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (window Window)
  (root Window)
  (subwindow Window)
  (time Time)
  (x int)
  (y int)
  (x_root int)
  (y_root int)
  (mode int)
  (detail int)
  (same_screen Bool)
  (focus Bool)
  (state unsigned-int))
(define-ffi-type XEnterWindowEvent XCrossingEvent)
(define-ffi-type XLeaveWindowEvent XCrossingEvent)

(define-ffi-struct XFocusChangeEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (window Window)
  (mode int)
  (detail int))
(define-ffi-type XFocusInEvent XFocusChangeEvent)
(define-ffi-type XFocusOutEvent XFocusChangeEvent)

(define-ffi-struct XKeymapEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (window Window)
  (key_vector (array char 32)))

(define-ffi-struct XExposeEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (window Window)
  (x int)
  (y int)
  (width int)
  (height int)
  (count int))

(define-ffi-struct XGraphicsExposeEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (drawable Drawable)
  (x int)
  (y int)
  (width int)
  (height int)
  (count int)
  (major_code int)
  (minor_code int))

(define-ffi-struct XNoExposeEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (drawable Drawable)
  (major_code int)
  (minor_code int))

(define-ffi-struct XVisibilityEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (window Window)
  (state int))

(define-ffi-struct XCreateWindowEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (parent Window)
  (window Window)
  (x int)
  (y int)
  (width int)
  (height int)
  (border_width int)
  (override_redirect Bool))

(define-ffi-struct XDestroyWindowEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (event Window)
  (window Window))

(define-ffi-struct XUnmapEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (event Window)
  (window Window)
  (from_configure Bool))

(define-ffi-struct XMapEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (event Window)
  (window Window)
  (override_redirect Bool))

(define-ffi-struct XMapRequestEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (parent Window)
  (window Window))

(define-ffi-struct XReparentEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (event Window)
  (window Window)
  (parent Window)
  (x int)
  (y int)
  (override_redirect Bool))

(define-ffi-struct XConfigureEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (event Window)
  (window Window)
  (x int)
  (y int)
  (width int)
  (height int)
  (border_width int)
  (above Window)
  (override_redirect Bool))

(define-ffi-struct XGravityEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (event Window)
  (window Window)
  (x int)
  (y int))

(define-ffi-struct XResizeRequestEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (*display Display)
  (window Window)
  (width int)
  (height int))

(define-ffi-struct XConfigureRequestEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (parent Window)
  (window Window)
  (x int)
  (y int)
  (width int)
  (height int)
  (border_width int)
  (above Window)
  (detail int)
  (value_mask unsigned-long))

(define-ffi-struct XCirculateEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (event Window)
  (window Window)
  (place int))

(define-ffi-struct XCirculateRequestEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (parent Window)
  (window Window)
  (place int))

(define-ffi-struct XPropertyEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (window Window)
  (atom Atom)
  (time Time)
  (state int))

(define-ffi-struct XSelectionClearEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (window Window)
  (selection Atom)
  (time Time))

(define-ffi-struct XSelectionRequestEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (owner Window)
  (requestor Window)
  (selection Atom)
  (target Atom)
  (property Atom)
  (time Time))

(define-ffi-struct XSelectionEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (requestor Window)
  (selection Atom)
  (target Atom)
  (property Atom)
  (time Time))

(define-ffi-struct XColormapEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (window Window)
  (colormap Colormap)
  (new Bool)
  (state int))

(define-ffi-struct XClientMessageEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (window Window)
  (message_type Atom)
  (format int)
  (data (union anonymous-union1
               (b (array char 20))
               (s (array short 10))
               (l (array long 5)))))

(define-ffi-struct XMappingEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (window Window)
  (request int)
  (first_keycode int)
  (count int))

(define-ffi-struct XErrorEvent
  (type int)
  (display_name (pointer Display))
  (resourceid XID)
  (serial unsigned-long)
  (error_code unsigned-char)
  (request_code unsigned-char)
  (minor_code unsigned-char))

(define-ffi-struct XAnyEvent
  (type int)
  (serial unsigned-long)
  (send_event Bool)
  (display (pointer Display))
  (window Window))

(define-ffi-type XEvent
  (union XEvent
         (type int)
         (xany XAnyEvent)
         (xkey XKeyEvent)
         (xbutton XButtonEvent)
         (xmotion XMotionEvent)
         (xcrossing XCrossingEvent)
         (xfocus XFocusChangeEvent)
         (xexpose XExposeEvent)
         (xgraphicsexpose XGraphicsExposeEvent)
         (xnoexpose XNoExposeEvent)
         (xvisibility XVisibilityEvent)
         (xcreatewindow XCreateWindowEvent)
         (xdestroywindow XDestroyWindowEvent)
         (xunmap XUnmapEvent)
         (xmap XMapEvent)
         (xmaprequest XMapRequestEvent)
         (xreparent XReparentEvent)
         (xconfigure XConfigureEvent)
         (xgravity XGravityEvent)
         (xresizerequest XResizeRequestEvent)
         (xconfigurerequest XConfigureRequestEvent)
         (xcirculate XCirculateEvent)
         (xcirculaterequest XCirculateRequestEvent)
         (xproperty XPropertyEvent)
         (xselectionclear XSelectionClearEvent)
         (xselectionrequest XSelectionRequestEvent)
         (xselection XSelectionEvent)
         (xcolormap XColormapEvent)
         (xclient XClientMessageEvent)
         (xmapping XMappingEvent)
         (xerror XErrorEvent)
         (xkeymap XKeymapEvent)
         (pad (array long 24))))

(define-ffi-struct XCharStruct
    (lbearing short)
    (rbearing short)
    (width short)
    (ascent short)
    (descent short)
    (attributes unsigned-short))

(define-ffi-struct XFontProp
    (name Atom)
    (card32 unsigned-long))

(define-ffi-struct XFontStruct
    (ext_data (pointer void))
    (fid Font)
    (direction unsigned-int)
    (min_char_or_byte2 unsigned-int)
    (max_char_or_byte2 unsigned-int)
    (min_byte1 unsigned-int)
    (max_byte1 unsigned-int)
    (all_chars_exist Bool)
    (default_char unsigned-int)
    (n_properties int)
    (properties (pointer XFontProp))
    (min_bounds XCharStruct)
    (max_bounds XCharStruct)
    (per_char (pointer XCharStruct))
    (ascent int)
    (descent int))

(define-ffi-struct XTextItem
  (chars c-string)
  (nchars int)
  (delta int)
  (font Font))

(define-ffi-struct XChar2b
  (byte1 unsigned-char)
  (byte2 unsigned-char))

(define-ffi-struct XTextItem16
  (chars (pointer XChar2b))
  (nchars int)
  (delta int)
  (font Font))

;; new structure for manipulating TEXT properties; used with WM_NAME, 
;; WM_ICON_NAME, WM_CLIENT_MACHINE, and WM_COMMAND.
(define-ffi-struct XTextProperty
  (value (pointer unsigned-char))
  (encoding Atom)                       ; prop type
  (format int)                          ; prop data format: 8, 16, or 32
  (nitems unsigned-long))		; number of data items in value

(define-ffi-struct XSizeHints
  (flags long)
  (x int) (y int)
  (width int) (height int)
  (min_width int) (min_height int)
  (max_width int) (max_height int)
  (width_inc int) (height_inc int)
  (min_aspect_x int) (min_aspect_y int)
  (max_aspect_x int) (max_aspect_y int)
  (base_width int) (base_height int)
  (win_gravity int))

(define-ffi-struct XWMHints
  (flags long)
  (input Bool)
  (initial_state int)
  (icon_pixmap Pixmap)
  (icon_window Window)
  (icon_x int)
  (icon_y int)
  (icon_mask Pixmap)
  (window_group XID))

(define-ffi-struct XClassHint
  (res_name (pointer char))
  (res_class (pointer char)))


(defmacro define-xlib-function (fn rt &rest in-args)
  "Define new foreign xlib function FN."
  (let* ((n (symbol-name fn))
         (ns (if (and (> (length n) 1)
                      (char= (aref n 0) ?X)
                      (char= (aref n 1) ?:))
                 (concat (substring n 0 1) (substring n 2))
               n))
         (as (mapcar 'first in-args))
         (at (mapcar 'second in-args)))
    `(define-ffi-function ,fn (,@as)
       "No documentation."
       '(function ,rt ,@at)
       ,ns)))

(define-xlib-function X:OpenDisplay (pointer Display)
  (name c-string))

(define-xlib-function X:CloseDisplay void
  (dpy (pointer Display)))

(define-xlib-function X:ServerVendor c-string
  (dpy (pointer Display)))

(define-xlib-function X:DisplayString c-string
  (dpy (pointer Display)))


(define-xlib-function X:GrabServer int
  (dpy (pointer Display)))

(define-xlib-function X:UngrabServer int
  (dpy (pointer Display)))

(define-xlib-function X:ActivateScreenSaver int
  (dpy (pointer Display)))

(define-xlib-function X:AddHost int
  (dpy (pointer Display)) (host (pointer XHostAddress)))

(define-xlib-function X:AddHosts int
  (dpy (pointer Display)) (hosts (pointer XHostAddress)) (num int))

(define-xlib-function X:AllocColor int
  (dpy (pointer Display)) (cmap Colormap) (xc-in-out (pointer XColor)))

(define-xlib-function X:AllocColorCells int
  (dpy (pointer Display))
  (cmap Colormap)
  (config Bool)
  (plane-masks-return (pointer unsigned-long))
  (nplanes unsigned-int)
  (pixels-return (pointer unsigned-long))
  (npixels unsigned-int))

(define-xlib-function X:AllocColorPlanes int
  (dpy (pointer Display))
  (cmap Colormap)
  (config Bool)
  (pixels-return (pointer unsigned-long))
  (ncolors int)
  (nreds int)
  (ngreens int)
  (nblues int)
  (rmask-return (pointer unsigned-long))
  (gmask-return (pointer unsigned-long))
  (bmask-return (pointer unsigned-long)))

(define-xlib-function X:AllocNamedColor int
  (dpy (pointer Display))
  (cmap Colormap)
  (color-name c-string)
  (screen-def-return (pointer XColor))
  (exact-def-return (pointer XColor)))

(define-xlib-function X:QueryColors int
  (dpy (pointer Display))
  (cmap Colormap)
  (defs-in-out (pointer XColor))
  (ncolors int))

(define-xlib-function X:CreateSimpleWindow Window
  (dpy (pointer Display)) (parent Window)
  (x int) (y int) (w unsigned-int) (h unsigned-int)
  (bw unsigned-int) (border unsigned-long) (bg unsigned-long))

(define-xlib-function X:CreateWindow Window
  (dpy (pointer Display)) (parent Window)
  (x int) (y int) (w unsigned-int) (h unsigned-int)
  (bw unsigned-int)
  (depth int) (class unsigned-int) (visual (pointer Visual))
  (vmask unsigned-long) (attrs (pointer XSetWindowAttributes)))

(define-xlib-function X:ChangeWindowAttributes int
  (xdpy (pointer Display)) (win Window) (vmask unsigned-long)
  (pointer XSetWindowAttributes))

(define-xlib-function X:SetWindowBackground int
  (dpy (pointer Display)) (win Window) (pixel unsigned-long))

(define-xlib-function X:SetWindowBackgroundPixmap int
  (dpy (pointer Display)) (win Window) (pixmap Pixmap))

(define-xlib-function X:SetWindowBorder int
  (dpy (pointer Display)) (win Window) (pixel unsigned-long))

(define-xlib-function X:SetWindowBorderPixmap int
  (dpy (pointer Display)) (win Window) (pixmap Pixmap))

(define-xlib-function X:SetWindowColormap int
  (dpy (pointer Display)) (win Window) (cmap Colormap))

(define-xlib-function X:GetWindowAttributes int
  (dpy (pointer Display)) (win Window) (pointer XWindowAttributes))

(define-xlib-function X:GetGeometry int
  (dpy (pointer Display))
  (d Drawable) (root_return (pointer Window))
  (x_return (pointer int)) (y_return (pointer int))
  (width_return (pointer unsigned-int))
  (height_return (pointer unsigned-int))
  (border_width_return (pointer unsigned-int))
  (depth_return (pointer unsigned-int)))

(define-xlib-function X:ConfigureWindow int
  (dpy (pointer Display)) (win Window) (vmask unsigned-int)
  (pointer XWindowChanges))

(define-xlib-function X:MoveWindow int
  (dpy (pointer Display)) (win Window) (x int) (y int))

(define-xlib-function X:ResizeWindow int
  (dpy (pointer Display)) (w Window)
  (width unsigned-int) (height unsigned-int))

(define-xlib-function X:MoveResizeWindow int
  (dpy (pointer Display)) (w Window)
  (x int) (y int) (width unsigned-int) (height unsigned-int))

(define-xlib-function X:SetWindowBorderWidth int
  (dpy (pointer Display)) (win Window) (width unsigned-int))

(define-xlib-function X:MapWindow int
  (dpy (pointer Display)) (win Window))

(define-xlib-function X:UnmapWindow int
  (dpy (pointer Display)) (win Window))

(define-xlib-function X:MapRaised int
  (dpy (pointer Display)) (win Window))

(define-xlib-function X:RaiseWindow int
  (dpy (pointer Display)) (win Window))

(define-xlib-function X:LowerWindow int
  (dpy (pointer Display)) (win Window))

(define-xlib-function X:ReparentWindow int
  (dpy (pointer Display))
  (w Window) (parent Window)
  (x int) (y int))
  
(define-xlib-function X:DestroyWindow int
  (dpy (pointer Display)) (win Window))

(define-xlib-function X:DestroySubwindows int
  (dpy (pointer Display)) (win Window))

(define-xlib-function X:QueryTree int
  (dpy (pointer Display)) (win Window)
  (root-return (pointer Window)) (parent-return (pointer Window))
  (children-return (pointer (pointer Window)))
  (nchildren-return (pointer unsigned-int)))

(define-xlib-function X:InternAtom Atom
  (dpy (pointer Display)) (name c-string) (only-if-exists Bool))

(define-xlib-function X:GetAtomName (pointer char)
  (dpy (pointer Display)) (atom Atom))

(define-xlib-function X:GetWindowProperty int
  (dpy (pointer Display)) (w Window) (prop Atom)
  (off long) (len long) (delete Bool) (req-type Atom)
  (ac-type-return (pointer Atom)) (ac-format-return (pointer int))
  (nitems-return (pointer unsigned-long))
  (bytes-after-return (pointer unsigned-long))
  (prop-return (pointer (pointer unsigned-char))))

(define-xlib-function X:SetWMNormalHints void
  (dpy (pointer Display))
  (w Window) (hints (pointer XSizeHints)))

(define-xlib-function X:GetWMNormalHints int
  (dpy (pointer Display))
  (w Window) (hints_return (pointer XSizeHints))
  (supplied_return (pointer long)))

(define-xlib-function X:AllocWMHints (pointer XWMHints))

(define-xlib-function X:SetWMHints int
  (dpy (pointer Display)) (w Window) (pointer XWMHints))

(define-xlib-function X:GetWMHints (pointer XWMHints)
  (dpy (pointer Display)) (w Window))

(define-xlib-function X:SetWMProtocols int
  (dpy (pointer Display))
  (w Window) (protocols (pointer Atom))
  (count int))

(define-xlib-function X:GetWMProtocols int
  (dpy (pointer Display))
  (w Window) (protocols_return (pointer (pointer Atom)))
  (count_return (pointer int)))

(define-xlib-function X:SetWMName void
  (dpy (pointer Display))
  (w Window)
  (text_prop (pointer XTextProperty)))

(define-xlib-function X:StoreName int
  (dpy (pointer Display))
  (w Window)
  (window_name c-string))

(define-xlib-function X:FetchName int
  (dpy (pointer Display))
  (w Window)
  (window_name_return (pointer c-string)))

(define-xlib-function X:GetWMName int
  (dpy (pointer Display))
  (w Window)
  (text_prop_return (pointer XTextProperty)))

(define-xlib-function X:FreeStringList void
  (list (pointer (pointer char))))

(define-xlib-function X:GetCommand int
  (dpy (pointer Display))
  (w Window)
  (argv_return (pointer (pointer (pointer char))))
  (argc_return (pointer int)))

(define-xlib-function X:SetCommand int
  (dpy (pointer Display))
  (w Window)
  (argv (pointer c-string))
  (argc int))

(define-xlib-function X:GetTransientForHint int
  (dpy (pointer Display))
  (w Window)
  (prop_window_return (pointer Window)))

(define-xlib-function X:SetTransientForHint int
  (dpy (pointer Display))
  (w Window)
  (prop_window Window))

(define-xlib-function X:SetIconName int
  (dpy (pointer Display))
  (w Window)
  (icon_name c-string))

(define-xlib-function X:GetIconName int
  (dpy (pointer Display))
  (w Window)
  (icon_name_return (pointer c-string)))

(define-xlib-function X:AllocClassHint (pointer XClassHint))

(define-xlib-function X:SetClassHint void
  (dpy (pointer Display))
  (w Window)
  (class_hints (pointer XClassHint)))

(define-xlib-function X:GetClassHint int
  (dpy (pointer Display))
  (w Window)
  (class_hints_return (pointer XClassHint)))

(define-xlib-function X:ChangeProperty int
  (dpy (pointer Display)) (w Window) (prop Atom) (type Atom)
  (format int) (mode int) (data (pointer unsigned-char)) (nelements int))

(define-xlib-function X:DeleteProperty int
  (dpy (pointer Display)) (w Window) (prop Atom))

(define-xlib-function X:DoesBackingStore int
  (scr (pointer Screen)))

(define-xlib-function X:DoesSaveUnders int
  (scr (pointer Screen)))

(define-xlib-function X:DisableAccessControl int
  (dpy (pointer Display)))

(define-xlib-function X:DrawArc int
  (dpy (pointer Display))
  (d Drawable)
  (gc GC)
  (x int) (y int)
  (width unsigned-int)
  (height unsigned-int)
  (angle1 int) (angle2 int))

(define-xlib-function X:DrawArcs int
  (dpy (pointer Display))
  (d Drawable)
  (gc GC)
  (arcs (pointer XArc))
  (narcs int))

(define-xlib-function X:DrawImageString int
  (dpy (pointer Display))
  (d Drawable)
  (gc GC)
  (x int) (y int)
  (str c-string)
  (len int))

(define-xlib-function X:DrawLine int
  (dpy (pointer Display))
  (d Drawable)
  (gc GC)
  (x1 int) (y1 int)
  (x2 int) (y2 int))

(define-xlib-function X:DrawLines int
  (dpy (pointer Display))
  (d Drawable)
  (gc GC)
  (ponts (pointer XPoint))
  (npoints int)
  (mode int))

(define-xlib-function X:DrawPoint int
  (dpy (pointer Display))
  (d Drawable)
  (gc GC)
  (x int) (y int))

(define-xlib-function X:DrawPoints int
  (dpy (pointer Display))
  (d Drawable)
  (gc GC)
  (points (pointer XPoint))
  (npoints int)
  (mode int))

(define-xlib-function X:DrawRectangle int
  (dpy (pointer Display))
  (d Drawable)
  (gc GC)
  (x int) (y int)
  (width unsigned-int) (height unsigned-int))
  
(define-xlib-function X:DrawRectangles int
  (dpy (pointer Display))
  (d Drawable)
  (gc GC)
  (rects (pointer XRectangle))
  (nrects int))

(define-xlib-function X:DrawSegments int
  (dpy (pointer Display))
  (d Drawable)
  (gc GC)
  (segs (pointer XSegment))
  (nsegs int))

(define-xlib-function X:DrawString int
  (dpy (pointer Display))
  (d Drawable)
  (gc GC)
  (x int) (y int)
  (str c-string)
  (len int))

(define-xlib-function X:DrawText int
  (dpy (pointer Display))
  (d Drawable)
  (gc GC)
  (x int) (y int)
  (items (pointer XTextItem))
  (nitems int))

(define-xlib-function X:QueryTextExtents int
  (dpy (pointer Display))
  (font_id XID)
  (string c-string)
  (nchars int)
  (direction_return (pointer int))
  (font_ascent_return (pointer int))
  (font_descent_return (pointer int))
  (overall_return (pointer XCharStruct)))

(define-xlib-function X:EnableAccessControl int
  (dpy (pointer Display)))

(define-xlib-function X:EventsQueued int
  (dpy (pointer Display)) (mode int))

(define-xlib-function X:FillArc int
  (dpy (pointer Display))
  (d Drawable)
  (gc GC)
  (x int) (y int)
  (width unsigned-int) (height unsigned-int)
  (angle1 int) (angle2 int))

(define-xlib-function X:FillArcs int
  (dpy (pointer Display))
  (d Drawable)
  (gc GC)
  (arcs (pointer XArc))
  (narcs int))

(define-xlib-function X:FillPolygon int
  (dpy (pointer Display))
  (d Drawable)
  (gc GC)
  (ponts (pointer XPoint))
  (npoints int)
  (shape int)
  (mode int))

(define-xlib-function X:FillRectangle int
  (dpy (pointer Display))
  (d Drawable)
  (gc GC)
  (x int) (y int)
  (width unsigned-int) (height unsigned-int))

(define-xlib-function X:FillRectangles int
  (dpy (pointer Display))
  (d Drawable)
  (gc GC)
  (recs (pointer XRectangle))
  (nrecs int))

(define-xlib-function X:ClearArea int
  (dpy (pointer Display))
  (w Window)
  (x int)
  (y int)
  (width unsigned-int)
  (height unsigned-int)
  (exposures Bool))

(define-xlib-function X:Flush int
  (dpy (pointer Display)))

(define-xlib-function X:Sync int
  (dpy (pointer Display))
  (discard Bool))

(define-xlib-function X:Bell int
  (dpy (pointer Display)) (percent int))

(define-xlib-function X:NextEvent int
  (dpy (pointer Display)) (pevent (pointer XEvent)))

(define-xlib-function X:CheckTypedEvent int
  (dpy (pointer Display)) (type int) (pevent (pointer XEvent)))

(define-xlib-function X:Pending boolean
  (dpy (pointer Display)))

(define-xlib-function X:CreateGC GC
  (dpy (pointer Display))
  (d Drawable)
  (valuemask unsigned-long)
  (values (pointer XGCValues)))

(define-xlib-function X:ChangeGC int
  (dpy (pointer Display))
  (gc GC)
  (valuemask unsigned-long)
  (values (pointer XGCValues)))

(define-xlib-function X:FreeGC void
  (dpy (pointer Display))
  (gc GC))

(define-xlib-function X:GetGCValues int
  (dpy (pointer Display))
  (gc GC)
  (valuemask unsigned-long)
  (values_return XGCValues))

(define-xlib-function X:SelectInput int
  (dpy (pointer Display))
  (win Window)
  (event-mask long))

(define-xlib-function X:ScreenOfDisplay (pointer Screen)
  (dpy (pointer Display)) (scr_num int))

(defun X:RootWindow (dpy scr)
  (let ((sp (X:ScreenOfDisplay dpy scr)))
    (Screen->root sp)))

(defun X:WhitePixel (dpy scr)
  (let ((sp (X:ScreenOfDisplay dpy scr)))
    (Screen->white_pixel sp)))

(defun X:BlackPixel (dpy scr)
  (let ((sp (X:ScreenOfDisplay dpy scr)))
    (Screen->black_pixel sp)))

(defun X:DefaultColormap (dpy scr)
  (let ((sp (X:ScreenOfDisplay dpy scr)))
    (Screen->cmap sp)))

(defun X:DefaultVisual (dpy scr)
  (let ((sp (X:ScreenOfDisplay dpy scr)))
    (Screen->root_visual sp)))

(defun X:DefaultDepth (dpy scr)
  (let ((sp (X:ScreenOfDisplay dpy scr)))
    (Screen->root_depth sp)))

(define-xlib-function X:PutBackEvent int
  (dpy (pointer Display))
  (pevent (pointer XEvent)))

(define-ffi-function X:Enq (dpy wevent)
  "Enqueue wire event WEVENT to DPY's event queue."
  '(function void (pointer Display) (pointer void))
  "_XEnq")

;;; Font ops
(define-xlib-function X:LoadFont Font
  (dpy (pointer Display))
  (name c-string))

(define-xlib-function X:QueryFont (pointer XFontStruct)
  (dpy (pointer Display))
  (font_ID XID))

(define-xlib-function X:LoadQueryFont (pointer XFontStruct)
  (dpy (pointer Display))
  (name c-string))

(define-xlib-function X:FreeFont int
  (dpy (pointer Display))
  (font_struct (pointer XFontStruct)))

(define-xlib-function X:FreeFontInfo int
  (names pointer)
  (free_info (pointer XFontStruct))
  (actual_count int))

(define-xlib-function X:GetFontProperty Bool
  (font_struct (pointer XFontStruct))
  (atom Atom)
  (val_ret (pointer unsigned-long)))

(define-xlib-function X:UnloadFont int
  (dpy (pointer Display))
  (font Font))

(define-xlib-function X:TextWidth int
  (font_struct (pointer XFontStruct))
  (string c-string)
  (count int))


(define-xlib-function X:CreatePixmap Pixmap
  (dpy (pointer Display))
  (d Drawable)
  (width unsigned-int)
  (height unsigned-int)
  (depth unsigned-int))

(define-xlib-function X:FreePixmap int
  (dpy (pointer Display))
  (pixmap Pixmap))


(define-xlib-function X:PutImage int
  (dpy (pointer Display))
  (d Drawable)
  (gc GC)
  (img (pointer XImage))
  (src_x int)
  (src_y int)
  (dest_x int)
  (dest_y int)
  (width unsigned-int)
  (height unsigned-int))

(define-xlib-function X:GetImage (pointer XImage)
  (dpy (pointer Display))
  (d Drawable)
  (x int)
  (y int)
  (width unsigned-int)
  (height unsigned-int)
  (plane_mask unsigned-long)
  (format int))

(define-xlib-function X:DestroyImage int
  (pointer XImage))

(define-xlib-function X:Free int
  (data (pointer void)))

(define-xlib-function X:SetDashes int
  (dpy (pointer Display))
  (gc GC)
  (dash_offset int)
  (dash_list (pointer char))
  (n int))

(define-xlib-function X:SetClipRectangles int
  (dpy (pointer Display))
  (gc GC)
  (clip_x_origin int)
  (clip_y_origin int)
  (rects (pointer XRectangle))
  (nrects int)
  (ordering int))

(define-xlib-function X:SetClipMask int
  (dpy (pointer Display))
  (gc GC)
  (pixmap Pixmap))

(define-xlib-function X:GrabKeyboard int
  (dpy (pointer Display))
  (grab_window Window)
  (owner_events Bool)
  (pointer_mode int)
  (keyboard_mode int)
  (time Time))

(define-xlib-function X:UngrabKeyboard int
  (dpy (pointer Display))
  (time Time))

(define-xlib-function X:GrabPointer int
  (dpy (pointer Display))
  (grab_window Window)
  (owner_events Bool)
  (event_mask unsigned-int)
  (pointer_mode int)
  (keyboard_mode int)
  (confine_to Window)
  (cursor Cursor)
  (time Time))

(define-xlib-function X:UngrabPointer int
  (dpy (pointer Display))
  (time Time))

(define-xlib-function X:ChangeActivePointerGrab int
  (dpy (pointer Display))
  (event_mask unsigned-int)
  (cursor Cursor)
  (time Time))

(define-xlib-function X:QueryPointer Bool
  (dpy (pointer Display))
  (w Window)
  (root_return (pointer Window))
  (child_return (pointer Window))
  (root_x_return (pointer int))
  (root_y_return (pointer int))
  (win_x_return (pointer int))
  (win_y_return (pointer int))
  (mask_return (pointer unsigned-int)))

(define-xlib-function X:WarpPointer int
  (dpy (pointer Display))
  (src_w Window) (dest_w Window)
  (src_x int) (src_y int)
  (src_width unsigned-int) (src_height unsigned-int)
  (dest_x int) (dest_y int))

(define-xlib-function X:TranslateCoordinates Bool
  (dpy (pointer Display))
  (src_w Window) (dest_w Window)
  (src_x int) (src_y int)
  (dest_x_return (pointer int))
  (dest_y_return (pointer int))
  (child_return (pointer Window)))

(define-xlib-function X:CreateFontCursor Cursor
  (dpy (pointer Display))
  (shape unsigned-int))

(define-xlib-function X:CreatePixmapCursor Cursor
  (dpy (pointer Display))
  (source Pixmap) (mask Pixmap)
  (foreground_color (pointer XColor))
  (background_color (pointer XColor))
  (x unsigned-int) (y unsigned-int))

(define-xlib-function X:CreateGlyphCursor Cursor
  (dpy (pointer Display))
  (source_font Font)
  (mask_font Font)
  (source_char unsigned-int)
  (mask_char unsigned-int)
  (foreground_color (pointer XColor))
  (background_color (pointer XColor)))

(define-xlib-function X:RecolorCursor int
  (dpy (pointer Display))
  (cursor Cursor)
  (foreground_color (pointer XColor))
  (background_color (pointer XColor)))

(define-xlib-function X:FreeCursor int
  (dpy (pointer Display))
  (cursor Cursor))

(define-xlib-function X:ChangeSaveSet int
  (dpy (pointer Display))
  (w Window)
  (change_mode int))

(define-xlib-function X:AddToSaveSet int
  (dpy (pointer Display))
  (w Window))

(define-xlib-function X:RemoveFromSaveSet int
  (dpy (pointer Display))
  (w Window))

(define-xlib-function X:SetCloseDownMode int
  (dpy (pointer Display))
  (close_mode int))

(define-xlib-function X:KillClient int
  (dpy (pointer Display))
  (resource XID))

(define-xlib-function X:SendEvent int
  (dpy (pointer Display))
  (w Window)
  (propagate Bool)
  (event_mask long)
  (event_send (pointer XEvent)))

(define-xlib-function X:SetInputFocus int
  (dpy (pointer Display))
  (focus Window)
  (revert_to int)
  (time Time))

(define-xlib-function X:GrabButton int
  (dpy (pointer Display))
  (button unsigned-int)
  (modifiers unsigned-int)
  (grab_window Window)
  (owner_event Bool)
  (event_mask unsigned-int)
  (pointer_mode unsigned-int)
  (keyboard_mode unsigned-int)
  (confine_to Window)
  (cursor Cursor))

(define-xlib-function X:UngrabButton int
  (dpy (pointer Display))
  (button unsigned-int)
  (modifiers unsigned-int)
  (grab_window Window))

(define-xlib-function X:GrabKey int
  (dpy (pointer Display))
  (keycode unsigned-int)
  (modifiers unsigned-int)
  (grab_window Window)
  (owner_event Bool)
  (pointer_mode unsigned-int)
  (keyboard_mode unsigned-int))

(define-xlib-function X:UngrabKey int
  (dpy (pointer Display))
  (keycode unsigned-int)
  (modifiers unsigned-int)
  (grab_window Window))
  
(define-xlib-function X:GetModifierMapping (pointer XModifierKeymap)
  (dpy (pointer Display)))

(define-xlib-function X:FreeModifiermap void
  (modmap (pointer XModifierKeymap)))

(define-xlib-function X:GetKeyboardMapping (pointer KeySym)
  (dpy (pointer Display))
  (first_keycode KeyCode)
  (keycode_count int)
  (keysyms_per_keycode_return (pointer int)))

(define-xlib-function X:AllowEvents int
  (dpy (pointer Display))
  (event_mode int)
  (time Time))

(define-xlib-function X:GetInputFocus int
  (dpy (pointer Display))
  (focus_return (pointer Window))
  (revert_to_return (pointer int)))

(define-xlib-function X:CopyArea int
  (dpy (pointer Display))
  (src Drawable) (dest Drawable)
  (gc GC) (src_x int) (src_y int)
  (width unsigned-int) (height unsigned-int)
  (dest_x int) (dest_y int))

(define-xlib-function X:QueryExtension Bool
  (dpy (pointer Display))
  (name c-string)
  (mop (pointer int))
  (fev (pointer int))
  (fer (pointer int)))

(define-xlib-function X:SetSelectionOwner int
  (dpy (pointer Display))
  (selection Atom)
  (owner Window)
  (time Time))

(define-xlib-function X:GetSelectionOwner Window
  (dpy (pointer Display))
  (selection Atom))

(define-xlib-function X:ConvertSelection int
  (dpy (pointer Display))
  (selection Atom)
  (target Atom)
  (property Atom)
  (requestor Window)
  (time Time))

(define-xlib-function X:Synchronize int
  (dpy (pointer Display))
  (onoff Bool))

;; Misc
(cffi:defcfun ("XmbTextPropertyToTextList"
               XmbTextPropertyToTextList) int
               (display pointer)
               (text-prop pointer)
               (lr (pointer (pointer c-string)))
               (cnt (pointer int)))


(provide 'ffi-xlib)

;;; ffi-xlib.el ends here
