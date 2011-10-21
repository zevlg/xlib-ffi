;;; xlib-const.el --- Constants used in Xlib for masks and the like.

;; Copyright (C) 1996, 1997, 1998 Eric M. Ludlam
;; Copyright (C) 2003-2005 XWEM Org.
;;
;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;; Modified: Zajcev Evgeny <zevlg@yandex.ru>
;; Keywords: xlib, xwem
;; X-RCS: $Id: xlib-const.el,v 1.5 2004/11/29 19:48:18 lg Exp $
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;

;;; Commentary:
;;
;; Constants used with our X connection.

;;; Code:


(require 'xlib-version)

(defconst X-False 0 "False")
(defconst X-True 1 "True")

(defconst X-CopyFromParent 0 "CopyFromParent opcode.")
(defconst X-InputOutput 1 "InputOutput opcode.")
(defconst X-InputOnly 2 "InputOnly opcode.")

;;; Gravity

(defconst X-ForgetGravity 0 "Forget about bit gravity.")
(defconst X-NorthWestGravity 1 "NorthWest gravity.")
(defconst X-NorthGravity 2 "North gravity.")
(defconst X-NorthEastGravity 3 "NorthEast gravity.")
(defconst X-WestGravity 4 "West gravity.")
(defconst X-CenterGravity 5 "Center gravity.")
(defconst X-EastGravity 6 "East gravity.")
(defconst X-SouthWestGravity 7 "SouthWest gravity.")
(defconst X-SouthGravity 8 "South gravity.")
(defconst X-SouthEastGravity 9 "SouthEast gravity.")
(defconst X-StaticGravity 10 "Static gravity.")

;; Window gravity + bit gravity above

(defconst X-UnmapGravity 0 "Unmap gravity.")

;; backing store

(defconst X-NotUseful 0 "NotUseful backing store.")
(defconst X-WhenMapped 1 "WhenMapped backing store.")
(defconst X-Always 2 "Always backing store.")

;;; Event Masks

(defconst XM-NoEvent #x0 "No Event mask.")
(defconst XM-KeyPress #x1 "KeyPress bitmask.")
(defconst XM-KeyRelease #x2 "KeyRelease bitmask.")
(defconst XM-ButtonPress #x4 "ButtonPress bitmask.")
(defconst XM-ButtonRelease #x8 "ButtonRelease bitmask.")
(defconst XM-EnterWindow #x10 "EnterWindow bitmask.")
(defconst XM-LeaveWindow #x20 "LeaveWindow bitmask.")
(defconst XM-PointerMotion #x40 "PointerMotion bitmask.")
(defconst XM-PointerMotionHint #x80 "PointerMotionHint bitmask.")
(defconst XM-Button1Motion #x100 "Button2Motion bitmask.")
(defconst XM-Button2Motion #x200 "Button2Motion bitmask.")
(defconst XM-Button3Motion #x400 "Button3Motion bitmask.")
(defconst XM-Button4Motion #x800 "Button4Motion bitmask.")
(defconst XM-Button5Motion #x1000 "Button5Motion bitmask.")
(defconst XM-ButtonMotion #x2000 "Button bitmask.")
(defconst XM-KeymapState #x4000 "KeymapState bitmask.")
(defconst XM-Exposure #x8000 "Exposure bitmask.")
(defconst XM-VisibilityChange #x10000 "VisibilityChange bitmask.")
(defconst XM-StructureNotify #x20000 "StructureNotify bitmask.")
(defconst XM-ResizeRedirect #x40000 "ResizeRedirect bitmask.")
(defconst XM-SubstructureNotify #x80000 "SubstructureNotify bitmask.")
(defconst XM-SubstructureRedirect #x100000 "SubstructureRedirect bitmask.")
(defconst XM-FocusChange #x200000 "FocusChange bitmask.")
(defconst XM-PropertyChange #x400000 "PropertyChange bitmask.")
(defconst XM-ColormapChange #x800000 "ColormapChange bitmask.")
(defconst XM-OwnerGrabButton #x1000000 "OwnerGrabButton bitmask.")

;; Event OpCodes

(defconst X-SyntheticMask 128 "Mask the synthetic part off.")
(defconst X-KeyPress 2 "KeyPress event.")
(defconst X-KeyRelease 3 "KeyRelease event.")
(defconst X-ButtonPress 4 "ButtonPress event.")
(defconst X-ButtonRelease 5 "ButtonRelease event.")
(defconst X-MotionNotify 6 "MotionNotify event.")
(defconst X-EnterNotify 7 "EnterNotify event.")
(defconst X-LeaveNotify 8 "LeaveNotify event.")
(defconst X-FocusIn 9 "FocusIn event.")
(defconst X-FocusOut 10 "FocusOut event.")
(defconst X-KeymapNotify 11 "KeymapNotify event.")
(defconst X-Expose 12 "Expose event.")
(defconst X-GraphicsExpose 13 "GraphicsExpose event.")
(defconst X-NoExpose 14 "NoExpose event.")
(defconst X-VisibilityNotify 15 "VisibilityNotify event.")
(defconst X-CreateNotify 16 "CreateNotify event.")
(defconst X-DestroyNotify 17 "DestroyNotify event.")
(defconst X-UnmapNotify 18 "UnmapNotify event.")
(defconst X-MapNotify 19 "MapNotify event.")
(defconst X-MapRequest 20 "MapRequest event.")
(defconst X-ReparentNotify 21 "ReparentNotify event.")
(defconst X-ConfigureNotify 22 "ConfigureNotify event.")
(defconst X-ConfigureRequest 23 "ConfigureRequest event.")
(defconst X-GravityNotify 24 "GravityNotify event.")
(defconst X-ResizeRequest 25 "ResizeRequest event.")
(defconst X-CirculateNotify 26 "CirculateNotify event.")
(defconst X-CirculateRequest 27 "CirculateRequest event.")
(defconst X-PropertyNotify 28 "PropertyNotify event.")
(defconst X-SelectionClear 29 "SelectionClear event.")
(defconst X-SelectionRequest 30 "SelectionRequest event.")
(defconst X-SelectionNotify 31 "SelectionNotify event.")
(defconst X-ColormapNotify 32 "ColormapNotify event.")
(defconst X-ClientMessage 33 "ClientMessage event.")
(defconst X-MappingNotify 34 "MappingNotify event.")
(defconst X-MaxEvent 35 "1 greater than the largest event opcode.")

;; Properties
(defconst X-PropertyNewValue 0)
(defconst X-PropertyDelete 1)

;;; Stacking constants
(defconst X-Above 0 "Stacking mode Above.")
(defconst X-Below 1 "Stacking mode Below.")
(defconst X-TopIf 2 "Stacking mode TopIf.")
(defconst X-BottomIf 3 "Stacking mode BottomIf.")
(defconst X-Opposite 4 "Stacking mode Opposite.")

;;; Atom format
(defconst X-format-8 8 "8 bit formatting for Atoms.")
(defconst X-format-16 16 "16 bit formatting for Atoms.")
(defconst X-format-32 32 "32 bit formatting for Atoms.")

;;; Property Modes for atoms
(defconst X-PropModeReplace 0 "Property Mode Replace")
(defconst X-PropModePrepend 1 "Property Mode Prepend")
(defconst X-PropModeAppend  2 "Property Mode Append")

;;; KeyButtonMask

(defconst X-Shift #x1 "Shift bitmask.")
(defconst X-Lock #x2 "Lock bitmask.")
(defconst X-Control #x4 "Control bitmask.")
(defconst X-Mod1 #x8 "Mod1 bitmask.")
(defconst X-Mod2 #x10 "Mod2 bitmask.")
(defconst X-Mod3 #x20 "Mod3 bitmask.")
(defconst X-Mod4 #x40 "Mod4 bitmask.")
(defconst X-Mod5 #x80 "Mod5 bitmask.")
(defconst X-Button1 #x100 "Button1 bitmask.")
(defconst X-Button2 #x200 "Button2 bitmask.")
(defconst X-Button3 #x400 "Button3 bitmask.")
(defconst X-Button4 #x800 "Button4 bitmask.")
(defconst X-Button5 #x1000 "Button5 bitmask.")

(defconst X-XButton1 1)
(defconst X-XButton2 2)
(defconst X-XButton3 3)
(defconst X-XButton4 4)
(defconst X-XButton5 5)
;;; Graphic context stuff
;;

;;; functions
(defconst X-GXClear 0 "GC function type id.")
(defconst X-GXAnd 1 "GC function type id.")
(defconst X-GXAndReverse 2 "GC function type id.")
(defconst X-GXCopy 3 "GC function type id.")
(defconst X-GXAndInverted 4 "GC function type id.")
(defconst X-GXNoOp 5 "GC function type id.")
(defconst X-GXXor 6 "GC function type id.")
(defconst X-GXOr 7 "GC function type id.")
(defconst X-GXNor 8 "GC function type id.")
(defconst X-GXEquiv 9 "GC function type id.")
(defconst X-GXInvert 10 "GC function type id.")
(defconst X-GXOrReverse 11 "GC function type id.")
(defconst X-GXCopyInverted 12 "GC function type id.")
(defconst X-GXOrInverted 13 "GC function type id.")
(defconst X-GXNand 14 "GC function type id.")
(defconst X-GXSet 15 "GC function type id.")

;; line styles
(defconst X-LineSolid 0 "GC line-style.")
(defconst X-LineOnOffDash 1 "GC line-style.")
(defconst X-LineDoubleDash 2 "GC line-style.")

;; cap-styles
(defconst X-CapNotLast 0 "GC cap-styles.")
(defconst X-CapButt 1 "GC cap-styles.")
(defconst X-CapRound 2 "GC cap-styles.")
(defconst X-CapProjecting 3 "GC cap-styles.")

;; join styles
(defconst X-JoinMiter 0 "GC join-style.")
(defconst X-JoinRound 1 "GC join-style.")
(defconst X-JoinBevel 2 "GC join-style.")

;; fill style
(defconst X-FillSolid 0 "GC fill-style.")
(defconst X-FillTiled 1 "GC fill-style.")
(defconst X-FillStippled 2 "GC fill-style.")
(defconst X-FillOpaqueStippled 3 "GC fill-style.")

;; fill rule
(defconst X-EvenOddRule 0 "GC fill-rule.")
(defconst X-WindingRule 1 "GC fill-rule.")

;; arc-mode
(defconst X-ArcChord 0 "GC arc mode.")
(defconst X-ArcPieSlice 1 "GC arc mode.")

;; Subwindow mode
(defconst X-ClipByChildren 0 "GC subwindow-mode.")
(defconst X-IncludeInferiors 1 "GC subwindow-mode.")

;; XSetClipRectangles ordering
(defconst X-UnSorted 0 "Unsorted list.")
(defconst X-YSorted 1 "Sorted by Y.")
(defconst X-YXSorted 2 "Sorted by X and Y.")
(defconst X-YXBanded 3)

;; Imaging
(defconst X-XYBitmap 0)			; depth 1, XYFormat
(defconst X-XYPixmap 1)			; depth == drawable depth
(defconst X-ZPixmap 2)			; depth == drawable depth

;;; Some color type stuff
;;
(defconst X-AllocNone 0 "No color entries writable.")
(defconst X-AllocAll  1 "All color entries writable.")

(defconst X-DoRed 1 "Do Red mask.")
(defconst X-DoGreen 2 "Do Green mask.")
(defconst X-DoBlue 4 "Do blue mask.")
(defconst X-DoRedGreenBlue 7 "All Color Dos ored together.")

;;; Poling coordinate mode
(defconst X-Origin 0 "Specifies point drawn with relation to origin.")
(defconst X-Previous 1 "Specifies points draw with relation to previous point.")

;;; Filling shapes
(defconst X-Complex 0)
(defconst X-Nonconvex 1)
(defconst X-Convex 2)

;; Misc
(defconst X-None 0 "universal null resource or null atom")

(defconst X-RevertToNone 0 "for XSetInputFocus")
(defconst X-RevertToPointerRoot 1 "for XSetInputFocus")
(defconst X-RevertToParent 2 "for XSetInputFocus")

(defconst X-ParentRelative 1
  "Background pixmap in CreateWindow and ChangeWindowAttributes.")
(defconst X-CopyFromParent 0
  "Border pixmap in CreateWindow and ChangeWindowAttributes special
  VisualID and special window class passed to CreateWindow.")
(defconst X-PointerWindow 0 "destination window in SendEvent")
(defconst X-InputFocus 1 "destination window in SendEvent")
(defconst X-PointerRoot	1 "focus window in SetInputFocus")
(defconst X-AnyPropertyType 0 "special Atom, passed to GetProperty")
(defconst X-AnyKey 0 "special Key Code, passed to GrabKey")
(defconst X-AnyButton 0 "special Button Code, passed to GrabButton")
(defconst X-AllTemporary 0 "special Resource ID passed to KillClient")
(defconst X-CurrentTime	0 "special Time")
(defconst X-NoSymbol 0 "special KeySym")

(defconst X-GrabModeSync 0 "specific mode")
(defconst X-GrabModeAsync 1 "specific mode")

(defconst X-AllPlanes -1 "Mask for all planes in XGetImage.")

;; AllowEvents modes
(defconst X-AsyncPointer 0)
(defconst X-SyncPointer 1)
(defconst X-ReplayPointer 2)
(defconst X-AsyncKeyboard 3)
(defconst X-SyncKeyboard 4)
(defconst X-ReplayKeyboard 5)
(defconst X-AsyncBoth 6)
(defconst X-SyncBoth 7)

;; For window Attributes
(defconst X-CWBackPixmap #x1)
(defconst X-CWBackPixel #x2)
(defconst X-CWBorderPixmap #x4)
(defconst X-CWBorderPixel #x8)
(defconst X-CWBitGravity #x10)
(defconst X-CWWinGravity #x20)
(defconst X-CWBackingStore #x40)
(defconst X-CWBackingPlanes #x80)
(defconst X-CWBackingPixel #x100)
(defconst X-CWOverrideRedirect #x200)
(defconst X-CWSaveUnder #x400)
(defconst X-CWEventMask #x800)
(defconst X-CWDontPropagate #x1000)
(defconst X-CWColormap #x2000)
(defconst X-CWCursor #x4000)

;; used in ChangeSaveSet
(defconst X-SetModeInsert 0)
(defconst X-SetModeDelete 1)

;; used in ConfigureWindow
(defconst X-CWX #x1)
(defconst X-CWY #x2)
(defconst X-CWWidth #x4)
(defconst X-CWHeight #x8)
(defconst X-CWBorderWidth #x10)
(defconst X-CWSibling #x20)
(defconst X-CWStackMode #x40)

;;; Cursors
(defconst X-XC-num_glyphs 154)
(defconst X-XC-X_cursor 0)
(defconst X-XC-arrow 2)
(defconst X-XC-based_arrow_down 4)
(defconst X-XC-based_arrow_up 6)
(defconst X-XC-boat 8)
(defconst X-XC-bogosity 10)
(defconst X-XC-bottom_left_corner 12)
(defconst X-XC-bottom_right_corner 14)
(defconst X-XC-bottom_side 16)
(defconst X-XC-bottom_tee 18)
(defconst X-XC-box_spiral 20)
(defconst X-XC-center_ptr 22)
(defconst X-XC-circle 24)
(defconst X-XC-clock 26)
(defconst X-XC-coffee_mug 28)
(defconst X-XC-cross 30)
(defconst X-XC-cross_reverse 32)
(defconst X-XC-crosshair 34)
(defconst X-XC-diamond_cross 36)
(defconst X-XC-dot 38)
(defconst X-XC-dotbox 40)
(defconst X-XC-double_arrow 42)
(defconst X-XC-draft_large 44)
(defconst X-XC-draft_small 46)
(defconst X-XC-draped_box 48)
(defconst X-XC-exchange 50)
(defconst X-XC-fleur 52)
(defconst X-XC-gobbler 54)
(defconst X-XC-gumby 56)
(defconst X-XC-hand1 58)
(defconst X-XC-hand2 60)
(defconst X-XC-heart 62)
(defconst X-XC-icon 64)
(defconst X-XC-iron_cross 66)
(defconst X-XC-left_ptr 68)
(defconst X-XC-left_side 70)
(defconst X-XC-left_tee 72)
(defconst X-XC-leftbutton 74)
(defconst X-XC-ll_angle 76)
(defconst X-XC-lr_angle 78)
(defconst X-XC-man 80)
(defconst X-XC-middlebutton 82)
(defconst X-XC-mouse 84)
(defconst X-XC-pencil 86)
(defconst X-XC-pirate 88)
(defconst X-XC-plus 90)
(defconst X-XC-question_arrow 92)
(defconst X-XC-right_ptr 94)
(defconst X-XC-right_side 96)
(defconst X-XC-right_tee 98)
(defconst X-XC-rightbutton 100)
(defconst X-XC-rtl_logo 102)
(defconst X-XC-sailboat 104)
(defconst X-XC-sb_down_arrow 106)
(defconst X-XC-sb_h_double_arrow 108)
(defconst X-XC-sb_left_arrow 110)
(defconst X-XC-sb_right_arrow 112)
(defconst X-XC-sb_up_arrow 114)
(defconst X-XC-sb_v_double_arrow 116)
(defconst X-XC-shuttle 118)
(defconst X-XC-sizing 120)
(defconst X-XC-spider 122)
(defconst X-XC-spraycan 124)
(defconst X-XC-star 126)
(defconst X-XC-target 128)
(defconst X-XC-tcross 130)
(defconst X-XC-top_left_arrow 132)
(defconst X-XC-top_left_corner 134)
(defconst X-XC-top_right_corner 136)
(defconst X-XC-top_side 138)
(defconst X-XC-top_tee 140)
(defconst X-XC-trek 142)
(defconst X-XC-ul_angle 144)
(defconst X-XC-umbrella 146)
(defconst X-XC-ur_angle 148)
(defconst X-XC-watch 150)
(defconst X-XC-xterm 152)

;; Some keys
(require 'xlib-keyconst)
;; Compat
(defconst XK-NumLock XK-Num-Lock)
(defconst XK-Space XK-space)

;; Window states
(defconst X-WithdrawnState 0.0)
(defconst X-NormalState 1.0)
(defconst X-IconicState 3.0)

;; Notify mode
(defconst X-NotifyNormal 0)
(defconst X-NotifyGrab 1)
(defconst X-NotifyUngrab 2)
(defconst X-NotifyWhileGrabbed 3)

(defconst X-NotifyHint 1)		; for MotionNotify
		       
;; Notify detail
(defconst X-NotifyAncestor 0)
(defconst X-NotifyVirtual 1)
(defconst X-NotifyInferior 2)
(defconst X-NotifyNonlinear 3)
(defconst X-NotifyNonlinearVirtual 4)
(defconst X-NotifyPointer 5)
(defconst X-NotifyPointerRoot 6)
(defconst X-NotifyDetailNone 7)

;; Used in GetWindowAttributes reply
(defconst X-Unmapped 0)
(defconst X-Unviewable 1)
(defconst X-Viewable 2)

;; Visibility notify
(defconst X-VisibilityUnobscured 0)
(defconst X-VisibilityPartiallyObscured 1)
(defconst X-VisibilityFullyObscured 2)

;; Circulation request
(defconst X-PlaceOnTop 0)
(defconst X-PlaceOnBottom 1)

;; Protocol families
(defconst X-FamilyInternet 0)
(defconst X-FamilyDECnet 1)
(defconst X-FamilyChaos 2)

;; Byte order
(defconst X-LSBFirst 0)
(defconst X-MSBFirst 1)

;; X errors
(defconst X-BadRequest 1)
(defconst X-BadValue 2)
(defconst X-BadWindow 3)
(defconst X-BadPixmap 4)
(defconst X-BadAtom 5)
(defconst X-BadCursor 6)
(defconst X-BadFont 7)
(defconst X-BadMatch 8)
(defconst X-BadDrawable 9)
(defconst X-BadAccess 10)
(defconst X-BadAlloc 11)
(defconst X-BadColor 12)
(defconst X-BadGC 13)
(defconst X-BadIDChoice 14)
(defconst X-BadName 15)
(defconst X-BadLength 16)
(defconst X-BadImplementation 17)
(defconst X-BadFirstExtension 128)
(defconst X-BadLastExtension 255)


(provide 'xlib-const)

;;; xlib-const.el ends here
