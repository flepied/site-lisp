;; 10mouse.el --- 
;;
;; Copyright (C) 1996, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: mouse.el,v 1.2 2003-07-04 13:20:02 flepied Exp $
;; Keywords: 
;;

;;=============================================================================
;; Autoloaded section.
;;=============================================================================
(autoload 'pasting-mouse-drag-secondary "mouse-extras"
  "Drag out a secondary selection, then paste it at the current point.

To test this function, evaluate:
	(global-set-key [M-down-mouse-1] 'pasting-mouse-drag-secondary)
put the point at one place, then click and drag over some other region."
  t)

(autoload 'moving-mouse-drag-secondary "mouse-extras"
  "Sweep out a secondary selection, then move it to the current point."
  t)

(autoload 'mouse-drag-throw "mouse-extras"
  "``Throw'' the page according to a mouse drag.

A ``throw'' is scrolling the page at a speed relative to the distance
from the original mouse click to the current mouse location.  Try it;
you'll like it.  It's easier to observe than to explain.

If the mouse is clicked and released in the same place of time we
assume that the user didn't want to scdebugroll but wanted to whatever
mouse-2 used to do, so we pass it through.

Throw scrolling was inspired (but is not identical to) the ``hand''
option in MacPaint, or the middle button in Tk text widgets.  Some of
the following code is derived from mouse-drag-region of emacs-19.28.

Some people think throw-scrolling should go in the opposite direction.
If you agree, (setq mouse-throw-with-scroll-bar t).

To test this function, evaluate:
    (global-set-key [down-mouse-2] 'mouse-drag-throw)"
  t)


;;;###

;;;***


;;=============================================================================
;; Configuration section.
;;=============================================================================

;;=============================================================================
;; mouse extra functionalities
;;=============================================================================
(global-set-key [M-down-mouse-1] 'pasting-mouse-drag-secondary)
(global-set-key [M-S-down-mouse-1] 'moving-mouse-drag-secondary)
(global-set-key [down-mouse-2] 'mouse-drag-throw)

(global-set-key [mouse-4] 'scroll-up)
(global-set-key [mouse-5] 'scroll-down)

;;; 10mouse.el ends here
