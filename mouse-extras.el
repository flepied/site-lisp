
;;;
;;; mouse-extras.el
;;; Copyright (C) 1994-1996 by John Heidemann <johnh@isi.edu>
;;; $Id: mouse-extras.el,v 2.21 1996/02/13 19:38:07 johnh Exp $
;;;
;;; A description of this package follows the copyright notice below.
;;; This file is not part of GNU emacs
;;; (but could be if people are interested).
;;;

;;;
;;; COPYRIGHT NOTICE
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of version 2 of the GNU General Public
;;; License as published by the Free Software Foundation.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;;; for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.
;;;

;;;
;;; LCD Archive Entry:
;;; mouse-extras|John Heidemann|johnh@isi.edu|
;;; ``one-click'' mouse commands to scroll and copy/move text|
;;; $Date: 1996/02/13 19:38:07 $|$Revision: 2.21 $|~/misc/mouse-extras|
;;;
;;; Mouse-extras is also available on the web at
;;; <http://ficus-www.cs.ucla.edu/ficus-members/johnh/software.html>.
;;;

;;;
;;; What's new with mouse-extras 2.21?
;;;
;;; - support for emacs-19.{29,30}
;;; - point now stays on the visible screen during horizontal scrolling
;;;   (bug identified and fix suggested by Tom Wurgler <twurgler@goodyear.com>)
;;; - better work-around for lost-mouse-events bug (supports double/triple
;;;   clicks), see \\[mouse-extras-work-around-drag-bug] for details.
;;; - work-around for lost-mouse-events bug now is OFF by default;
;;;   enable it if you have problems
;;;
;;;
;;; What is ``mouse-extras.el''?
;;;
;;; First, it provides one-click text copy and move.  Rather than the
;;; standard stroke-out-a-region (down-mouse-1, up-mouse-1) followed
;;; by a yank (down-mouse-2, up-mouse-2 or C-y), you can now stroke
;;; out a region and have it automatically pasted at the current
;;; point.  You can also move text just as easily.  Although the
;;; difference may not sound like much, it does make mousing text
;;; around a lot easier, IMHO.
;;;
;;; Now that you're doing things with the mouse, doesn't that scroll
;;; bar seem far away when you want to scroll?  I also overload
;;; mouse-2 to do ``throw'' scrolling.  You click and drag.  The
;;; distance you move from your original click turns into a scroll
;;; amount.  The scroll amount is scaled exponentially to make both
;;; large moves and short adjustments easy.  What this boils down to
;;; is that you can easily scroll around the buffer without much mouse
;;; movement.  Finally, clicks which aren't drags are passed off to
;;; the old mouse-2 binding, so old mouse-2 operations (find-file in
;;; dired-mode, yanking in most other modes) still work.
;;;
;;; Third, we provide an alternative way to scroll, ``drag''
;;; scrolling.  You can click on a character and then drag it around,
;;; scrolling the buffer with you.  The character always stays under
;;; the mouse.  Compared to throw-scrolling, this approach provides
;;; direct manipulation (nice) but requires more mouse movement
;;; (unfortunate).  It is offered as an alternative for those who
;;; prefer it.
;;;
;;; To use mouse-extras, place the following in your .emacs file:
;;;	(require 'mouse-extras)
;;;     (global-set-key [M-down-mouse-1] 'pasting-mouse-drag-secondary)
;;;     (global-set-key [M-S-down-mouse-1] 'moving-mouse-drag-secondary)
;;; -and either-
;;;     (global-set-key [down-mouse-2] 'mouse-drag-throw)
;;; -or-
;;;     (global-set-key [down-mouse-2] 'mouse-drag-drag)
;;;
;;; (These definitions override the old binding of M-mouse-1 to
;;; mouse-drag-secondary.  I find I don't use that command much so its
;;; loss is not important, and it can be made up with a M-mouse-1
;;; followed by a M-mouse-3.  I personally reserve M-mouse bindings
;;; for my window manager and bind everything to C-mouse.)
;;;
;;;
;;; Options:
;;;
;;; - reverse the throw-scroll direction with \\[mouse-throw-with-scroll-bar]
;;; - work around a bug with \\[mouse-extras-work-around-drag-bug]
;;;
;;;
;;; History and related work:
;;;
;;; One-click copying and moving was inspired by lemacs-19.8.
;;; Throw-scrolling was inspired by MacPaint's ``hand'' and by Tk's
;;; mouse-2 scrolling.  The package mouse-scroll.el by Tom Wurgler
;;; <twurgler@goodyear.com> is similar to mouse-drag-throw, but
;;; doesn't pass clicks through.
;;;
;;; These functions have been tested in emacs version 19.30,
;;; and this package has run in the past on 19.25-19.29.
;;;
;;;
;;; Known Bugs:
;;;
;;; - Highlighting is sub-optimal under 19.29 and XFree86-3.1.1
;;;   (see \\[mouse-extras-work-around-drag-bug] for details).
;;; - Pasting-mouse-drag-secondary and moving-mouse-drag-secondary
;;;   require X11R5 (or better) and so fail under older versions
;;;   of Open Windows (like that present in Solaris/x86 2.1).
;;;
;;;
;;; Future plans:
;;;
;;; I read about the chording features of Plan-9's Acme environment at
;;; <http://swifty.dap.csiro.au/%7Ecameron/wily/auug.html>.  I'd like
;;; to incorporate some of these ideas into mouse-extras.  The only
;;; lose is that this is not the current Emacs Way Of Doing Things, so
;;; there would be a learning curve for existing emacs users.
;;;
;;;
;;; Thanks:
;;;
;;; Thanks to Kai Grossjohann
;;; <grossjoh@dusty.informatik.uni-dortmund.de> for reporting bugs, to
;;; Tom Wurgler <twurgler@goodyear.com> for reporting bugs and
;;; suggesting fixes, and to Joel Graber <jgraber@ti.com> for
;;; prompting me to do drag-scrolling and for an initial
;;; implementation of horizontal drag-scrolling.
;;;
;;;    -johnh, 13-Feb-96
;;;



;;
;; move/paste code
;;

(defvar pasting-mouse-last-paste-start nil
  "Internal to \\[pasting-mouse-drag-secondary].")
(defvar pasting-mouse-last-paste-end nil
  "Internal to \\[pasting-mouse-drag-secondary].")

(defvar mouse-extras-have-drag-bug nil
  "Set to enable mouse-extras-work-around-drag-bug.
See \\[mouse-extras-work-around-drag-bug] for details.")

(defun mouse-extras-work-around-drag-bug (start-event end-event)
  "Code to work around a bug in post-19.29 emacs:  it drops mouse-drag events.
The problem occurs under XFree86-3.1.1 (X11R6pl11) but not under X11R5,
and under post-19.29 but not early versions of emacs.

19.29 and 19.30 seems to drop mouse drag events
sometimes. (Reproducable under XFree86-3.1.1 (X11R6pl11) and
XFree86-3.1.2 under Linux 1.2.x.  Doesn't occur under X11R5 and SunOS
4.1.1.)

To see if you have the problem:
Disable this routine (with (setq mouse-extras-have-drag-bug nil))..
Click and drag for a while.
If highlighting stops tracking, you have the bug.
If you have the bug (or the real fix :-), please let me know."

  ;; To work-around, call mouse-set-secondary with a fake
  ;; drag event to set the overlay,
  ;; the load the x-selection.
  (save-excursion
    (let*
	((start-posn (event-start start-event))
	 (end-posn (event-end end-event))
	 (end-buffer (window-buffer (posn-window end-posn)))
	 ;; First, figure out the region (left as point/mark).
	 (range (progn
		  (set-buffer end-buffer)
		  (mouse-start-end (posn-point start-posn)
				 (posn-point end-posn)
				 (1- (event-click-count start-event)))))
	 (beg (car range))
	 (end (car (cdr range))))
      ;; Second, set the overlay.
      (if mouse-secondary-overlay
	  (move-overlay mouse-secondary-overlay beg end)
	(setq mouse-secondary-overlay (make-overlay beg (posn-point end))))
      (overlay-put mouse-secondary-overlay 'face 'secondary-selection)
      ;; Third, set the selection.
      ;; (setq me-beg beg me-end end me-range range)  ; for debugging
      (set-buffer end-buffer)
      (x-set-selection 'SECONDARY (buffer-substring beg end)))))

    
(defun pasting-mouse-drag-secondary (start-event)
  "Drag out a secondary selection, then paste it at the current point.

To test this function, evaluate:
	(global-set-key [M-down-mouse-1] 'pasting-mouse-drag-secondary)
put the point at one place, then click and drag over some other region."
  (interactive "e")
  ;; Work-around: We see and react to each part of a multi-click event
  ;; as it proceeds.  For a triple-event, this means the double-event
  ;; has already copied something that the triple-event will re-copy
  ;; (a Bad Thing).  We therefore undo the prior insertion if we're on
  ;; a multiple event.
  (if (and pasting-mouse-last-paste-start
	   (>= (event-click-count start-event) 2))
      (delete-region pasting-mouse-last-paste-start
		     pasting-mouse-last-paste-end))

  ;; HACK: We assume that mouse-drag-secondary returns nil if
  ;; there's no secondary selection.  This assumption holds as of
  ;; emacs-19.22 but is not documented.  It's not clear that there's
  ;; any other way to get this information.
  (if (mouse-drag-secondary start-event)
      (progn
	(if mouse-extras-have-drag-bug
	    (mouse-extras-work-around-drag-bug start-event last-input-event))
	;; Remember what we do so we can undo it, if necessary.
	(setq pasting-mouse-last-paste-start (point))
	(insert (x-get-selection 'SECONDARY))
	(setq pasting-mouse-last-paste-end (point)))
    (setq pasting-mouse-last-paste-start nil)))
  

(defun mouse-kill-preserving-secondary ()
  "Kill the text in the secondary selection, but leave the selection set.

This command is like \\[mouse-kill-secondary] (that is, the secondary
selection is deleted and placed in the kill ring), except that it also
leaves the secondary buffer active on exit.

This command was derived from mouse-kill-secondary in emacs-19.28
by johnh@ficus.cs.ucla.edu."
  (interactive)
  (let* ((keys (this-command-keys))
	 (click (elt keys (1- (length keys)))))
    (or (eq (overlay-buffer mouse-secondary-overlay)
	    (if (listp click)
		(window-buffer (posn-window (event-start click)))
	      (current-buffer)))
	(error "Select or click on the buffer where the secondary selection is")))
  (save-excursion
    (set-buffer (overlay-buffer mouse-secondary-overlay))
    (kill-region (overlay-start mouse-secondary-overlay)
		 (overlay-end mouse-secondary-overlay)))
  ;; (delete-overlay mouse-secondary-overlay)
  ;; (x-set-selection 'SECONDARY nil)
  ;; (setq mouse-secondary-overlay nil)
)

(defun moving-mouse-drag-secondary (start-event)
  "Sweep out a secondary selection, then move it to the current point."
  (interactive "e")
  ;; HACK:  We assume that mouse-drag-secondary returns nil if
  ;; there's no secondary selection.  This works as of emacs-19.22.
  ;; It's not clear that there's any other way to get this information.
  (if (mouse-drag-secondary start-event)
      (progn
	(mouse-kill-preserving-secondary)
	(insert (x-get-selection 'SECONDARY))))
)



;;
;; scrolling code
;;

(defun safe-scroll (row-delta &optional col-delta)
  "* Scroll down ROW-DELTA lines and right COL-DELTA, ignoring buffer edge errors.
Keep the cursor on the screen as needed."
  (if (and row-delta
	   (/= 0 row-delta))
      (condition-case nil   ;; catch and ignore movement errors
	  (scroll-down row-delta)
	(beginning-of-buffer (message "Beginning of buffer"))
	(end-of-buffer (message "End of buffer"))))
  (if (and col-delta
	   (/= 0 col-delta))
      (progn
	(scroll-right col-delta)
	;; Make sure that the point stays on the visible screen
	;; (if truncation-lines in set).
	;; This code mimics the behavior we automatically get 
	;; when doing vertical scrolling.
	;; Problem identified and a fix suggested by Tom Wurgler.
	(cond
	 ((< (current-column) (window-hscroll))
	  (move-to-column (window-hscroll))) ; make on left column
	 ((> (- (current-column) (window-hscroll) (window-width) -2) 0)
	  (move-to-column (+ (window-width) (window-hscroll) -3)))))))

(defun repeatedly-safe-scroll (row-delta &optional col-delta)
  "* Scroll ROW-DELTA rows and COL-DELTA cols until an event happens."
  (while (sit-for mouse-scroll-delay)
    (safe-scroll row-delta col-delta)))

(defun events-are-point-events-p (start-posn end-posn)
  "* Determine if START-POSN and END-POSN are ``close''."
  (let*
      ((start-col-row (posn-col-row start-posn))
       (end-col-row (posn-col-row end-posn)))
    (and
;; We no longer exclude things by time.
;;     (< (- (posn-timestamp end-posn) (posn-timestamp start-posn))
;;	(if (numberp double-click-time)
;;	    (* 2 double-click-time)   ;; stretch it a little
;;	  999999)) ;; non-numeric => check by position alone
     (= (car start-col-row) (car end-col-row))
     (= (cdr start-col-row) (cdr end-col-row)))))

(defun should-do-col-scrolling ()
  "* Determine if it's wise to enable col-scrolling for the current window."
  (or truncate-lines
      (> (window-hscroll (selected-window)) 0)
      (< (window-width) (screen-width))))

(defvar mouse-throw-with-scroll-bar nil
  "* Set direction of mouse-throwing.
If nil, the text moves in the direction the mouse moves.
If t, the scroll bar moves in the direction the mouse moves.")
(defconst mouse-throw-magnifier-with-scroll-bar 
      [-16 -8 -4 -2 -1 0 0 0  1  2  4  8  16])
(defconst mouse-throw-magnifier-with-mouse-movement
      [ 16  8  4  2  1 0 0 0 -1 -2 -4 -8 -16])
(defconst mouse-throw-magnifier-min -6)
(defconst mouse-throw-magnifier-max 6)

(defun mouse-drag-throw (start-event)
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
  (interactive "e")
  ;; we want to do save-selected-window, but that requires 19.29
  (let* ((start-posn (event-start start-event))
	 (start-window (posn-window start-posn))
	 (start-row (cdr (posn-col-row start-posn)))
	 (start-col (car (posn-col-row start-posn)))
	 (old-selected-window (selected-window))
	 event end row mouse-delta scroll-delta
	 have-scrolled point-event-p old-binding
	 window-last-row
	 col mouse-col-delta window-last-col
	 (scroll-col-delta 0)
	 adjusted-mouse-col-delta
	 ;; be conservative about allowing horizontal scrolling
	 (col-scrolling-p (should-do-col-scrolling)))
    (select-window start-window)
    (track-mouse
      (while (progn
	       (setq event (read-event)
		     end (event-end event)
		     row (cdr (posn-col-row end))
		     col (car (posn-col-row end)))
	       (or (mouse-movement-p event)
		   (eq (car-safe event) 'switch-frame)))
	(if (eq start-window (posn-window end))
	    (progn
	      (setq mouse-delta (- start-row row)
		    adjusted-mouse-delta
		    (- (cond
			((<= mouse-delta mouse-throw-magnifier-min)
			 mouse-throw-magnifier-min)
			((>= mouse-delta mouse-throw-magnifier-max)
			 mouse-throw-magnifier-max)
			(t mouse-delta))
		       mouse-throw-magnifier-min)
		    scroll-delta (aref (if mouse-throw-with-scroll-bar
					   mouse-throw-magnifier-with-scroll-bar
					 mouse-throw-magnifier-with-mouse-movement)
				       adjusted-mouse-delta))
	      (if col-scrolling-p
		  (setq mouse-col-delta (- start-col col)
			adjusted-mouse-col-delta
			(- (cond
			    ((<= mouse-col-delta mouse-throw-magnifier-min)
			     mouse-throw-magnifier-min)
			    ((>= mouse-col-delta mouse-throw-magnifier-max)
			     mouse-throw-magnifier-max)
			    (t mouse-col-delta))
			   mouse-throw-magnifier-min)
			scroll-col-delta (aref (if mouse-throw-with-scroll-bar
						   mouse-throw-magnifier-with-scroll-bar
						 mouse-throw-magnifier-with-mouse-movement)
					       adjusted-mouse-col-delta)))))
	(if (or (/= 0 scroll-delta)
		(/= 0 scroll-col-delta))
	    (progn
	      (setq have-scrolled t)
	      (safe-scroll scroll-delta scroll-col-delta)
	      (repeatedly-safe-scroll scroll-delta scroll-col-delta))))) ;xxx
    ;; If it was a click and not a drag, prepare to pass the event on.
    ;; Note:  We must determine the pass-through event before restoring
    ;; the window, but invoke it after.  Sigh.
    (if (and (not have-scrolled)
	     (events-are-point-events-p start-posn end))
	(setq point-event-p t
	      old-binding (key-binding
			   (vector (event-basic-type start-event)))))
    ;; Now restore the old window.
    (select-window old-selected-window)
    ;; For clicks, call the old function.
    (if point-event-p
	(call-interactively old-binding))))

(defun mouse-drag-drag (start-event)
  "``Drag'' the page according to a mouse drag.

Drag scrolling moves the page according to the movement of the mouse.
You ``grab'' the character under the mouse and move it around.

If the mouse is clicked and released in the same place of time we
assume that the user didn't want to scroll but wanted to whatever
mouse-2 used to do, so we pass it through.

Drag scrolling is identical to the ``hand'' option in MacPaint, or the
middle button in Tk text widgets.  Some of the following code is
derived from mouse-drag-region of emacs-19.28.

To test this function, evaluate:
    (global-set-key [down-mouse-2] 'mouse-drag-drag)"
  (interactive "e")
  ;; we want to do save-selected-window, but that requires 19.29
  (let* ((start-posn (event-start start-event))
	 (start-window (posn-window start-posn))
	 (start-row (cdr (posn-col-row start-posn)))
	 (start-col (car (posn-col-row start-posn)))
	 (old-selected-window (selected-window))
	 event end row mouse-delta scroll-delta
	 have-scrolled point-event-p old-binding
	 window-last-row
	 col mouse-col-delta window-last-col
	 (scroll-col-delta 0)
	 ;; be conservative about allowing horizontal scrolling
	 (col-scrolling-p (should-do-col-scrolling)))
    (select-window start-window)
    (setq window-last-row (- (window-height) 2)
	  window-last-col (- (window-width) 2))
    (track-mouse
      (while (progn
	       (setq event (read-event)
		     end (event-end event)
		     row (cdr (posn-col-row end))
		     col (car (posn-col-row end)))
	       (or (mouse-movement-p event)
		   (eq (car-safe event) 'switch-frame)))
	;; Scroll if see if we're on the edge.
	;; NEEDSWORK: should handle mouse-in-other window.
	(cond
	 ((not (eq start-window (posn-window end)))
	  t) ; wait for return to original window
	 ((<= row 0) (repeatedly-safe-scroll -1 0))
	 ((>= row window-last-row) (repeatedly-safe-scroll 1 0))
	 ((and col-scrolling-p (<= col 1)) (repeatedly-safe-scroll 0 -1))
	 ((and col-scrolling-p (>= col window-last-col)) (repeatedly-safe-scroll 0 1))
	 (t
	  (setq scroll-delta (- row start-row)
		start-row row)
	  (if col-scrolling-p
	      (setq scroll-col-delta (- col start-col)
		    start-col col))
	  (if (or (/= 0 scroll-delta)
		  (/= 0 scroll-col-delta))
	      (progn
		(setq have-scrolled t)
		(safe-scroll scroll-delta scroll-col-delta)))))))
    ;; If it was a click and not a drag, prepare to pass the event on.
    ;; Note:  We must determine the pass-through event before restoring
    ;; the window, but invoke it after.  Sigh.
    (if (and (not have-scrolled)
	     (events-are-point-events-p start-posn end))
	(setq point-event-p t
	      old-binding (key-binding
			   (vector (event-basic-type start-event)))))
    ;; Now restore the old window.
    (select-window old-selected-window)
    ;; For clicks, call the old function.
    (if point-event-p
	(call-interactively old-binding))))

(provide 'mouse-extras)
