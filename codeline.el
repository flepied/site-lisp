;;;;
;;;; codeline.el
;;;;
;;;; author: <ttn@eclu.psu.edu>
;;;;
;;;; Copyright (C) 1993,1994  Thien-Thi Nguyen
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANT-
;;;; ABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;;;; Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software Founda-
;;;; tion, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;

;;;; version 1.1
;;;; - bugfix.  can handle comment-start in complete strings, ie both
;;;;   quotes on the same line.  this should represent most cases.
;;;; - new stuff.  special cases empty line, just comment.
;;;; - clean up.  use already-there code.  add autoloads.
;;;; - comments.  wow, over 4K now.  :-)

;;;; version 1.0 -- initial release

;;;; explanation
;;;;
;;;; got tired of hitting C-a and C-e only to go to the absolute edge
;;;; of a line, where either whitespace (beginning) or a comment (end)
;;;; necessitated extra keystrokes to get to the code.  the following
;;;; two commands move the point to the beginning and end of the code
;;;; part of the line, respectively, essentially ignoring leading white-
;;;; space and trailing comments.  subsequent invocations just call
;;;; their less-lengthly-named cousins.
;;;;
;;;; there are two special cases:
;;;; a) line consists of all whitespace (including just \n).  C-a moves
;;;; point to the proper mode-specific column.  C-e moves to end of line.
;;;; b) line has no code, only comment.  C-a and C-e move point to the
;;;; edge of the comment.
;;;;
;;;; code enhancement opportunities (aka bugs/caveats):
;;;; - cannot handle comments at the beginning of the line.
;;;; - cannot handle comment-start if in a 'incomplete' string.
;;;; - depends on mode-provided comment-start variable, which means that
;;;;   if you have
;;;;      int a;   /*comment with no space after slash-star */
;;;;   C-e will do the wrong thing.  (just use M-; eh?)
;;;;
;;;; feedback welcome!
;;;;

(defvar codeline-last-arg nil
  "enables passing through of args to subsequent command invocations.")

;;;###autoload
(defun beginning-of-code-line (arg)
  "moves point to first non-whitespace char on line for first invocation.
if line is just white-space, indents to proper column.  second invocation
moves point to beginning of line."
  (interactive "p")
  (if (eq this-command last-command)
      (beginning-of-line codeline-last-arg)
    (beginning-of-line)
    (if (looking-at "\\s-*\n")
	(indent-according-to-mode)
      (back-to-indentation)))
  (setq codeline-last-arg arg))

;;;###autoload
(defun end-of-code-line (arg)
  "if line is just a comment, moves to end of line.  otherwise, on
first invocation moves point to end of code line.  second invocation
moves point to end of line." 
  (interactive "p")
  (if (eq this-command last-command)
      (end-of-line codeline-last-arg)
    (or (and (boundp 'comment-start-regexp)
	     comment-start-regexp)
	(progn
	  (make-local-variable 'comment-start-regexp)
	  (setq comment-start-regexp (regexp-quote (or comment-start "")))))
    (let ((eol (progn (end-of-line) (point)))
	  (stop (progn (beginning-of-line) (point))))
      (if (looking-at (concat "\\s-*" comment-start-regexp ".*\n"))
	  (end-of-line)
	(while (if (re-search-forward "\"[^\"]*\"" eol t)
		   (setq stop (point))))
	(end-of-line)
	(while (re-search-backward comment-start-regexp stop t))
	(re-search-backward "\\S-" stop t)
	(if (not (looking-at "\n")) (forward-char 1)))))
  (setq codeline-last-arg arg))

;;;##autoload (define-key c-mode-map "\C-a" 'beginning-of-code-line)
;;;##autoload (define-key c-mode-map "\C-e" 'end-of-code-line)
;;;##autoload (define-key shared-lisp-mode-map "\C-a" 'beginning-of-code-line)
;;;##autoload (define-key shared-lisp-mode-map "\C-e" 'end-of-code-line)
;;;##autoload (define-key c++-mode-map "\C-a" 'beginning-of-code-line)
;;;##autoload (define-key c++-mode-map "\C-e" 'end-of-code-line)
;;;##autoload (define-key emacs-lisp-mode-map "\C-a" 'beginning-of-code-line)
;;;##autoload (define-key emacs-lisp-mode-map "\C-e" 'end-of-code-line)

;;;;
;;;; end of codeline.el
;;;;
