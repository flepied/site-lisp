;; Electric Font Lock Mode Menu
;; Copyright (C) 1995 Simon Marshall.

;; Author: Simon Marshall <simon@gnu.ai.mit.edu>
;; Keywords: languages, faces
;; Version: 1.01

;;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Purpose:
;;
;; This package creates a rudimentary menu for Font Lock in Emacs 19.29 and up.

;; Installation:
;; 
;; Put this file somewhere where Emacs can find it (i.e., in one of the paths
;; in your `load-path'), `byte-compile-file' it, and put in your ~/.emacs:
;;
;; (require 'font-lock-menu)

;; Feedback:
;;
;; Feedback is welcome.  To submit feedback please use the mechanism provided:
;;
;; M-x font-lock-menu-submit-feedback RET

;; 1.00--1.01:
;; - Made font-lock-menu.el `byte-compile-dynamic'.
;; - Made `font-lock-menu-all-keywords' use Emacs 19.30 keywords list.
;; - Made `font-lock-menu-change' use `font-lock-set-defaults'.

(if (or (not (boundp 'emacs-minor-version))
	(and (= emacs-major-version 19)
	     (< emacs-minor-version 29)))
    (error "Font Lock menu only works with Emacs 19.29 and up"))

(eval-when-compile
  (require 'font-lock)
  (setq byte-compile-dynamic t byte-compile-dynamic-docstrings t))

(defun font-lock-menu-submit-feedback ()
  "Submit via mail feedback on font-lock-menu.el."
  (interactive)
  (let ((reporter-prompt-for-summary-p t))
    (reporter-submit-bug-report "simon@gnu.ai.mit.edu" "font-lock-menu 1.01"
				nil nil nil "Hi Si.,")))

;; Make a menu and add it to the `edit' menu, before the Text Properties menu.
(defvar font-lock-menu (make-sparse-keymap "Fontification"))
(define-key-after (lookup-key global-map [menu-bar edit]) [font-lock-menu]
  (cons "Fontification" font-lock-menu) 'separator-edit)

;; Add the menu items in reverse order.  Yuck.
(define-key font-lock-menu [fontify-minimally]
  '("Fontify Minimally" . font-lock-menu-change-minimum))
(define-key font-lock-menu [fontify-maximally]
  '("Fontify Maximally" . font-lock-menu-change-maximum))
(define-key font-lock-menu [fontify-window]
  '("Fontify Window" . font-lock-menu-fontify-window))
(define-key font-lock-menu [fontify-buffer]
  '("Fontify Buffer" . font-lock-fontify-buffer))
(define-key font-lock-menu [fontify-toggle]
  '("Toggle On/Off" . font-lock-mode))

(defun font-lock-menu-fontify-window ()
  "Fontify the current window the way `font-lock-mode' would."
  (interactive)
  (let ((font-lock-verbose nil))
    (save-restriction
      (narrow-to-region (window-start) (window-end))
      (font-lock-fontify-buffer))))

(defun font-lock-menu-change-minimum ()
  "Turn on Font Lock mode using minimum decoration."
  (interactive)
  (font-lock-menu-change nil))

(defun font-lock-menu-change-maximum ()
  "Turn on Font Lock mode using maximum decoration."
  (interactive)
  (font-lock-menu-change t))

(defun font-lock-menu-change (level)
  (require 'font-lock)
  (let ((keywords (font-lock-menu-all-keywords)))
    (cond ((null keywords)
	   (message "No Font Lock keywords found for %s" major-mode))
	  ((= (length keywords) 1)
	   (if (not font-lock-mode)
	       (font-lock-mode)
	     (message "No other Font Lock keywords found for %s" major-mode)))
	  (t
	   (let ((new (font-lock-menu-choose-keyword level (cdr keywords))))
	     (if (and font-lock-mode (equal font-lock-keywords new))
		 (message "Font Lock keywords are unchanged")
	       (font-lock-mode 0)
	       (font-lock-set-defaults)
	       (setq font-lock-keywords new)
	       (font-lock-mode 1)))))))

(defun font-lock-menu-all-keywords ()
  (let ((keyword (or (nth 0 font-lock-defaults)
		     (nth 1 (assq major-mode font-lock-defaults-alist)))))
    (cond ((null keyword)
	   nil)				; None.
	  ((listp keyword)
	   keyword)			; Emacs 19.30 already has them.
	  (t
	   (mapcar 'intern (sort (all-completions (symbol-name keyword)
						  obarray) 'string-lessp))))))

(defun font-lock-menu-choose-keyword (level keywords)
  (let ((font-lock-keywords
	 (symbol-value
	  (cond ((numberp level)
		 (or (nth level keywords) (car (reverse keywords))))
		((eq level t)
		 (car (reverse keywords)))
		(t
		 (car keywords))))))
    (font-lock-compile-keywords)))

;; Provide ourselves:

(provide 'font-lock-menu)

;;; font-lock-menu.el ends here
