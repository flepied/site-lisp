;;; vapropos.el --- more apropos commands for users and programmers.
;;  Time-stamp: <1998-01-13 19:05:19 ecl>

;; Copyright (C) 1998 Emilio C. Lopes.

;; Author: Emilio Lopes <Emilio.Lopes@Physik.TU-Muenchen.DE>
;; Created: 09 Jan 1998
;; Version: 1.0
;; Keywords: help

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; If you have not received a copy of the GNU General Public License
;; along with this software, it can be obtained from the GNU Project's
;; World Wide Web server (http://www.gnu.org/copyleft/gpl.html), from
;; its FTP server (ftp://ftp.gnu.org/pub/gnu/GPL), by sending an eletronic
;; mail to this program's maintainer or by writting to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; General
;; =======
;;
;; This library extends the standard GNU Emacs "apropos" library by
;; providing the following additional commands:
;;
;; vapropos-apropos-variable: show variables or custom options whose
;;                            name match a given regular expression.
;;
;; vapropos-apropos-command:  show commands or functions whose name
;;                            match a given regular expression.
;;
;; See the documentation of these functions for details.
;;
;; This library was not tested with GNU Emacs 19.
;;
;; Please send bug reports, suggestions, improvements, etc. to the
;; author of this library (see e-mail address above), so that they can
;; be incorporated in future versions.
;;
;;
;; Installation
;; ============
;;
;; Put this file ("vapropos.el") in a directory listed in your
;; `load-path' and byte-compile it.
;;
;; Add the following to your "~/.emacs":
;;
;;      (autoload 'vapropos-apropos-variable "vapropos"
;;        "Show variables or custom options that match REGEXP." t nil)
;;      (autoload 'vapropos-apropos-command "vapropos"
;;        "Show commands or functions that match REGEXP." t nil)
;;
;; Optionally you can bind keys to the commands provided by this
;; library. See the GNU Emacs on-line documentation for details.

;;; Code:

(load "apropos")

(eval-when-compile
  (load "apropos"))

(or (fboundp 'unless)                   ; not needed in recent GNU Emacsen.
    (defmacro unless (cond &rest body)
      "(unless COND BODY...): if COND yields nil, do BODY, else return nil."
      (cons 'if (cons cond (cons nil body)))))

(defun vapropos-apropos-variable (apropos-regexp &optional do-all)
  "Show user variables or custom options that match REGEXP.
With optional prefix ARG or if `apropos-do-all' is non-nil, also show
normal variables."
  (interactive (list (read-string 
                      (concat "Apropos "
                              (unless (or current-prefix-arg apropos-do-all)
                                "user ")
                                "variable or custom option (regexp): "))
                current-prefix-arg))
  (let ((message
	 (let ((standard-output (get-buffer-create "*Apropos*")))
	   (print-help-return-message 'identity))))
    (or do-all (setq do-all apropos-do-all))
    (setq apropos-accumulator
	  (apropos-internal apropos-regexp
                            (if do-all
                                (lambda (symbol) (or (user-variable-p symbol)
                                                     (get symbol 'custom-type)
                                                     (boundp symbol)))
                              (lambda (symbol) (or (user-variable-p symbol)
                                                   (get symbol 'custom-type))))))
    (if (vapropos-apropos-print
	 nil
	 (lambda (p)
	   (let (doc symbol)
	     (while p
	       (setcar p (list
			  (setq symbol (car p))
                          nil
                          (if (or (user-variable-p symbol)
                                  (get symbol 'custom-type)
                                  (and do-all (boundp symbol)))
                              (if (setq doc (documentation-property
                                             symbol 'variable-documentation t))
                                  (substring doc 0 (string-match "\n" doc))
                                "(not documented)"))))
	       (setq p (cdr p)))))
	 nil)
	(and message (message message)))))
(fset 'vapropos-variable-apropos 'vapropos-apropos-variable)

(defun vapropos-apropos-command (apropos-regexp &optional do-all)
  "Show commands (interactively callable functions) that match REGEXP.
With optional prefix ARG or if `apropos-do-all' is non-nil, also show
normal non-interactive macros and functions."
  (interactive (list (read-string (concat "Apropos command "
					  (if (or current-prefix-arg
						  apropos-do-all)
					      "or function ")
					  "(regexp): "))
		     current-prefix-arg))
  (let ((message
	 (let ((standard-output (get-buffer-create "*Apropos*")))
	   (print-help-return-message 'identity))))
    (or do-all (setq do-all apropos-do-all))
    (setq apropos-accumulator
	  (apropos-internal apropos-regexp
			    (if do-all
				(lambda (symbol) (or (commandp symbol)
						     (fboundp symbol)))
			      'commandp)))
    (if (vapropos-apropos-print
	 t
	 (lambda (p)
	   (let (doc symbol)
	     (while p
	       (setcar p (list
			  (setq symbol (car p))
			  (if (or (commandp symbol) (and do-all (fboundp symbol)))
			      (if (setq doc (documentation symbol t))
				  (substring doc 0 (string-match "\n" doc))
				"(not documented)"))
                          nil           ; see "C-h f apropos-print".
                          ))
	       (setq p (cdr p)))))
	 nil)
	(and message (message message)))))
(fset 'vapropos-command-apropos 'vapropos-apropos-command)

;; This is a patched version of `apropos-print' that will appear in
;; the next version of GNU Emacs (20.3, I presume).
(defun vapropos-apropos-print (do-keys doc-fn spacing)
  "Output result of various apropos commands with `apropos-regexp'.
APROPOS-ACCUMULATOR is a list.  Optional DOC-FN is called for each element
of apropos-accumulator and may modify it resulting in (symbol fn-doc
var-doc [plist-doc]).  Returns sorted list of symbols and documentation
found."
  (if (null apropos-accumulator)
      (message "No apropos matches for `%s'" apropos-regexp)
    (if doc-fn
	(funcall doc-fn apropos-accumulator))
    (setq apropos-accumulator
	  (sort apropos-accumulator (lambda (a b)
				      (string-lessp (car a) (car b)))))
    (and apropos-label-face
	 (symbolp apropos-label-face)
	 (setq apropos-label-face `(face ,apropos-label-face
					 mouse-face highlight)))
    (with-output-to-temp-buffer "*Apropos*"
      (let ((p apropos-accumulator)
	    (old-buffer (current-buffer))
	    symbol item point1 point2)
	(set-buffer standard-output)
	(apropos-mode)
	(if window-system
	    (insert "If you move the mouse over text that changes color,\n"
		    (substitute-command-keys
		     "you can click \\[apropos-mouse-follow] to get more information.\n")))
	(insert (substitute-command-keys
		 "In this buffer, type \\[apropos-follow] to get full documentation.\n\n"))
	(while (consp p)
	  (or (not spacing) (bobp) (terpri))
	  (setq apropos-item (car p)
		symbol (car apropos-item)
		p (cdr p)
		point1 (point))
	  (princ symbol)		        ; print symbol name
	  (setq point2 (point))
	  ;; Calculate key-bindings if we want them.
	  (and do-keys
	       (commandp symbol)
	       (indent-to 30 1)
	       (if (let ((keys
			  (save-excursion
			    (set-buffer old-buffer)
			    (where-is-internal symbol)))
			 filtered)
		     ;; Copy over the list of key sequences,
		     ;; omitting any that contain a buffer or a frame.
		     (while keys
		       (let ((key (car keys))
			     (i 0)
			     loser)
			 (while (< i (length key))
			   (if (or (framep (aref key i))
				   (bufferp (aref key i)))
			       (setq loser t))
			   (setq i (1+ i)))
			 (or loser
			     (setq filtered (cons key filtered))))
		       (setq keys (cdr keys)))
		     (setq item filtered))
		   ;; Convert the remaining keys to a string and insert.
		   (insert
		    (mapconcat
		     (lambda (key)
		       (setq key (key-description key))
		       (if apropos-keybinding-face
			   (put-text-property 0 (length key)
					      'face apropos-keybinding-face
					      key))
		       key)
		     item ", "))
		 (insert "M-x")
		 (put-text-property (- (point) 3) (point)
				    'face apropos-keybinding-face)
		 (insert " " (symbol-name symbol) " ")
		 (insert "RET")
		 (put-text-property (- (point) 3) (point)
				    'face apropos-keybinding-face)))
	  (terpri)
	  ;; only now so we don't propagate text attributes all over
	  (put-text-property point1 point2 'item
			     (if (eval `(or ,@(cdr apropos-item)))
				 (car apropos-item)
			       apropos-item))
	  (if apropos-symbol-face
	      (put-text-property point1 point2 'face apropos-symbol-face))
	  (apropos-print-doc 'describe-function 1
			     (if (commandp symbol)
				 "Command"
			       (if (apropos-macrop symbol)
				   "Macro"
				 "Function"))
			     t)
	  (if (get symbol 'custom-type)
	      (apropos-print-doc 'customize-variable-other-window 2
				 "User Option" t)
	    (apropos-print-doc 'describe-variable 2
			       "Variable" t))
	  (apropos-print-doc 'customize-group-other-window 6 "Group" t)
	  (apropos-print-doc 'customize-face-other-window 5 "Face" t)
	  (apropos-print-doc 'widget-browse-other-window 4 "Widget" t)
	  (apropos-print-doc 'apropos-describe-plist 3
			     "Plist" nil)))))
  (prog1 apropos-accumulator
    (setq apropos-accumulator ())))	; permit gc

(provide 'vapropos)

;;; vapropos.el ends here
