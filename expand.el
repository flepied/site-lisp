;; expand.el --- minor mode to make abbreviations more usable.
;;
;; Copyright (C) 1995-1996, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <fred@sugix.frmug.fr.net>
;; Maintainer: Frederic Lepied <fred@sugix.frmug.fr.net>
;; Version: $Id: expand.el,v 1.9 1996/10/20 14:52:30 fred Exp fred $
;; Keywords: abbrev
;;
;; LCD Archive Entry:
;; expand|Frederic Lepied|fred@sugix.frmug.fr.net|
;; A minor mode to make abbreviations more usable.|
;; $Date: 1996/10/20 14:52:30 $|$Revision: 1.9 $|~/modes/expand.el.Z|
;; 
;; This file is NOT part of GNU Emacs but the same permissions apply.
;;
;; GNU Emacs  is free software;  you can redistribute it and/or modify
;; it under the terms of  the GNU General  Public License as published
;; by  the Free Software  Foundation;  either version  2, or (at  your
;; option) any later version.
;;
;; GNU  Emacs is distributed  in the hope that  it will be useful, but
;; WITHOUT    ANY  WARRANTY;  without even the     implied warranty of
;; MERCHANTABILITY or FITNESS FOR A  PARTICULAR PURPOSE.  See the  GNU
;; General Public License for more details.
;;
;; You should have received  a copy of  the GNU General Public License
;; along with GNU Emacs; see  the file COPYING.  If  not, write to the
;; Free Software Foundation, 675  Mass Ave, Cambridge, MA 02139,  USA.
;; This  program  is free  software;  you  can  redistribute it and/or
;; modify it  under  the terms of the  GNU  General Public License  as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;;
;;; Commentary:
;;
;; Purpose of this package:
;;   1. Make abbreviations more usable.
;;   2. Expand abbreviations only when they are at the end of a line and not
;;   in a comment or in a string.
;;   3. Position the cursor after expansion to a place specified by advance.
;;   4. Indent the expanded region.
;;   5. If a list of points as been provided with the abbreviation definition,
;;   the functions expand-jump-to-previous-mark and expand-jump-to-next-mark
;;   moved from mark to mark.
;;
;; Installation:
;;   * store this file somewhere in your load-path and byte compile it.
;;   * put (require 'expand) in your .emacs or in site-start.el or generate
;; autoloads.
;;   * and according to the mode install your expansion table.
;;
;;   Look at the Sample: section for emacs-lisp, perl and c expand lists.
;; For example for c-mode, you could declare your abbrev table with :
;;
;; (defconst c-expand-list
;;   '(("if" "if () {\n \n} else {\n \n}" (5 10 21))
;;     ("ifn" "if () {}" (5 8))
;;     ("uns" "unsigned ")
;;     ("for" "for(; ; ) {\n\n}" (5 7 9 13))
;;     ("switch" "switch () {\n\n}" (9 13))
;;     ("case" "case :\n\nbreak;\n" (6 8 16))
;;     ("do" "do {\n\n} while ();" (6 16))
;;     ("while" "while () {\n\n}" (8 12))
;;     ("default" "default:\n\nbreak;" 10)
;;     ("main" "int\nmain(int argc, char * argv[])\n{\n\n}\n" 37))
;;   "Expansions for C mode")
;; 
;;   and enter expand-mode with the following hook :
;;
;; (add-hook 'c-mode-hook (function (lambda()
;; 				   (expand-add-abbrevs c-mode-abbrev-table c-expand-list)
;; 				   (expand-mode))))
;;
;;   you can also bind jump functions to some keys and init some post-process
;; hooks :
;;
;; (add-hook 'expand-mode-load-hook
;; 	  (function
;; 	   (lambda()
;; 	     (add-hook 'expand-expand-hook 'indent-according-to-mode)
;; 	     (add-hook 'expand-jump-hook 'indent-according-to-mode)
;; 	     (define-key expand-map '[C-tab] 'expand-jump-to-next-mark)
;; 	     (define-key expand-map '[C-S-tab] 'expand-jump-to-previous-mark))))
;;
;; Remarks:
;;
;;   Has been tested under emacs 19.28, 19.30 and XEmacs 19.11.
;;   Many thanks to Heddy Boubaker <boubaker@cenatls.cena.dgac.fr>.
;;   Please send me a word to give me your feeling about this mode or
;; to explain me how you use it (your expansions table for example) using
;; the function expand-mode-submit-report.

;; expand is not a replacement for abbrev it is just a layer above it.
;; You could always declare your abbreviations with define-abbrev to have
;; the abbrev behavior with expand-mode.
;;
;;; Change Log:
;;
;; $Log: expand.el,v $
;; Revision 1.9  1996/10/20 14:52:30  fred
;; 	* Don't expand if the preceding char isn't a word constituent.
;;
;; Revision 1.8  1996/07/07 16:31:05  fred
;; 	* Corrected the   behavior of  expand  when in  Overwrite mode
;; (Thanks to Jerome Santini <santini@chambord.univ-orleans.fr>).
;;
;; Revision 1.7  1996/03/25 20:34:12  fred
;; 	* added perl and emacs lisp examples
;; from Jari Aalto <jaalto@tre.tele.nokia.fi>.
;;
;; Revision 1.6  1996/03/13 03:18:18  fred
;; Thanks to remarks from Jerome Santini <santini@chambord.univ-orleans.fr> :
;; 	* added hooks expand-expand-hook and expand-jump-hook to run
;; actions after expansions and moves.
;;
;; Revision 1.5  1996/02/07 05:26:54  fred
;; According to remarks from Jari Aalto <jaalto@tre.tele.nokia.fi> :
;;
;; 	* added hooks : expand-mode-hook and expand-mode-load-hook.
;; 	* expand-mode-name is now the name displayed in the modeline
;; to enable to change it.
;; 	* added expand-mode-submit-report
;;
;; Revision 1.4 1995/04/07 21:50:22 fred
;; 	* added test not to expand when we are in a comment or
;; in a string
;; 
;; Revision 1.3 1995/02/25 21:39:54 fred
;; * added support for a list of markers to visit after expansion.
;; * added support to use abbreviations defined with define-abbrev which
;; can be expanded anywhere not only at the end of a line in expand-mode.
;; 
;; Revision 1.2 1995/02/21 21:58:00 fred
;; * indent expanded region
;; * 3 possible forms : - a function
;;                      - string + position
;;                      - string
;; 
;; Revision 1.1 1995/02/19 15:06:50 fred
;; Initial revision
;;

;;; Constants:

(defconst expand-mode-version "$Id: expand.el,v 1.9 1996/10/20 14:52:30 fred Exp fred $"
  "version tag for expand.el")

(defconst expand-mode-help-address "expand-help@sugix.frmug.org"
  "email address to send requests, comments or bug reports")

(defvar expand-mode nil
  "Status variable for expand-mode")
(make-variable-buffer-local 'expand-mode)

(defvar expand-mode-name " Expand"
  "Name of mode displayed in the modeline for expand-mode")

(defvar expand-mode-hook nil
  "hooks run when expand-mode is enabled")

(defvar expand-mode-load-hook nil
  "hooks run when expand is loaded")

(defvar expand-expand-hook nil
  "hooks run when expansion is done")

(defvar expand-jump-hook nil
  "hooks run when jump to mark occurs")

;;; Samples:

(defconst expand-c-sample-expand-list
  '(("if" "if () {\n \n} else {\n \n}" (5 10 21))
    ("ifn" "if () {}" (5 8))
    ("uns" "unsigned ")
    ("for" "for(; ; ) {\n\n}" (5 7 9 13))
    ("switch" "switch () {\n\n}" (9 13))
    ("case" "case :\n\nbreak;\n" (6 8 16))
    ("do" "do {\n\n} while ();" (6 16))
    ("while" "while () {\n\n}" (8 12))
    ("default" "default:\n\nbreak;" 10)
    ("main" "int\nmain(int argc, char * argv[])\n{\n\n}\n" 37))
  "Expansions for C mode")

;; from Jari Aalto <jaalto@tre.tele.nokia.fi>
(defconst expand-sample-lisp-mode-expand-list
  '(("defu" "(defun   ()\n  \"\"\n  (interactive)\n  )"
     (8 11 16 20 36))
    (("defs" "(defsubst   ()\n  \"\"\n  (interactive)\n  )")
     (11 14 19 23 39))
    ("defm" "(defmacro  ()\n  \"\"\n  (` \n    ))"
     (11 13 18 25))
    ("defa" "(defadvice   (around   act)\n  \"\"\n  \n  )"
     (12 22 32 36))
     ("defc" "(defconst   nil\n  \"\")\n"
      (11 13 20))
     ("defv" "(defvar   nil\n  \"\")\n"
      (9 11 18))
      ("sav" "(save-excursion\n \n)"
       (18)) )
   "Lisp expansions.")
 
;; from Jari Aalto <jaalto@tre.tele.nokia.fi>
(defconst expand-sample-perl-mode-expand-list
  (list
   (list
    ;;   This is default perl subroutine template
    ;;
    "sub"
    (concat
     "#" (make-string 70 ?-) "\n"
     "sub   {\n"
     "    #   \n"
     "    #   \n"
     "    # RETURN\n"
     "    #   \n"
     "\n"
     "    local( $f ) = \"$lib.\";\n"	;; Function name AFTER period
     "    local() = @_;\n"		;; func arguments here
     "    \n"
     "    \n}\n"
     )
    (list 77 88 120 146 159 176))
   
   (list
    "for"				; foreach
    (concat
     "for (  )\n"
     "{\n\n\}"
     )
    (list 7 12))
   
   (list
    "whi"				; foreach
    (concat
     "while (  )\n"
     "{\n\n\}"
     )
    (list 9 15))
   
   ;;   The normal "if" can be used like
   ;;   print $F "xxxxxx"  if defined @arr;
   ;;
   (list
    "iff"
    (concat
     "if (  )\n"
     "{\n\n\}"
     )
    (list 6 12))
   
   (list "loc"  "local( $ );"	(list 9))
   (list "my"	"my( $ );"	(list 6))
   (list "ope"	"open(,\"\")\t|| die \"$f: Can't open [$]\";" (list 6 8 36))
   (list "clo"  "close ;"	7)
   (list "def"	"defined  "     (list 9))
   (list "und"	"undef ;"	(list 7))
   
   ;;   There is no ending colon, because they can be in statement
   ;;    defined $REXP_NOT_NEW && (print "xxxxx" );
   ;;
   (list "pr"  "print "		7)
   (list "pf"  "printf "	8)
   
   (list "gre"  "grep( //, );"	(list 8 11))
   (list "pus"  "push( , );"	(list 7 9))
   (list "joi"  "join( '', );"	(list 7 11))
   (list "rtu"  "return ;"	(list 8))
   )
  "Perl expansions.")

;;; Code:

;;;###autoload
(defun expand-mode(&optional arg)
  "Toggle expand mode.
With argument ARG, turn expand mode on if ARG is positive.
In expand mode, inserting an abbreviation at the end of a line
causes it to expand and be replaced by its expansion."
  (interactive "P")
  (setq expand-mode (if (null arg) (not expand-mode)
		       (> (prefix-numeric-value arg) 0)))
  (if expand-mode
      (progn
	(setq abbrev-mode nil)
	(run-hooks 'expand-mode-hook))))

;;;###autoload
(defvar expand-map (make-sparse-keymap)
  "key map used in expand-mode.")
(define-key expand-map " " 'expand)

(or (assq 'expand-mode minor-mode-alist)
    (setq minor-mode-alist (cons (list 'expand-mode expand-mode-name)
				 minor-mode-alist)))

(or (assq 'expand-mode minor-mode-map-alist)
    (setq minor-mode-map-alist (cons (cons 'expand-mode expand-map)
				     minor-mode-map-alist)))
 
;;;###autoload
(defun expand-add-abbrevs (table abbrevs)
  "Add a list of abbrev to the table.
Each abbrev description entry has the following format :
	(abbrev expansion arg)
where
      abbrev is the abbreviation to replace.
      expansion is the replacement string or a function which will make
the expansion. For example you could use the DMacros package to generate such functions.
      arg is an optional  element which can be a  number or a  list of
numbers. If arg is a  number, the cursor will be  placed at arg  chars
from  the beginning of  the expanded text.   If expansion is a list of
numbers the cursor will be placed according to the first number of the
list from the beginning of the expanded text and  marks will be placed
and you  will  be able to  visit  them  cyclicaly  with the  functions
expand-jump-to-previous-mark  and expand-jump-to-next-mark. If arg  is
omitted, the cursor will be placed at the end of the expanded text."
  (if (null abbrevs)
      table
    (expand-add-abbrev table (nth 0 (car abbrevs)) (nth 1 (car abbrevs))
		       (nth 2 (car abbrevs)))
    (expand-add-abbrevs table (cdr abbrevs))))

(defvar expand-list nil "Temporary variable used by expand-mode.")

(defvar expand-pos nil
  "If non nil, stores a vector containing markers to positions defined by the last expansion.
This variable is local to a buffer.")
(make-variable-buffer-local 'expand-pos)

(defvar expand-index 0
  "Index of the last marker used in expand-pos.
This variable is local to a buffer.")
(make-variable-buffer-local 'expand-index)

(defvar expand-point nil
  "End of the expanded region.
This variable is local to a buffer.")
(make-variable-buffer-local 'expand-point)

(defun expand-add-abbrev (table abbrev expansion arg)
  "Add one abbreviation and provide the hook to move to the specified
position"
  (let* ((string-exp (if (and (symbolp expansion) (fboundp expansion))
			 nil
		       expansion))
         (position   (if (and arg string-exp)
			 (if (listp arg)
			     (- (length expansion) (1- (car arg)))
			   (- (length expansion) (1- arg)))
		       0)))
    (define-abbrev
      table
      abbrev
      (or string-exp "")
      (if string-exp
	  (if (and (listp arg)
		   (not (null arg)))
	      (` (lambda()
		   (expand-build-list (, (length string-exp)) '(, arg))
		   (setq expand-point (point))
		   (backward-char (, position))))

	    (` (lambda()
		 (setq expand-point (point))
		 (backward-char (, position)))))
        expansion))))

;;;###autoload
(defun expand(arg)
  "Do the expansion job if we are at the end of a line or insert space"
  (interactive "p")
  (or (if (or (and (eolp)
		   (not (expand-in-literal)))
	      (expand-abbrev-not-expand (expand-previous-word)))
	  (let ((p (point)))
	    ;; don't expand if the preceding char isn't a word constituent
	    (if (and (eq (char-syntax (preceding-char))
			 ?w)
		     (expand-abbrev))
		(progn
		  ;; expand-point tells us if we were called from our code
		  ;; or from another function.
		  (if expand-point
		      (progn
			(if (vectorp expand-list)
			    (expand-build-marks expand-point))
			(indent-region p expand-point nil))
		    ;; an outside function can set expand-list to a list of
		    ;; markers in reverse order.
		    (if (listp expand-list)
			(setq expand-index 0
			      expand-pos (expand-list-to-markers expand-list)
			      expand-list nil)))
		  (run-hooks 'expand-expand-hook)
		  t))))
      (self-insert-command arg))
  (setq expand-point nil))

(defun expand-abbrev-not-expand(word)
  "Test if an abbrev as a hook"
  (or
   (and (intern-soft word local-abbrev-table)
	(not (symbol-function (intern-soft word local-abbrev-table))))
   (and (intern-soft word global-abbrev-table)
	(not (symbol-function (intern-soft word global-abbrev-table))))))

(defun expand-previous-word ()
  "Return the previous word"
  (save-excursion
    (let ((p (point)))
      (backward-word 1)
      (buffer-substring p (point)))))

(defun expand-jump-to-previous-mark()
  "Move the cursor to previous mark created by the expansion"
  (interactive)
  (if expand-pos
      (progn
	(setq expand-index (1- expand-index))
	(if (< expand-index 0)
	    (setq expand-index (1- (length expand-pos))))
	(goto-char (aref expand-pos expand-index))
	(run-hooks 'expand-jump-hook))))

(defun expand-jump-to-next-mark()
  "Move the cursor to next mark created by the expansion"
  (interactive)
  (if expand-pos
      (progn
	(setq expand-index (1+ expand-index))
	(if (>= expand-index (length expand-pos))
	    (setq expand-index 0))
	(goto-char (aref expand-pos expand-index))
	(run-hooks 'expand-jump-hook))))

(defun expand-build-list (len l)
  "Build a vector of offset positions from the list of positions" 
  (expand-clear-markers)
  (setq expand-list (vconcat l))
  (let ((i 0))
    (while (< i (length expand-list))
      (aset expand-list i (- len (1- (aref expand-list i))))
      (setq i (1+ i))))
  )

(defun expand-build-marks (p)
  "Transform the offsets vector into a marker vector" 
  (if expand-list
      (progn
	(setq expand-index 0)
	(setq expand-pos (make-vector (length expand-list) nil))
	(let ((i (1- (length expand-list))))
	  (while (>= i 0)
	    (aset expand-pos i (copy-marker (- p (aref expand-list i))))
	    (setq i (1- i))))
	(setq expand-list nil))))

(defun expand-clear-markers ()
  "Make the markers point nowhere"
  (if expand-pos
      (progn
    (let ((i (1- (length expand-pos))))
      (while (>= i 0)
	(set-marker (aref expand-pos i) nil)
	(setq i (1- i))))
    (setq expand-pos nil))))

(defun expand-in-literal ()
  "Test if we are in a comment or in a string"
  (save-excursion
    (let* ((lim (or (save-excursion
		      (beginning-of-defun)
		      (point))
		    (point-min)))
	   (here (point))
	   (state (parse-partial-sexp lim (point))))
      (cond
       ((nth 3 state) 'string)
       ((nth 4 state) 'comment)
       (t nil)))))

(defun expand-mode-submit-report ()
  "Report a problem, a suggestion or a comment about expand-mode"
  (interactive)
  (require 'reporter)
  (reporter-submit-bug-report
   expand-mode-help-address
    (concat "expand.el " expand-mode-version)
    '(expand-mode-name
      expand-mode-hook
      expand-mode-load-hook
      expand-map
      )
    nil
    nil
    "Dear expand.el maintainer,"))

;; support functions to add marks to jump from outside function

(defun expand-list-to-markers (l)
  "Transform a list of markers in reverse order into a vector in the correct order"
  (let* ((len (1- (length l)))
	 (loop len)
	 (v (make-vector (+ len 1) nil)))
    (while (>= loop 0)
      (aset v loop (if (markerp (car l)) (car l) (copy-marker (car l))))
      (setq l (cdr l)
	    loop (1- loop)))
    v))

(defun expand-add-mark ()
  "Add the current position to the marks list to be used after expansion
by `expand-jump-to-next-mark' and `expand-jump-to-previous-mark'"
  (setq expand-list (cons (copy-marker (point)) expand-list)) )

;; integration with skeleton.el
(defun expand-skeleton-end-hook ()
  "`skeleton-end-hook' to enable `expand-jump-to-next-mark' and
`expand-jump-to-previous-mark' for @ skeleton tags see `skeleton-insert'"
  (if skeleton-marks
      (setq expand-list skeleton-marks)))
  
(add-hook 'skeleton-end-hook (function expand-skeleton-end-hook))

;; run load hook
(run-hooks 'expand-mode-load-hook)

(provide 'expand)

;;; expand.el ends here
