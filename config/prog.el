;; 35prog.el --- 
;;
;; Copyright (C) 1996, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: prog.el,v 1.3 2004-01-19 07:32:38 flepied Exp $
;; Keywords: 
;;

;;=============================================================================
;; Autoloaded section.
;;=============================================================================

(autoload 'xrdb-mode "xrdb-mode" "Mode for editing X resource files" t)

(autoload 'fold-mode "foldingo"
  "Toggle fold-mode (or enter it if ARG is true).

Fold mode adds a new menu item under the Tools menu of the menubar.
From this menu:
  You may (re)enter or leave the Fold mode.
  You can hide or show all the folds of a buffer.
  You can hide or show the current fold containing the dot.

You can also click the mouse (more precisely \\[fold-mouse-handle])
on either }}} or {{{ to show/hide the associated fold.

Fold mode can be customized with the following variables (whose default
value for a given mode are stored in its Plist):
      fold-begin-regexp
      fold-end-regexp
      fold-open-face
      fold-closed-face
      fold-compute-folds
      fold-implicit-fold-threshold
      fold-comment-start
      fold-implicit-closed-fold-height
      fold-mode-hook
" t)

(autoload 'm4-mode "m4-mode" "A major-mode to edit m4 macro files" t)


;;;###

;;;***

;;;### (autoloads (end-of-code-line beginning-of-code-line) "codeline" "../codeline.el" (11507 25260))
;;; Generated autoloads from ../codeline.el

(autoload (quote beginning-of-code-line) "codeline" "\
moves point to first non-whitespace char on line for first invocation.
if line is just white-space, indents to proper column.  second invocation
moves point to beginning of line." t nil)

(autoload (quote end-of-code-line) "codeline" "\
if line is just a comment, moves to end of line.  otherwise, on
first invocation moves point to end of code line.  second invocation
moves point to end of line." t nil)
; (define-key shared-lisp-mode-map "\C-a" 'beginning-of-code-line)
; (define-key shared-lisp-mode-map "\C-e" 'end-of-code-line)
 (define-key emacs-lisp-mode-map "\C-a" 'beginning-of-code-line)
 (define-key emacs-lisp-mode-map "\C-e" 'end-of-code-line)

;;;***

;;;### (autoloads (comment-out-region) "comment" "../comment.el" (12098 54501))
;;; Generated autoloads from ../comment.el

(defvar comment-mode-alist (quote ((t 35 1 " ") (awk-mode 35 1 " ") (c++-mode "// ") (c-mode " * " "/* " "\n */") (emacs-lisp-mode 59 2 " ") (fortran-mode 67 1 " ") (lisp-interaction-mode 59 2 " ") (lisp-mode 59 2 " ") (mail-mode 62) (pascal-mode " * " "(* " "\n *)") (perl-mode 35 1 " ") (scheme-mode 59 2 " ") (tex-mode 37 1 " ") (texinfo-mode "@c ") (text-mode 35 1 " "))) "\
Association between major mode and types of comment characters.
This variable is a list of lists; each list consists of a major mode
and one of two possible sets of values:

* If the first value is nil or a string, the arguments are as follows:

    If only one string is specified, that string is inserted in front of
    every line in the region.

    If a second string is specified, it is the \"begin comment\" string.
    For example, in C a comment begins with \"/*\".  The first string, if
    non-`nil', will be inserted before the rest of the lines.

    If a third string is specified, it is the \"end comment\" string.  For
    example, in C a comment ends with \"*/\".  The first and second
    strings, if non-`nil', will be inserted before the first and middle
    lines, respectively.

* If the first value is a single character in numeric form, the
  arguments are as follows:

    The first value is the character to insert in front of every line.

    The second value, if non-`nil', is the default number of times to
    insert that character on each line, if no prefix argument is specified.
    This value defaults to 1 if unspecified.

    The third value, if non-`nil' means to insert that char or string
    after the inserted comment character(s).  Usually it will be a space or
    empty.

If no entry for a particuar mode exists, the values of `comment-start' and
`comment-end' are used if they exist and contain useful values (e.g. not
\"\").  These are standard variables in some versions of Emacs 19.

Otherwise, the characters specified in this alist with the key `t' are
used.  This key should be kept as close to the beginning of the alist as
possible to minimize searching for it.")

(autoload (quote comment-out-region) "comment" "\
Comment or uncomment a region of text according to major mode.

This command, when called with no prefix argument or a positive numeric
prefix argument, puts comments characters appropriate to the current major
mode in front of (or around) the lines of the region delimited by point and
mark.

If called with a generic prefix argument or with a negative numeric prefix
argument, attempt to remove the comments in front of the text in the
region.

When called from lisp programs, this function takes 3 optional arguments:
the beginning and end of the region to comment, and a count which
determines whether to add or remove comments depending on whether it is nil
\(like specifying no prefix arg), positive, or negative.

Mode-specific comment characters are defined in the table
`comment-mode-alist'." t nil)

;;;***

;;;### (autoloads (expand-add-abbrevs) "expand" "../expand.el" (13008 4325))
;;; Generated autoloads from ../expand.el

(autoload (quote expand-add-abbrevs) "expand" "\
Add a list of abbrev to abbrev table TABLE.
ABBREVS is a list of abbrev definitions; each abbrev description entry
has the form (ABBREV EXPANSION ARG).

ABBREV is the abbreviation to replace.

EXPANSION is the replacement string or a function which will make the
expansion.  For example you, could use the DMacros or skeleton packages
to generate such functions.

ARG is an optional argument which can be a number or a list of
numbers.  If ARG is a number, point is placed ARG chars from the
beginning of the expanded text.

If ARG is a list of numbers, point is placed according to the first
member of the list, but you can visit the other specified positions
cyclicaly with the functions `expand-jump-to-previous-mark' and
`expand-jump-to-next-mark'.

If ARG is omitted, point is placed at the end of the expanded text." nil nil)

;;;***

;;;### (autoloads (granny granny-mode) "granny" "../granny.el" (13083 51038))
;;; Generated autoloads from ../granny.el

(defvar granny-mode-hook nil "\
*Hooks to run when granny-mode is turned on.")

(autoload (quote granny-mode) "granny" "\
Major mode for granny buffers.

You can use the mode-specific menu to alter the time-span of the used
colors." t nil)

(autoload (quote granny) "granny" "\
Display the result of the cvs annotate command using colors.
New lines are displayed in red, old in blue. To use granny, press
\\[granny] when placed in a file controlled by CVS." t nil)

;;;***

;;;### (autoloads (module-add-entry) "module" "../module.el" (13424 40693))
;;; Generated autoloads from ../module.el

(defvar module-alist nil "\
alist associating directories regex and hooks")

(autoload (quote module-add-entry) "module" nil nil nil)

;;;***

;;;### (autoloads (simple-merge-mode) "simple-merge-mode" "../simple-merge-mode.el" (14348 46918))
;;; Generated autoloads from ../simple-merge-mode.el

(easy-mmode-define-minor-mode simple-merge-minor-mode "Minor mode to simplify editing output from the diff3 program.\nA minor mode variant of `simple-merge-mode'.\n\\{simple-merge-minor-mode-map}" nil " SMerge")

(autoload (quote simple-merge-mode) "simple-merge-mode" "\
Major mode to simplify editing output from the diff3 program.

`simple-merge-mode' makes it easier to edit files containing
\"conflict marks\", generated for example by diff3 and CVS.

There are two modes of operation, \"edit mode\" and \"fast mode\".
Fast mode is the default mode.

Special key bindings in \"fast mode\":
\\{simple-merge-basic-keymap}

In `edit' mode, commands must be prefixed by \\[simple-merge-basic-keymap]." t nil)

;;;***

;;;### (autoloads (word-help set-help-file reset-word-help) "word-help" "../word-help.el" (12955 50060))
;;; Generated autoloads from ../word-help.el

(autoload (quote reset-word-help) "word-help" "\
Clear all cached indexes in the `word-help' system.
You should only need this when installing new info files, and/or 
adding more TeXinfo files to the `word-help' system." t nil)

(autoload (quote set-help-file) "word-help" "\
Change which set of TeXinfo files used for help-mode.

`word-help' maintains a list over which TeXinfo files which are
relevant for each programming language (`word-help-mode-alist').  It
usually selects the correct one, based upon the value of `mode-name'.
If this guess is incorrect, you may also use this function manually to
instruct future `word-help' calls which TeXinfo files to use." t nil)

(autoload (quote word-help) "word-help" "\
Find documentation on the keyword under the cursor.
The determination of which language the keyword belongs to, is based upon
The relevant info file is selected by matching `mode-name' (the major
mode) against the assoc list `word-help-mode-alist'.  

If this is not possible, `set-help-file' will be invoked for selecting
the relevant info file.  `set-help-file' may also be invoked
interactively by the user.

If the keyword you are looking at is not available in any index, no
default suggestion will be presented. " t nil)

;;;***


;;=============================================================================
;; Configuration section.
;;=============================================================================
;; .in suppression in mode specification magic.
(setq auto-mode-alist (cons '(".in$" nil t) auto-mode-alist))

;; must be set first to take place before the previous one.
(setq auto-mode-alist (cons '("\\.m[4c]$\\|configure\\.in" . m4-mode)
			    auto-mode-alist)
      interpreter-mode-alist (cons '("make" . makefile-mode)
				   interpreter-mode-alist) )

(defun foldingo-activation ()
  ;; protect fold-mode to prevent frequent errors
  (condition-case ()
      (fold-mode 1)
    (error t))
  (define-key fold-keymap [(control c) ?h] 'fold-close-current-fold)
  (define-key fold-keymap [(control c) ?s] 'fold-open-current-fold)
  (define-key fold-keymap [(control c) ?u] 'fold-open-all-folds)
  (define-key fold-keymap [(control c) ?a] 'fold-close-all-folds)
)

(global-set-key [(control c)(q)] 'comment-out-region)

(add-hook 'expand-load-hook
	  (function
	   (lambda()
	     (add-hook 'expand-expand-hook 'indent-according-to-mode)
	     (add-hook 'expand-jump-hook 'indent-according-to-mode)
	     ;; (control shift tab) doesn't work with my bindings
	     ;;(define-key abbrev-mode-map '[(meta control tab)] 'expand-jump-to-previous-mark)
	     )))

;; add make to script mode list to enable debian rules.
(setq interpreter-mode-alist (cons '("make" . makefile-mode) interpreter-mode-alist))

;; context help binding
(define-key help-map '[(control i)] 'word-help)

;;==============================================================================
;; suppress emacs 19.29 CVS support
;;==============================================================================
(setq vc-handle-cvs nil)

(require 'cvs)
(define-key cvs:map "\C-cva" 'granny)
;; cvs -l update do no loger work
(setq cvs-no-log-option nil)

(require 'file-log)
(define-key ctl-x-map "l" 'flog-add-entry)
(define-key ctl-x-4-map "l" 'flog-add-entry-other-window)
(define-key ctl-x-5-map "l" 'flog-add-entry-other-frame)

(define-key menu-bar-tools-menu [separator-print]
  '("--"))
(define-key menu-bar-tools-menu [flog-add-entry]
 '("Add file log entry" . flog-add-entry))
(define-key menu-bar-tools-menu [add-change-log-entry]
 '("Add ChangeLog entry" . add-change-log-entry))

;;==============================================================================
;; xrdb support
;;==============================================================================
(setq auto-mode-alist
      (append '(("\\.Xdefaults$"    . xrdb-mode)
                ("\\.Xenvironment$" . xrdb-mode)
                ("\\.Xresources$"   . xrdb-mode)
                ("*.\\.ad$"         . xrdb-mode)
                )
              auto-mode-alist))

;; 1999-07-12 Noah Friedman <friedman@splode.com>
;; Public domain

(defun make-buffer-file-executable-if-script-p ()
  "Make file executable according to umask if not already executable.
If file already has any execute bits set at all, do not change existing
file modes."
  (and (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (save-match-data
             (looking-at "^#!"))))
       (let* ((current-mode (file-modes (buffer-file-name)))
              (add-mode (logand ?\111 (default-file-modes))))
         (or (/= (logand ?\111 current-mode) 0)
             (zerop add-mode)
             (set-file-modes (buffer-file-name)
                             (logior current-mode add-mode))))))

(add-hook 'after-save-hook 'make-buffer-file-executable-if-script-p)

(defun sm-try-simple-merge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (simple-merge-minor-mode-mode 1))))
;(add-hook 'find-file-hooks 'sm-try-simple-merge)

;;; 35prog.el ends here
