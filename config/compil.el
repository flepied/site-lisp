;; 50compile.el --- 
;;
;; Copyright (C) 1996, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: compil.el,v 1.2 2004-05-05 06:57:04 flepied Exp $
;; Keywords: 
;;

(defvar config-use-igrep t)

;;=============================================================================
;; Autoloaded section.
;;=============================================================================
(if config-use-igrep
(progn
(autoload (function igrep) "igrep"
  "*Run `grep` PROGRAM to match EXPRESSION in FILES..." t)
(autoload (function igrep-find) "igrep"
  "*Run `grep` via `find`..." t)
(autoload (function dired-do-igrep) "igrep"
  "*Run `grep` on the marked (or next prefix ARG) files." t)
(autoload (function dired-do-igrep-find) "igrep"
  "*Run `grep` via `find` on the marked (or next prefix ARG) directories." t)
(autoload (function grep) "igrep"
  "*Run `grep` PROGRAM to match EXPRESSION in FILES..." t)
(autoload (function egrep) "igrep"
  "*Run `egrep`..." t)
(autoload (function fgrep) "igrep"
  "*Run `fgrep`..." t)
(autoload (function agrep) "igrep"
  "*Run `agrep`..." t)
(autoload (function grep-find) "igrep"
  "*Run `grep` via `find`..." t)
(autoload (function egrep-find) "igrep"
  "*Run `egrep` via `find`..." t)
(autoload (function fgrep-find) "igrep"
  "*Run `fgrep` via `find`..." t)
(autoload (function agrep-find) "igrep"
  "*Run `agrep` via `find`..." t)
(autoload (function dired-do-grep) "igrep"
  "*Run `grep` on the marked (or next prefix ARG) files." t)
(autoload (function dired-do-grep-find) "igrep"
  "*Run `grep` via `find` on the marked (or next prefix ARG) directories." t)
))

(autoload 'global-replace-lines "globrep"
  "Splice back to their original files the (possibly edited) output of grep.
This is a type of query-replace in which the original lines (in the
files) are replaced by those currently in the grep-output buffer.  You
will be asked to replace (y), don't replace (n), quit (q), or replace all.  

When all changes are done in a file, you will be asked to
either save and kill the buffer, simply save it, or leave it as is.
In the replacement text, all ^M are changed to newlines, to allow a
single line to be replaced by multiple lines.

Global-replace will check out files automatically when needed (and
will also offer to steal a lock as a last resort).  Files checked out
this way are automatically checked in." t)

(autoload 'global-replace "globrep"
  "From a grep output buffer, query replace STRING by REPLACEMENT.
Prefix argument or optional RESCAN forces rescanning of the *compilation*
buffer.  See also global-replace-lines for a more flexible approach." t)


;;;###

;;;***

;;;### (autoloads (mode-compile-kill mode-compile mode-compile-submit-bug-report) "mode-compile" "../mode-compile.el" (12104 64567))
;;; Generated autoloads from ../mode-compile.el

(defvar mode-compile-make-program "make" "\
*The `make' program used to process makefiles.

If you have GNU make installed with name \"gmake\" use it.")

(defvar mode-compile-ignore-makefile-backups t "\
*Tell mode compile to ignore makefiles backup files
when selecting the Makefile to use.")

(defvar mode-compile-default-make-options "-k" "\
*Default options to give to `make'.")

(defvar mode-compile-make-options mode-compile-default-make-options "\
*Options to give to `make'.
This could be any form evaluating to a string.

Some people asked me a way to modify the make options everytime
a compilation command is launched, do this:

 (defun my-mode-compile-ask-make-options()
   \"*Hook called by mode-compile, asking for make options.\"
   (interactive)
   (read-string \"Make options: \"
                mode-compile-default-make-options))
 (setq mode-compile-make-options
           'my-mode-compile-ask-make-options)

")

(defvar mode-compile-prefered-default-makerule (quote none) "\
*Default makerule you would like to see in minibuffer as a default
choice when selecting the make rule to build.

Possible values are:
'none    -- let mode-compile deciding for you.
'all     -- try hard to show you the \"all\" rule.
'default -- try hard to show you the \"default\" rule.
'file    -- try to show you the name of the file which will be
            result of compilation.
The 'none action is taken as default is something fail.")

(defvar mode-compile-ignore-makerule-regexp nil "\
*Makefile rules which must be ignored when building completion list.

For example if you want to remove all `files rules' set
it to: \"\\\\.\\\\([aoc]\\\\|s[ao][.0-9]*\\\\)\". ")

(defvar mode-compile-byte-compile-dir-interactive-p t "\
*Non nil means when byte-compiling a directory ask for each file
needing to be recompiled or not.")

(defvar mode-compile-save-all-p nil "\
*Non nil means save ALL the modified buffers without asking
before launching compilation command.")

(defvar mode-compile-always-save-buffer-p nil "\
*Non nil means save the current buffer without asking
before launching compilation command.")

(defvar mode-compile-never-edit-command-p nil "\
*Non nil means never ask to user to edit the compile command.")

(defvar mode-compile-other-screen-p nil "\
*Non nil means compile in another screen.
A new Emacs SCREEN is created and the compilation command is executed in this
other screen.
To specify the screen parameters see also variable
`mode-compile-screen-parameters-alist'.")

(defvar mode-compile-before-compile-hook nil "\
Hook to be run before compile command is executed 
when `mode-compile' is invoked.")

(defvar mode-compile-after-compile-hook nil "\
Hook to be run after compile command is executed 
when `mode-compile' is invoked.")

(defvar mode-compile-before-kill-hook nil "\
Hook to be run before killing compile command is executed 
when `mode-compile-kill' is invoked.")

(defvar mode-compile-after-kill-hook nil "\
Hook to be run after killing compile command is executed 
when `mode-compile-kill' is invoked.")

(defvar mode-compile-choosen-compiler nil "\
*Global variable containing the name of the compiler
which will be used for compiling without makefile.
 Could be used in combination with (cc|c++|ada|f77)-default-compiler-options
to automaticaly choose the compiler specific options.

example:
 (defun my-compiler-get-options()
   (cond
    ((string= mode-compile-choosen-compiler \"gcc\")
      \"-Wall -pedantic-errors\")
    ((string= mode-compile-choosen-compiler \"cc\")
      \"cc options whatever they are...\")
    (t
     (message \"Don't know this compiler: %s\" mode-compile-choosen-compiler)
     (read-string
      (format \"Options for %s compiler\" mode-compile-choosen-compiler)))))

  (setq cc-default-compiler-options 'my-compiler-get-options)

")

(defvar mode-compile-expert-p nil "\
*Non nil means `mode-compile' will not speaks too much.

See also variable variable mode-compile-reading-time. ")

(defvar mode-compile-reading-time 1 "\
*Seconds to wait in verbose mode after printing a message.

In verbose mode mode-compile print too much messages that it is allmost
impossible to read them. Just setting this delay leave you the time to
read all the messages. If you don't want any delay set it to `0'.

See also function sit-for.")

(defconst mode-compile-version "2.13" "\
Current version of mode-compile package.

mode-compile.el,v 2.13 1994/12/14 10:22:58 boubaker Exp
Please send bugs-fixes/contributions/comments to boubaker@dgac.fr")

(autoload (quote mode-compile-submit-bug-report) "mode-compile" "\
*Submit via mail a bug report on mode-compile v2.13." t nil)

(autoload (quote mode-compile) "mode-compile" "\
*Compile the file in the current buffer with a dynamically built command.
 
The command is built according to the current major mode the function was 
invoked from.
Currently know how to compile in: 
 `c-mode' ,              -- function cc-compile.
 `c++-mode',             -- function c++-compile.
 `ada-mode',             -- function ada-compile.
 `fortran-mode',         -- function f77-compile.
 `emacs-lisp-mode'       -- function elisp-compile.
 `lisp-interaction-mode' -- function elisp-compile.
 `makefile-mode'         -- function makefile-compile.
 `dired-mode'            -- function dired-compile.
 `sh-mode'               -- function sh-compile.
 `csh-mode'              -- function csh-compile.
 `zsh-mode'              -- function zsh-compile.
 `perl-mode'             -- function perl-compile.
 `fundamental-mode'      -- function guess-compile.
 `text-mode'             -- function guess-compile.
 `indented-text-mode'    -- function guess-compile.
 `compilation-mode'      -- function default-compile.
 The function `guess-compile' is called when mode is unknown. 

The variable `mode-compile-modes-alist' contain description of known modes.

The hooks variables `mode-compile-before-compile-hook' and 
`mode-compile-after-compile-hook' are run just before and after 
invoking the compile command of the mode.

Use the command `mode-compile-kill' (\\[mode-compile-kill]) to abort a 
running compilation.

Bound on \\[mode-compile]." t nil)

(autoload (quote mode-compile-kill) "mode-compile" "\
*Kill the running compilation launched by `mode-compile' (\\[mode-compile]) command.
 
The compilation command is killed according to the current major mode 
the function was invoked from.
Currently know how to kill compilations from: 
 `c-mode' ,              -- function kill-compilation.
 `c++-mode' ,            -- function kill-compilation.
 `ada-mode' ,            -- function kill-compilation.
 `fortran-mode' ,        -- function kill-compilation.
 `emacs-lisp-mode'       -- function keyboard-quit.
 `lisp-interaction-mode' -- function keyboard-quit.
 `makefile-mode'         -- function kill-compilation.
 `dired-mode'            -- function kill-compilation.
 `sh-mode'               -- function kill-compilation.
 `csh-mode'              -- function kill-compilation.
 `zsh-mode'              -- function kill-compilation.
 `perl-mode'             -- function kill-compilation.
 `fundamental-mode'      -- Bound dynamically.
 `text-mode'             -- Bound dynamically.
 `indented-text-mode'    -- Bound dynamically.
 `compilation-mode'      -- function kill-compilation.

The variable `mode-compile-modes-alist' contain description of 
ALL known modes.

The hooks variables `mode-compile-before-kill-hook' and 
`mode-compile-after-kill-hook' are run just before and after 
invoking the kill compile command of the mode.

Bound on \\[mode-compile-kill]." t nil)

;;;***


;;=============================================================================
;; Configuration section.
;;=============================================================================

;;==============================================================================
;; positionne le cusrseur a la fin du buffer de compilation
;;==============================================================================
(defadvice compile-internal (after my-scroll act comp)
  "Forces compile buffer to scroll. See around line 363 in compile.el"
  (let* ((ob (current-buffer)))
    (save-excursion
      (select-window (get-buffer-window ad-return-value t))
      (goto-char (point-max))
      (select-window (get-buffer-window ob t))
      )))

;;=============================================================================
;; One compilation buffer per directory.
;;=============================================================================
(make-variable-buffer-local 'compile-command)
(setq-default compile-command "make -k")

(defun fred-compilation-buffer-name-function (Q)
  "Establish the name of the buffer created by `compile'"
  (let* ((path	(expand-file-name default-directory))
	 (dir	(and (string-match "\\([^/]+\\)/$" path)
		     (substring path (match-beginning 1) (match-end 1)))))
    (concat "*" dir ".compil*")))

(setq compilation-buffer-name-function 'fred-compilation-buffer-name-function)

;;=============================================================================
;; Font-lock setup for compilation mode.
;;=============================================================================
(setq font-lock-warning-face 'font-lock-function-name-face)
(setq font-lock-error-face 'font-lock-comment-face)

(setq compilation-mode-font-lock-keywords
  ;; This regexp needs a bit of rewriting.  What is the third grouping for?
  '(("^\\([^ \n:]*:\\([0-9]+:[ \t]*warning:\\)+\\)\\(.*\\)$" 3 font-lock-warning-face)
    ("^\\([^ \n:]*:\\([0-9]+:\\)+\\)\\(.*\\)$" 3 font-lock-error-face))
)

;;=============================================================================
;; misc grep functions
;;=============================================================================
(and config-use-igrep
(setq igrep-options "-i")		; ignore case
)

(setq igrep-use-zgrep nil)		; use zgrep

(global-set-key [(control c) ?c] 'mode-compile)
(global-set-key [(control c) ?k] 'mode-compile-kill)

;;; 50compile.el ends here
