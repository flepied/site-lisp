;; 40python-prog.el --- 
;;
;; Copyright (C) 1996, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: python.el,v 1.1 1999-12-09 07:34:42 flepied Exp $
;; Keywords: 
;;

;;=============================================================================
;; Autoloaded section.
;;=============================================================================


;;;###

;;;***

;;;### (autoloads (pdb) "pdb" "../pdb.el" (13093 27401))
;;; Generated autoloads from ../pdb.el

(autoload (quote pdb) "pdb" "\
Run pdb on program FILE in buffer *gud-FILE*.
      The directory containing FILE becomes the initial working directory
      and source-file directory for your debugger." t nil)

;;;***

;;;### (autoloads (py-shell python-mode) "python-mode" "../python-mode.el" (13093 27690))
;;; Generated autoloads from ../python-mode.el

(eval-when-compile (condition-case nil (progn (require (quote cl)) (require (quote imenu))) (error nil)))

(autoload (quote python-mode) "python-mode" "\
Major mode for editing Python files.
To submit a problem report, enter `\\[py-submit-bug-report]' from a
`python-mode' buffer.  Do `\\[py-describe-mode]' for detailed
documentation.  To see what version of `python-mode' you are running,
enter `\\[py-version]'.

This mode knows about Python indentation, tokens, comments and
continuation lines.  Paragraphs are separated by blank lines only.

COMMANDS
\\{py-mode-map}
VARIABLES

py-indent-offset		indentation increment
py-block-comment-prefix		comment string used by comment-region
py-python-command		shell command to invoke Python interpreter
py-scroll-process-buffer		always scroll Python process buffer
py-temp-directory		directory used for temp files (if needed)
py-beep-if-tab-change		ring the bell if tab-width is changed" t nil)

(autoload (quote py-shell) "python-mode" "\
Start an interactive Python interpreter in another window.
This is like Shell mode, except that Python is running in the window
instead of a shell.  See the `Interactive Shell' and `Shell Mode'
sections of the Emacs manual for details, especially for the key
bindings active in the `*Python*' buffer.

See the docs for variable `py-scroll-buffer' for info on scrolling
behavior in the process window.

Warning: Don't use an interactive Python if you change sys.ps1 or
sys.ps2 from their default values, or if you're running code that
prints `>>> ' or `... ' at the start of a line.  `python-mode' can't
distinguish your output from Python's output, and assumes that `>>> '
at the start of a line is a prompt from Python.  Similarly, the Emacs
Shell mode code assumes that both `>>> ' and `... ' at the start of a
line are Python prompts.  Bad things can happen if you fool either
mode.

Warning:  If you do any editing *in* the process buffer *while* the
buffer is accepting output from Python, do NOT attempt to `undo' the
changes.  Some of the output (nowhere near the parts you changed!) may
be lost if you do.  This appears to be an Emacs bug, an unfortunate
interaction between undo and process filters; the same problem exists in
non-Python process buffers using the default (Emacs-supplied) process
filter." t nil)

;;;***


;;=============================================================================
;; Configuration section.
;;=============================================================================
(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))

;;; 40python-prog.el ends here
