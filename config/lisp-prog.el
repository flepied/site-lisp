;; 40lisp-prog.el --- 
;;
;; Copyright (C) 1996, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: lisp-prog.el,v 1.1 1999-12-09 07:34:42 flepied Exp $
;; Keywords: 
;;

(cfg-require 'prog)

;;=============================================================================
;; Autoloaded section.
;;=============================================================================
(autoload 'format-lisp-code-directory "lispdir"
  "Convert GNU Emacs Lisp Code Directory into something a human could read.
Calls value of lisp-dir-hook with no args if that value is non-nil." t)
(autoload 'lisp-dir-apropos "lispdir"
  "Display entries in Lisp Code Directory for TOPIC in separate window.
Calls value of lisp-dir-apropos-hook with no args if that value is non-nil." t)
(autoload 'lisp-dir-retrieve "lispdir"
  "Retrieves a copy of the NAMEd package.  The NAME must be an exact match.
Calls value of lisp-dir-retrieve-hook with no args if that value is non-nil." t)
(autoload 'lisp-dir-verify "lispdir"
  "Verifies the archive location of the NAMEd package using ange-ftp." t)

(autoload 'vapropos-apropos-variable "vapropos"
  "Show variables or custom options that match REGEXP." t nil)
(autoload 'vapropos-apropos-command "vapropos"
  "Show commands or functions that match REGEXP." t nil)


;;;###

;;;***

;;;### (autoloads (checkdoc-minor-mode checkdoc-defun checkdoc-eval-defun checkdoc checkdoc-current-buffer checkdoc-eval-current-buffer) "checkdoc" "../checkdoc.el" (13168 6891))
;;; Generated autoloads from ../checkdoc.el

(autoload (quote checkdoc-eval-current-buffer) "checkdoc" "\
Evaluate and check documentation for the current buffer.
Evaluation is done first because good documentation for something that
doesn't work is just not useful.  Comments, docstrings, and rogue
spacing are all verified." t nil)

(autoload (quote checkdoc-current-buffer) "checkdoc" "\
Check the current buffer for doc style, comment style, and rogue spaces.
Optional argument TAKE-NOTES non-nil will store all found errors in a
warnings buffer, otherwise it stops after the first error." t nil)

(autoload (quote checkdoc) "checkdoc" "\
Find the first doc string in the current buffer which is stylisticly poor.
Prefix argument TAKE-NOTES means to continue through the whole buffer and
save warnings in a separate buffer." t nil)

(autoload (quote checkdoc-eval-defun) "checkdoc" "\
Evaluate the current form with `eval-defun' and check it's documentaion.
Evaluation is done first so the form will be read before the
documentation is checked.  If there is a doc error, then the display
of what was evaluated will be overwritten by the diagnostic message." t nil)

(autoload (quote checkdoc-defun) "checkdoc" "\
Examine the doc string of the function or variable under point.
Calls `error' if the doc string produces diagnostics.  If NO-ERROR is
non-nil, then do not call error, but call `message' instead.
If the document check passes, then check the function for rogue white
space at the end of each line." t nil)

(autoload (quote checkdoc-minor-mode) "checkdoc" "\
Toggle Checkdoc minor mode.  A mode for checking lisp doc strings.
With prefix ARG, turn Checkdoc minor mode on iff arg is positive.

In checkdoc minor mode, the usual bindings for `eval-defun' which is
bound to \\<checkdoc-minor-keymap> \\[checkdoc-eval-defun] and `eval-current-buffer' are overridden to include
checking of documentation strings.

\\{checkdoc-minor-keymap}" t nil)

;;;***

;;;### (autoloads (update-config-autoloads) "config" "../config.el" (13179 16144))
;;; Generated autoloads from ../config.el

(autoload (quote update-config-autoloads) "config" "\
Update autoloads from first arg and save them in second arg" t nil)

;;;***

;;;### (autoloads (eldoc-print-fnsym-args turn-on-eldoc-mode eldoc-mode) "eldoc" "../eldoc.el" (12455 48805))
;;; Generated autoloads from ../eldoc.el

(defvar eldoc-mode nil "\
*If non-nil, show the defined parameters for the elisp function near point.

For the emacs lisp function at the beginning of the sexp which point is
within, show the defined parameters for the function in the echo area.
This information is extracted directly from the function or macro if it is
in pure lisp.

If the emacs function is a subr, the parameters are obtained from the
documentation string if possible.

If point is over a documented variable, print that variable's docstring
instead; see function `eldoc-print-var-docstring'.

This variable is buffer-local.")

(autoload (quote eldoc-mode) "eldoc" "\
*If non-nil, then enable eldoc-mode (see variable docstring)." t nil)

(autoload (quote turn-on-eldoc-mode) "eldoc" "\
Unequivocally turn on eldoc-mode (see variable documentation)." t nil)

(autoload (quote eldoc-print-fnsym-args) "eldoc" "\
*Show the defined parameters for the function near point.
For the function at the beginning of the sexp which point is within, show
the defined parameters for the function in the echo area.
This information is extracted directly from the function or macro if it is
in pure lisp.
If the emacs function is a subr, the parameters are obtained from the
documentation string if possible." t nil)

;;;***


(defun indirect-emacs-lisp-buffer (arg)
  "Edit Emacs Lisp code in this buffer in another window.
Optional argument ARG is number of sexps to include in that buffer."
  (interactive "P")
  (let ((buffer-name (generate-new-buffer-name " *elisp*"))
	(arg (if arg (prefix-numeric-value arg) 0)))
    (pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))
    (emacs-lisp-mode)
    (narrow-to-region (point) (save-excursion (forward-sexp arg) (point)))))

;;=============================================================================
;; Configuration section.
;;=============================================================================
(defun config-lisp-hook ()
  (foldingo-activation))

(add-hook 'emacs-lisp-mode-hook (function (lambda()
					    (local-set-key '[f5] 'eval-region)	; F5
					    (local-set-key '[f6] 'eval-current-buffer)	; F5
					    (turn-on-eldoc-mode)
					    (config-lisp-hook))))
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-mode-hook (function config-lisp-hook))

(setq lisp-code-directory "/usr/doc/site-lisp/LCD-datafile.gz")

;;; 40lisp-prog.el ends here
