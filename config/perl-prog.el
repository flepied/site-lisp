;; 40perl-prog.el --- 
;;
;; Copyright (C) 1997, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: perl-prog.el,v 1.1 1999-12-09 07:34:42 flepied Exp $
;; Keywords: 
;;

(cfg-require 'prog)

;;=============================================================================
;; Autoloaded section.
;;=============================================================================


;;;###

;;;***


;;=============================================================================
;; Configuration section.
;;=============================================================================
(defun perldoc (arg)
  (interactive "sPerldoc entry : ")
  (let ((manual-program "perldoc")
	(Man-switches ""))
    (man arg)))

(defun config-cperl-hook ()
  (define-key cperl-mode-map [(shift f1)] 'perldoc))

(defun config-perl-hook ()
  (define-key perl-mode-map [(shift f1)] 'perldoc))

(eval-after-load "cperl-mode" '(config-cperl-hook))
(eval-after-load "perl-mode" '(config-perl-hook))

;;; 40perl-prog.el ends here
