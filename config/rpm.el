;; rpm.el --- 
;;
;; Copyright (C) 1999, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: rpm.el,v 1.2 2000-08-24 12:45:40 flepied Exp $
;; Keywords: 
;;

;;=============================================================================
;; Autoloaded section.
;;=============================================================================

(autoload 'rpm-spec-mode "rpm-spec-mode-mdk.el" "RPM spec mode." t)


;;;###

;;;***


;;=============================================================================
;; Configuration section.
;;=============================================================================

(setq auto-mode-alist (append '(("\\.spec" . rpm-spec-mode))
			      auto-mode-alist))

(defvar ffap-spec-path '("../SOURCES"))
(defun ffap-spec (name) (ffap-locate-file name t ffap-spec-path))
(require 'ffap)
(add-to-list 'ffap-alist (cons 'rpm-spec-mode 'ffap-spec))

;;; rpm.el ends here
