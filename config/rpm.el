;; rpm.el --- 
;;
;; Copyright (C) 1999, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: rpm.el,v 1.3 2001-10-22 16:13:44 flepied Exp $
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

(defun my-spec-cleanup ()
  (save-excursion
    (beginning-of-buffer)
    (if (re-search-forward "^Copyright:" nil t)
	(replace-match "License:" nil t))
    (beginning-of-buffer)
    (if (re-search-forward "^Serial:" nil t)
	(replace-match "Epoch:" nil t))
      ))

(add-hook 'rpm-spec-mode-hook (function my-spec-cleanup))

;;; rpm.el ends here
