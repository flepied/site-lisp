;; rpm.el --- 
;;
;; Copyright (C) 1999, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: rpm.el,v 1.1 1999-12-09 07:34:42 flepied Exp $
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

;;; rpm.el ends here
