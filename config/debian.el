;; 00debian.el --- 
;;
;; Copyright (C) 1997, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: debian.el,v 1.1 1999-12-09 07:34:42 flepied Exp $
;; Keywords: 
;;

;;=============================================================================
;; Autoloaded section.
;;=============================================================================


;;;###

;;;***


;;=============================================================================
;; Configuration section.
;;=============================================================================
(defun clean-mailname ()
  (goto-char 0)
  (replace-string "\n" ""))

(let ((mailname (debian-file->string "/etc/mailname" (function clean-mailname))))
  (if (not mailname)
      (progn
	(message "No /etc/mailname. Reverting to default...")
	(sit-for 3))
    (setq mail-host-address mailname
	  user-mail-address (concat (user-login-name) "@" mailname)
	  ange-ftp-default-password user-mail-address
	  )))

;;; 00debian.el ends here
