;; 00options.el --- 
;;
;; Copyright (C) 1996, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: options.el,v 1.1 1999-12-09 07:34:42 flepied Exp $
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

;;=============================================================================
;; command line options
;;=============================================================================
(setq command-switch-alist (cons '("-desktop" . desktop-init)
				 (cons '("-minitel" . minitel-setup)
				       command-switch-alist) ) )

(defun desktop-init (name)
  (require 'desktop)
  (desktop-load-default)
  (require 'desktop-phase) ;; <--- has to be loaded before the buffers are created...
  (desktop-read)
  )

(defun minitel-setup (name)
  (setq baud-rate 1200) )

;;; 00options.el ends here
