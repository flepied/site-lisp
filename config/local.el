;; 00local.el --- 
;;
;; Copyright (C) 1996, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: local.el,v 1.1 1999-12-09 07:34:42 flepied Exp $
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
;; Pour le calendrier.
;;=============================================================================
;; Latitude longitude balise Orly
(setq calendar-latitude 48.73
      calendar-longitude 2.383)

;;==============================================================================
;; Définitions des proxies.
;;==============================================================================
(setenv "http_proxy" "http://www:8080")
;(setenv "ftp_proxy" "http://www:8080")

(setq elisp-archive-host "ftp.ibp.fr"
      elisp-archive-directory "/pub/emacs/elisp-archive/")
      
;;; 00local.el ends here
