;; 50cfg.el --- 
;;
;; Copyright (C) 1996, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: 50cfg.el,v 1.1 1999-12-09 07:34:42 flepied Exp $
;; Keywords: 
;;

;;=============================================================================
;; Autoloaded section.
;;=============================================================================


;;;###

;;;***

;;;### (autoloads (update-config-autoloads cfg-add cfg-require cfg-load-all load-cfgs) "cfg" "../cfg.el" (13277 38430))
;;; Generated autoloads from ../cfg.el

(autoload (quote load-cfgs) "cfg" "\
Load all the config by calling `cfg-load-all'." nil (quote macro))

(autoload (quote cfg-load-all) "cfg" "\
Load all the config specified by `cfg-requested'" nil nil)

(autoload (quote cfg-require) "cfg" "\
Load the cfg searching in `cfg-dirs'." nil nil)

(autoload (quote cfg-add) "cfg" "\
Add a config to be loaded by `config-load-all'." nil nil)

(autoload (quote update-config-autoloads) "cfg" "\
Update autoloads from first arg and save them in second arg" t nil)

;;;***


;;=============================================================================
;; Configuration section.
;;=============================================================================
(setq cfg-dirs '("/etc/emacs/config/"))
(mapcar 'cfg-add '(base debian))

;;; 50cfg.el ends here
