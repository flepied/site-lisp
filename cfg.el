;;; cfg.el --- utilities to maintain large site-lisp.
;;
;; Copyright (C) 1997, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Created: Fri Jul 25 20:08:33 1997
;; Version: $Id$
;; Keywords: maint
;;
;; LCD Archive Entry:
;; cfg|Frederic Lepied|Frederic.Lepied@sugix.frmug.org|
;; utilities to maintain large site-lisp.|
;; $Date$|$Revision$|~/path/cfg.el.Z|
;; 
;; This file is NOT part of GNU Emacs but the same permissions apply.
;;
;; GNU Emacs  is free software;  you can redistribute it and/or modify
;; it under the terms of  the GNU General  Public License as published
;; by  the Free Software  Foundation;  either version  2, or (at  your
;; option) any later version.
;;
;; GNU  Emacs is distributed  in the hope that  it will be useful, but
;; WITHOUT    ANY  WARRANTY;  without even the     implied warranty of
;; MERCHANTABILITY or FITNESS FOR A  PARTICULAR PURPOSE.  See the  GNU
;; General Public License for more details.
;;
;; You should have received  a copy of  the GNU General Public License
;; along with GNU Emacs; see  the file COPYING.  If  not, write to the
;; Free Software Foundation, 675  Mass Ave, Cambridge, MA 02139,  USA.
;; This  program  is free  software;  you  can  redistribute it and/or
;; modify it  under  the terms of the  GNU  General Public License  as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;;; Commentary:

;;; Code:

;;============================================================================
;; Utilities  to  maintain multiple cfguration  files which  can be
;; loaded individually. This is usefull for  big emacs site to provide
;; multiple cfguration for different usages.
;;============================================================================

(defvar cfg-dirs ()
  "List of directories containing config files.
Directory names must end with a /.")

(defvar cfg-loaded ()
  "List of config already loaded.")

(defvar cfg-requested ()
  "List of config to load.")

;;;###autoload
(defmacro load-cfgs (&rest cfgs)
  "Load all the config by calling `cfg-load-all'."
  `(progn
     (mapcar 'cfg-add ',cfgs)
     (cfg-load-all) ) )

;;;###autoload
(defun cfg-load-all ()
  "Load all the config specified by `cfg-requested'"
  (mapcar 'cfg-require cfg-requested) )

;;;###autoload
(defun cfg-require (cfg)
  "Load the cfg searching in `cfg-dirs'."
  (or (memq cfg cfg-loaded)
      (let ((pathname (cfg-find-path (symbol-name cfg) cfg-dirs)))
	(if (not pathname)
	    (message "!!! Unable to find the file for %s !!!" cfg)
	  (if (not (cfg-load-file pathname))
	      (message "!!! Error while loading %s !!!" pathname)
	    (message "*** Config %s loaded ***" cfg)
	    (setq cfg-loaded (cons cfg cfg-loaded)) ) ) ) ) )

;;;###autoload
(defun cfg-add (cfg)
  "Add a config to be loaded by `config-load-all'."
  (add-hook 'cfg-requested cfg) )

(defun cfg-load-file (file)
  "Load a file without breaking on error."
  (if debug-on-error
      (load-file file)
    (condition-case nil
	(load file nil t t)
      (error nil) ) ) )

(defun cfg-find-path (cfg dirs)
  (if (null dirs)
      nil
    (let ((path (expand-file-name (concat (car dirs) cfg ".el"))))
      (if (file-exists-p path)
	  path
	(cfg-find-path cfg (cdr dirs)) ) ) ) )

;;============================================================================
;; Utilities to maintain autoload statements in files.
;;============================================================================

;;;###autoload
(defun update-config-autoloads (autoload cfg)
  "Update autoloads from first arg and save them in second arg"
  (interactive "fUpdate autoloads from : \nfSave generated autoloads in : ")
  (require 'autoload)
  (let ((old generated-autoload-file)
	(generated-autoload-file cfg))
    ;; I use set-variable because generated-autoload-file is a defconst...
    (message "updating %s autoloads into %s" autoload generated-autoload-file)
    (update-file-autoloads autoload)
    (set-variable 'generated-autoload-file old)
    ))

(defun update-configs-autoloads (base cfgs)
  "Update autoloads in cfgs"
  (while cfgs
    (let ((cfg	(caar cfgs))
	  (files	(cadar cfgs)))
      (while files
	(update-config-autoloads (concat base (car files))
				 (concat base cfg))
	(setq files (cdr files)))
      (setq cfgs (cdr cfgs)))))

;;============================================================================
;; Clean up load-path from invalid directory paths.
;;============================================================================
(defun clean-up-load-path ()
  (interactive)
  (let ((p load-path))
    (while p
      (if (not (file-accessible-directory-p (car p)))
	  (setq load-path (delq (car p) load-path)))
      (setq p (cdr-safe p)))))

;;============================================================================

(provide 'cfg)

;;; cfg.el ends here
