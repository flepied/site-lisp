;; 20dired.el --- 
;;
;; Copyright (C) 1996, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: dired.el,v 1.2 2001-10-24 17:38:08 flepied Exp $
;; Keywords: 
;;

;;=============================================================================
;; Autoloaded section.
;;=============================================================================

(autoload 'locate "locate"
 "Run the locate command, putting results in *Locate* buffer" t)
(autoload 'locate-with-filter "locate"
 "Run the locate command with a filter" t)

(autoload 'deb-view "deb-view"
  "View the control and data files of a Debian package archive file. Press \"q\"
in either window to kill both buffers.

In dired, press ^d on the dired line of the .deb file to view.
Or, execute: ESC x deb-view RET, and enter the .deb file name
at the prompt.

You are shown two tar files in tar-mode (see tar-mode for help).
In the case of old .deb format files, the control info is shown
but not the other files of control.tar, such as install scripts.
Additional features that deb-view adds to tar-mode:
q - kill both view buffers (INFO and DATA) and return to the
    dired buffer if that's where you executed deb-mode.
v - executes deb-view-tar-view instead of tar-view, with the
    additional smarts to uncompress .gz and .Z files for viewing.
N - Like in dired, formats man pages for viewing, with the
    additional smarts to uncompress .gz and .Z man files for viewing.
W - use w3-mode to view an HTML file.
These functions are also available in tar-mode on real tar files
when deb-view is loaded.

To view files not supported by deb-view, such as graphics, use the
copy command in tar-mode (\"c\") to copy the file to a temp directory.
You can then do what you want to the file. " t)

(autoload 'deb-view-dired-view "deb-view"
  "View the control and data files of the Debian package archive file on this line. Press \"q\"
in either window to kill both buffers and return to the dired buffer. See deb-view." t)


;;;###

;;;***


;;=============================================================================
;; Configuration section.
;;=============================================================================

;==============================================================================
; chargement de dired-x
;==============================================================================
(add-hook 'dired-load-hook
	  (function (lambda ()
		      (define-key dired-mode-map "U" 'dired-unmark-all-files-no-query)
		      (define-key dired-mode-map [right] 'dired-view-file)
		      (require 'dired-x)
		      (define-key dired-mode-map "\C-d" 'deb-view-dired-view)
		      )))
;; Essaie de deviner un nom pour les copies plus intelligeant que le répertoire
;; courant.
(setq dired-dwim-target t)

;; Pour ne plus aller chercher les .dired
(setq dired-local-variables-file nil)

(defadvice dired-get-filename (around check-mode activate)
  "Use an alternative function in Locate mode"
  (cond ((eq major-mode 'locate-mode)
	 (setq ad-return-value (locate-get-filename)))
	(t
	 ad-do-it)))

;;; 20dired.el ends here
