;; html.el --- 
;;
;; Copyright (C) 1998, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: html.el,v 1.1 1999-12-09 07:34:42 flepied Exp $
;; Keywords: 
;;

(defvar config-use-iso-sgml t)

;;=============================================================================
;; Autoloaded section.
;;=============================================================================


;;;***


;;=============================================================================
;; Transparent accent coding/decoding for html and sgml modes.
;;=============================================================================
(and config-use-iso-sgml
(require 'iso-sgml)
)

;;=============================================================================
;; powerfull html mode
;;=============================================================================
(if (featurep 'imenu)
    (progn
      ;; Make an index for imenu
      (defun html-helper-imenu-index ()
	"Return an table of contents for an html buffer for use with Imenu."
	(let ((space ? ) ; a char
	      (toc-index '())
	      toc-str)
	  (save-excursion
	    (goto-char (point-min))
	    (while (re-search-forward "^\\s-*<h\\([1-9]\\)>\\([^\n<>]*\\)" nil t)
	      (setq toc-str 
		    (concat 
		     (make-string 
		      (* 2 (- (string-to-number (match-string 1)) 1)) 
		      space)
		     (match-string 2)))
	      (beginning-of-line)
	      (setq toc-index (cons (cons toc-str (point)) toc-index))
	      (end-of-line)
	      ))
	  (nreverse toc-index)))
      
      (defun html-helper-imenu-setup ()
	(setq imenu-sort-function nil
	      imenu-create-index-function 'html-helper-imenu-index)
	(imenu-add-to-menubar "TOC")
	)
      
      (add-hook 'html-helper-mode-hook 'html-helper-imenu-setup)
      ))

(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))

;;; html.el ends here
