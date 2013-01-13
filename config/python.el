;; 40python-prog.el --- 
;;
;; Copyright (C) 1996, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: python.el,v 1.1 1999-12-09 07:34:42 flepied Exp $
;; Keywords: 
;;

;;=============================================================================
;; Autoloaded section.
;;=============================================================================


;;;###

;;;***

;;;### (autoloads (pdb) "pdb" "../pdb.el" (13093 27401))
;;; Generated autoloads from ../pdb.el

(autoload (quote pdb) "pdb" "\
Run pdb on program FILE in buffer *gud-FILE*.
      The directory containing FILE becomes the initial working directory
      and source-file directory for your debugger." t nil)



(autoload (quote nosetests-compile) "nosetests" "" t nil)

;;=============================================================================
;; Configuration section.
;;=============================================================================
(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))

(when (load "flymake" t)

  (defun flymake-pyflakes-init () 
    (let* ((temp-file (flymake-init-create-temp-buffer-copy 
		       'flymake-create-temp-inplace)) 
	   (local-file (file-relative-name 
			temp-file 
			(file-name-directory buffer-file-name)))) 
      (list "pyflakes" (list local-file)))) 

  (add-to-list 'flymake-allowed-file-name-masks 
	       '("\\.py\\'" flymake-pyflakes-init))

  (add-hook 'find-file-hook 'flymake-find-file-hook)
  )


(defun pydoc (arg)
  (interactive "sPydoc entry : ")
  (let ((manual-program "pydoc")
	(Man-switches ""))
    (man arg)))

(add-hook 'python-mode-hook (function (lambda()
					(define-key python-mode-map [(shift f1)] 'pydoc)
					(define-key python-mode-map (kbd "C-S-t") (function nosetests-compile))
					;(require 'pymacs)
					;(pymacs-load "ropemacs" "rope-")
					) ))


;;; 40python-prog.el ends here
