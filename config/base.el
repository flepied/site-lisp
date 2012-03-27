;; 00base.el --- 
;;
;; Copyright (C) 1996, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: base.el,v 1.9 2010-09-21 08:01:35 fred Exp $
;; Keywords: 
;;

;;=============================================================================
;; Autoloaded section.
;;=============================================================================
(autoload 'background "background"
  "Run COMMAND in the background like csh.  A message is displayed when
the job starts and finishes.  The buffer is in shell mode, so among
other things you can control the job and send input to it.  The
process object is returned if anyone cares.  See also shell-mode and
the variables background-show and background-select." t)

(autoload 'fnexpand-complete "fnexpand"
  "Expand the file name, env var or command near point" t)


;;;###

;;;***

;;;### (autoloads (update-file-header make-box-comment make-divisor make-revision make-header) "header" "../header.el" (13176 7651))
;;; Generated autoloads from ../header.el

(autoload (quote make-header) "header" "\
Makes a standard file header at the top of the buffer. A header is
   composed of a mode line, a body, and an end line.  The body is
   constructed by calling the functions in make-header-hooks.
   The mode line and end lines start and terminate block comments while the
   body lines just have to continue the comment. " t nil)

(autoload (quote make-revision) "header" "\
Inserts a revision marker after the history line.  Makes the history
   line if it does not already exist." t nil)

(autoload (quote make-divisor) "header" "\
A divisor line is the comment start, filler, and the comment end" t nil)

(autoload (quote make-box-comment) "header" "\
Inserts a box comment that is built using mode specific comment characters." t nil)

(autoload (quote update-file-header) "header" "\
If the file has been modified, searches the first header-max chars in the
   buffer using the regexps in file-header-update-alist. When a match is
   found, it applies the corresponding function with the point located just
   after the match.  The functions can use (match-beginning) and
   (match-end) calls to find out the strings that causes them to be invoked." t nil)

;;;***

;;;### (autoloads (recursive-edit-in-region apply-command-to-region) "vrac" "../vrac.el" (13179 16756))
;;; Generated autoloads from ../vrac.el

(autoload (quote apply-command-to-region) "vrac" "\
Execute a command with the buffer narrowed to the current region" t nil)

(autoload (quote recursive-edit-in-region) "vrac" "\
Enter recursive edit with buffer narrowed to region.
Upon exit from recursive edit, restore restrictions, point and mark." t nil)

;;;***


(defun global-change-directory (from to)
  "Change directory of all buffers with default-directory FROM to TO."
  (interactive "DGlobally change directory from: \nDTo: ")
  (let ((bufs (buffer-list))
	(from (expand-file-name from)))
    (while bufs
      (with-current-buffer (car bufs)
	(when (equal from (expand-file-name default-directory))
	  (setq default-directory to)))
      (setq bufs (cdr bufs)))))

;;=============================================================================
;; Configuration section.
;;=============================================================================

(defvar is-xemacs (string-match "xemacs" emacs-version)
  "")

;; pour les habitués à electric-accents.
(defalias 'electric-accents 'iso-accents-mode)

;;=============================================================================
;; Enleve le binding de f1 et help qui vient avec emacs 19.30 et +
;;=============================================================================
(setq help-event-list ())
(setq key-translation-map (make-sparse-keymap))

;;=============================================================================
;; Pour éviter les fichiers ~/.saves-*
;;=============================================================================
(setq auto-save-list-file-prefix nil)

;;=============================================================================
;; Sauvegarde des positions dans chaque fichier.
;;=============================================================================
(require 'saveplace)
(setq-default save-place t)

;;=============================================================================
;; Ligne de description etendue (nom fichier + arbo, date, heure, mail)
;;=============================================================================
(require 'mode-line)
(setq-default mode-line-format
  '("--"
    mode-line-modified
    "- L%l:%c -"
    mode-line-buffer-identification
    " %[("
    (-10 . mode-name) ; truncate mode name to 10 chars, it got too long - LRD
    minor-mode-alist
    "%n"
    mode-line-process
    " "
    (-3 . "%p") ; make string at most 3 chars: `Top', `Bot', or `nn%' - LRD
    ")%] "
    global-mode-string
    " %-"))

;; affiche le nom de fichier /home/titi/toto/ => ~toto/
(setq file-name-abbreviation-alist (nconc file-name-abbreviation-alist
					  (list (cons "^/home/\\([^/]+\\)/" "~\\1/"))))

;;==============================================================================
;; affiche le nom de la machine, le nom du buffer + l'heure et le mail dans
;; le titre de la frame.
;;==============================================================================
(defvar system-short-name (if (string-match "^\\([^.]+\\)." (system-name))
			      (substring (system-name) (match-beginning 1) (match-end 1))
			    (system-name))
  "System short name if your system is prep.mit.ai.edu, it would be \"prep\"")

(setq-default frame-title-format `(("" ,(user-login-name) "@" system-short-name) ": %b - " global-mode-string))

;;=============================================================================
;; Visualisation des lignes trop longues et tronquature des lignes par defaut.
;;=============================================================================
;;(require 'auto-show)
;;(set-default 'truncate-lines t )

;;=============================================================================
;; Update headers on write.
;;=============================================================================
(add-hook 'write-file-hooks 'update-file-header)
(setq  make-header-hooks '())

;;=============================================================================
;;=============================================================================
(eval-after-load "diff"
  '(defun diff-write-contents-hooks ()))

;;=============================================================================
;; Anonymous ftp if no user supplied.
;;=============================================================================
(setq ange-ftp-default-user "anonymous")

;;=============================================================================
;; imenu stuff
;;=============================================================================
;; (if (and nil (fboundp 'imenu--sort-by-name))
;;     (progn
;;       (setq imenu-sort-function 'imenu--sort-by-name)
;;       (define-key global-map [S-down-mouse-3] 'imenu)
;;       (require 'imenu)

;;       (add-hook 'find-file-hooks 'turn-on-imenu-if-enabled t)
;;       (defun turn-on-imenu-if-enabled ()
;; 	"Check if imenu can be turned on using an heuristic..."
;; 	(if (or (and (eq imenu-create-index-function
;; 			 'imenu-default-create-index-function)
;; 		     (or imenu-extract-index-name-function
;; 			 imenu-generic-expression))
;; 		(and imenu-create-index-function
;; 		     (not (eq imenu-create-index-function
;; 			      'imenu-default-create-index-function))))
;; 	    (condition-case nil
;; 		(imenu-add-to-menubar "Index")
;; 	      (error nil))))

;;=============================================================================
;; Affiche les noms des fonctions (index créés par imenu) dans le modeline.
;;=============================================================================
      ;; (setq which-func-unknown ""
      ;; 	    which-func-modes 'ANY
      ;; 	    which-func-amodes 'ANY
      ;; which-func-format '(" [" which-func-current "]"))
      ;; (load "which-function")
      ;; (which-func-mode)
      ;; ))

;;=============================================================================
;; pas de toolbar en emacs 21
;;=============================================================================
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))

;;=============================================================================
;; on-the fly (de)compression
;;=============================================================================
(if is-xemacs
    (require 'crypt)
  (require 'jka-compr)
  (setq auto-compression-mode 1))

;;=============================================================================
;; insert a time stamp under cursor
;;=============================================================================
(defun insert-time-stamp ()
  (interactive)
  (insert (format "%s" (current-time-string))))

;;=============================================================================
;; insert the path of the current file.
;;=============================================================================
(defun insert-path ()
  (interactive)
  (insert (or buffer-file-name default-directory)))

;;=============================================================================
;; Affectation des touches de fonctions.
;;=============================================================================
(define-key global-map "\C-m" 'newline-and-indent)
(define-key global-map "\C-j" 'newline)

(global-set-key '[f1]	'manual-entry) ; F1
(global-set-key '[f2]	'compile)	; F2
(global-set-key '[(control f2)]	'mode-compile)	; control F2 (mode-compile est defini plus tard...)
(global-set-key '[f3]	'(lambda() (interactive) (revert-buffer t t))) ; F3
(global-set-key '[f4]	'indent-region)	; F4
(global-set-key '[f5]	'ff-find-other-file) ; F5
(global-set-key '[(control f5)]	'(lambda() (interactive) (ff-find-other-file t))) ; control F5
(global-set-key '[f6]	'execute-extended-command) ; F6
(global-set-key '[f7]	'goto-line)	; F7
(global-set-key '[f8]	'insert-time-stamp) ; F8
(global-set-key '[f9]	'apply-command-to-region)	 ; F9
(global-set-key '[f10]	'next-error)	 ; F10
(global-set-key '[f11]	'fnexpand-complete)
(global-set-key '[f12]	'insert-path)	 ; F12

(global-set-key '[f22] 'ps-spool-buffer-with-faces) ;f22 is prsc
(global-set-key '[(shift f22)] 'ps-spool-region-with-faces)
(global-set-key '[(control f22)] 'ps-despool)
(global-set-key '[print] 'ps-spool-buffer-with-faces)
(global-set-key '[(shift print)] 'ps-spool-region-with-faces)
(global-set-key '[(control print)] 'ps-despool)

(if (not window-system)
    (progn
      (global-set-key '[kp-1]	'end-of-buffer)	 ; home
      (global-set-key '[kp-7]	'beginning-of-buffer)	 ; fin
      (global-set-key '[insertchar]	'overwrite-mode)	 ; inser
      ))

(if (locate-library "complete")
    (progn
      (global-set-key "\M-\r" 'complete)
      (global-set-key [?\C-\r] 'complete)
      (define-key function-key-map [(control return)] [?\C-\r])))

(define-key esc-map "&" 'background)

;;=============================================================================
;; Find file or url at point.
;;=============================================================================
(if (locate-library "ffap")
    (progn
      (autoload 'find-file-at-point "ffap" "" t)
      (global-set-key "\C-x\C-f" 'find-file-at-point)))

;;=============================================================================
;; Dynamic completion.
;;=============================================================================
(setq dabbrev-case-fold-search	nil
      dabbrev-case-replace	t)

;;==============================================================================
;; split the ediff windows vertically
;;==============================================================================
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-merge-split-window-function 'split-window-horizontally)

;;=============================================================================
;; visual bell
;;=============================================================================
(setq visible-bell t)

;;=============================================================================
;; narrow to a region for further editing on C-c [ and exit on C-c ]
;;=============================================================================
(define-key mode-specific-map "[" 'recursive-edit-in-region)
(define-key mode-specific-map "]" 'exit-recursive-edit)

;;=============================================================================
;; Pas de nouvelle ligne insérée à la fin du fichier quand on descend avec la
;; flêche.
;;=============================================================================
(setq next-line-add-newlines nil)
(setq scroll-step 1)			; on scroll d'une ligne a la fois

;;=============================================================================
;; pour avoir le minibuffer qui se retaille automatiquement
;;=============================================================================
(if (fboundp 'resize-minibuffer-mode)
    (resize-minibuffer-mode 1))

;;=============================================================================
;;=============================================================================
(add-hook 'minibuffer-setup-hook (function
				  (lambda()
				    (modify-syntax-entry ?_ "w")
				    (modify-syntax-entry ?- "w")
				    (modify-syntax-entry ?. "w"))))

;;==============================================================================
;; split the ediff windows vertically
;;==============================================================================
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-merge-split-window-function 'split-window-horizontally)

(global-set-key [(control c) ?=] (function (lambda() (interactive) (diff-backup (buffer-file-name (current-buffer))))))

;;==============================================================================
;; ignore les extensions .g et .so dans la completion des fichiers
;;==============================================================================
(setq completion-ignored-extensions (cons ".so" (cons ".g" completion-ignored-extensions)))

;;==============================================================================
;; pour X11.
;;==============================================================================
(cond (window-system
       ;; selection des buffers custom.
       (require 'msb)
       ;; extension pour la selection a la souris
       (require 'mouse-sel)
       ;; met en gris la selection
       (transient-mark-mode 1)
       ;; But only in the selected window
       (setq highlight-nonselected-windows nil)
       ;; Enable pending-delete
       (delete-selection-mode 1)
       ;; insertion a l'emplacement du curseur texte
       (setq mouse-yank-at-point t)
       ;; pas de cycle sur les clicks
       (setq mouse-sel-cycle-clicks nil)
       ))

;;==============================================================================
;; pour le man
;;==============================================================================
(setq Man-switches "-a")		; pour avoir toutes les pages

(add-hook 'Man-mode-hook
	  '(lambda ()
	     (Man-mouseify-xrefs)
	     (define-key Man-mode-map "\r" 'Man-do-manual-reference)
	     (define-key Man-mode-map "\t" 'Man-next-manual-reference)
	     (define-key Man-mode-map "\e\t" 'Man-prev-manual-reference)
	     (define-key Man-mode-map [mouse-2] 'Man-mouse-manual-reference)
	     ))

(autoload 'Man-mouseify-xrefs "man-xref" t)

;;==============================================================================
;; To work with dired right binding see 20dired.el
;;==============================================================================
(eval-after-load "view" '(define-key view-mode-map [left] 'View-exit))

;;; 00base.el ends here
