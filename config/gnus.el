;; 70gnus.el --- 
;;
;; Copyright (C) 1996, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: gnus.el,v 1.3 2001-10-22 16:12:49 flepied Exp $
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
(eval-after-load "message"
  '(setq message-required-news-headers (cons '(X-Attribution . x-attribution)
					    (cons '(X-Face . x-face)
						  message-required-news-headers))
	message-required-mail-headers (cons '(X-Attribution . x-attribution)
					    (cons '(X-Face . x-face)
						  message-required-mail-headers))))

(defvar x-face nil "*String used to add a X-Face header in outgoing GNUS message")

(defun x-face ()
  ""
  x-face)

(and (file-exists-p (expand-file-name "~/.x-face"))
     (load "vrac")
     (setq x-face (file->string "~/.x-face")))

(defvar x-attribution nil
  "*Default attribution name to insert in header for newsreader citation scheme")

(defun x-attribution()
  ""
  x-attribution)

;; Sauvegarde dans un fichier
(setq gnus-message-archive-group '((if (message-news-p)
				       "misc-news"
				     (concat "mail.sent." (format-time-string
							   "%Y-%m" (current-time))))))

(setq news-reply-header-hook nil)
(setq sc-auto-fill-region-p nil)	; pour ne pas remplir les paragraphes

;; Pour afficher la description du groupe en plus
(setq gnus-group-line-format "%M%S%5N: %G %O %z\t%D\n")

;; Pour afficher le nombre d'articles dans la thread et pour cacher les threads
;; par défaut.
(setq gnus-summary-line-format "%U%R%z%I%((%3t)%[%4L: %-20,20n%]%) %s\n" ;; "%U%R%z%I%((%3t)%[%4L: %-20,20%]%) %s\n"
      gnus-button-url 'w3-fetch
      gnus-asynchronous t

      ;; No question from gnus...
      gnus-break-pages nil
      gnus-novice-user nil
      gnus-subscribe-newsgroup-method 'gnus-subscribe-hierarchically
      gnus-interactive-catchup nil
      gnus-auto-select-first nil
      gnus-large-newsgroup ""

      gnus-thread-hide-subtree t)

;; Groupes de mail toujours visibles
(setq gnus-permanently-visible-groups "^nnml:")

;; Cache les signatures PGP, couleur max et date locale.
(add-hook 'gnus-startup-hook
	  (function
	   (lambda()
	     (add-hook 'gnus-article-display-hook 'gnus-article-highlight)
	     (remove-hook 'gnus-article-display-hook 'gnus-article-maybe-highlight)
	     (add-hook 'gnus-article-display-hook 'gnus-article-date-local)
	     (add-hook 'gnus-article-display-hook 'gnus-article-hide-pgp)
	     )))

;;; 70gnus.el ends here
