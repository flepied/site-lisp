;; 65mail-news.el --- 
;;
;; Copyright (C) 1996, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: mail-news.el,v 1.2 2004-06-17 07:00:22 flepied Exp $
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
(setq bbdb-completion-type 'primary-or-name)

(autoload 'newsticker-start "newsticker" "Emacs Newsticker" t)

(defvar gnus-summary-print-command "a2ps -g -d --strip=1 -Email -q"
  "*command used to print an article in Summary mode")

(defun gnus-summary-print ()
  "Print the current article using the commande configured in
`gnus-summary-print-command'."
  (interactive)
  (gnus-summary-select-article nil t t)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (save-restriction
      (widen)
      (shell-command-on-region (point-min) (point-max) gnus-summary-print-comma\nd nil)
      (setq gnus-last-shell-command gnus-summary-print-command)
      )
    ))

(add-hook 'gnus-summary-prepare-hook
          '(lambda ()
             (define-key
               gnus-summary-mode-map "i" 'gnus-summary-print)))


(defun cto ()
  (interactive)
  (save-excursion
    (let (end)
      (end-of-buffer)
      (setq end (point))
      (if (re-search-backward message-signature-separator nil t)
	  (delete-region (- (point) 1) end)))
    (let ((message-signature "Frédéric Lepied - Mandrakesoft CTO"))
      (message-insert-signature))))

;;; 65mail-news.el ends here
