;; single.el --- 
;;
;; Copyright (C) 1998, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: single.el,v 1.1 1999-12-09 07:34:42 flepied Exp $
;; Keywords: 
;;

;;=============================================================================
;; Autoloaded section.
;;=============================================================================
(autoload 'feedmail-send-it "feedmail")


;;;###

;;;***

;;;### (autoloads (feedmail-queue-reminder feedmail-run-the-queue feedmail-run-the-queue-global-prompt feedmail-run-the-queue-no-prompts) "feedmail" "feedmail.el" (13704 35449))
;;; Generated autoloads from feedmail.el

(autoload (quote feedmail-run-the-queue-no-prompts) "feedmail" "\
Like feedmail-run-the-queue, but suppress confirmation prompts." t nil)

(autoload (quote feedmail-run-the-queue-global-prompt) "feedmail" "\
Like feedmail-run-the-queue, but with a global confirmation prompt.
This is generally most useful if run non-interactively, since you can 
bail out with an appropriate answer to the global confirmation prompt." t nil)

(autoload (quote feedmail-run-the-queue) "feedmail" "\
Visit each message in the feedmail queue directory and send it out.
Return value is a list of three things: number of messages sent, number of
messages skipped, and number of non-message things in the queue (commonly
backup file names and the like)." t nil)

(autoload (quote feedmail-queue-reminder) "feedmail" "\
Perform some kind of reminder activity about queued and draft messages.
Called with an optional symbol argument which says what kind of event
is triggering the reminder activity.  The default is 'on-demand, which
is what you typically would use if you were putting this in your emacs start-up
or mail hook code.  Other recognized values for WHAT-EVENT (these are passed
internally by feedmail):

   after-immediate      (a message has just been sent in immediate mode)
   after-queue          (a message has just been queued)
   after-draft          (a message has just been placed in the draft directory)
   after-run            (the queue has just been run, possibly sending messages)

WHAT-EVENT is used as a key into the table feedmail-queue-reminder-alist.  If
the associated value is a function, it is called without arguments and is expected
to perform the reminder activity.  You can supply your own reminder functions 
by redefining feedmail-queue-reminder-alist.  If you don't want any reminders, 
you can set feedmail-queue-reminder-alist to nil." t nil)

;;;***


;;=============================================================================
;; Configuration section.
;;=============================================================================
(setq send-mail-function 'feedmail-send-it)

(setq feedmail-enable-queue t)
(setq auto-mode-alist (cons '("\\.fqm$" . mail-mode) auto-mode-alist))

;; for message.el
(setq message-send-mail-function 'feedmail-send-it)
(add-hook 'message-mail-send-hook 'feedmail-mail-send-hook-splitter)

;; for desktop.el
(setq desktop-missing-file-warning nil)
(setq desktop-buffer-age-limit 10)
(eval-after-load "desktop" '(setq desktop-locals-to-save
				  (append desktop-locals-to-save
					  '(compile-command))))
(eval-after-load "desktop"
  '(setq desktop-files-not-to-save (concat ".fqm$\\|" desktop-files-not-to-save)))

;; for gracefull exit
(add-hook 'kill-emacs-hook (function (lambda()
				       ;; only run the queue if feedmail has
				       ;; already been loaded.
				       (if (fboundp 'feedmail-buffer-to-smtpmail)
					   (feedmail-run-the-queue-global-prompt)))))

;; email sender
(setq feedmail-binmail-template "/usr/sbin/sendmail -t")

;;; single.el ends here
