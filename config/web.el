;; 60web.el --- 
;;
;; Copyright (C) 1996, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: web.el,v 1.1 1999-12-09 07:34:42 flepied Exp $
;; Keywords: 
;;

;;=============================================================================
;; Autoloaded section.
;;=============================================================================


;;;### (autoloads (slashdot) "slashdot" "../slashdot.el" (14334 61773))
;;; Generated autoloads from ../slashdot.el

(autoload (quote slashdot) "slashdot" "\
Attempt to view the current Slashdot headlines.
This function will open a connection and download the latest headlines from the
slashdot.org, \"News for Nerds\" website, and display them.  This will also
place clickable links to allow you to read the articles with either `w3', or
and external browser." t nil)

;;;***

;;;### (autoloads (webjump) "webjump" "../webjump.el" (12844 63209))
;;; Generated autoloads from ../webjump.el

(autoload (quote webjump) "webjump" "\
Jumps to a Web site from a programmable hotlist.

See the documentation for the `webjump-sites' variable for how to customize the
hotlist.

Feedback on WebJump can be sent to the author, Neil W. Van Dyke <nwv@acm.org>,
or submitted via `\\[webjump-submit-bug-report]'.  The latest version can be
gotten from `http://www.cs.brown.edu/people/nwv/'." t nil)

;;;***


;;=============================================================================
;; Configuration section.
;;=============================================================================

;;=============================================================================
;; www browsing
;;=============================================================================
(add-hook 'w3-load-hooks (function (lambda ()
				     (fset 'url-create-cached-filename
					   'url-create-cached-filename-using-md5))))

(add-hook 'w3-mode-hook (function (lambda() (local-set-key "" (quote find-file)))))

(global-set-key "\C-c\C-j" 'webjump)

(defun dired-open-w3 ()
  "In dired mode open the current file via `w3-open-local'"
  (interactive)
  (w3-open-local (dired-get-filename)))

(add-hook 'dired-mode-hook (function (lambda()
				       (local-set-key "W" 'dired-open-w3))))

;;; 60web.el ends here
