;;; debian-bug.el --- report a bug to Debian's bug tracking system

;; Copyright (C) 1998, 1999 Free Software Foundation, Inc.

;; Author: Francesco Potortì <pot@gnu.org>
;; Keywords: debian, bug, reporter
;; Version: $Id: debian-bug.el 1.4 1999/09/22 15:41:37 pot Exp $

;; debian-bug.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; debian-bug.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;
;;

;;; Todo:

;; - Add doc strings and commentary.
;; - Help on menu of pseudo packages and severity levels (how???)
;; - we assume sed, uname, date are there; is that ok?
;; - Remove the debugging escape

;;; Code:

(defvar debian-bug-mail-address
  "Debian GNU/Linux bug list <submit@bugs.debian.org>")

(defvar debian-bug-program nil)

(defvar debian-bug-status-file "/var/lib/dpkg/status")

(defvar debian-bug-severity-alist
  '(("critical") ("grave") ("important") ("normal") ("wishlist")))

(defvar debian-bug-pseudo-packages
  '("base" "bootdisk" "rootdisk" "bugs.debian.org" "ftp.debian.org"
    "nonus.debian.org" "www.debian.org" "manual" "project" "general"
    "kernel" "lists.debian.org"))

(defvar debian-bug-packages-obarray nil)
(defvar debian-bug-packages-date nil)

(defalias 'report-debian-bug 'debian-bug)

;;; Functions:

(defun debian-bug-intern (name)
  (intern name debian-bug-packages-obarray))

(defun debian-bug-fill-packages-obarray ()
  ""
  (if (and (vectorp debian-bug-packages-obarray)
	   (equal debian-bug-packages-date
		  (nth 5 (file-attributes debian-bug-status-file))))
      nil
    (let (len
	  (packages (length debian-bug-pseudo-packages))
	  (buf (get-buffer-create " debian-bug-packages")))
      (message "Building list of installed packages...")
      (save-excursion
	(set-buffer buf)
	(erase-buffer)
	(if (zerop
	     (call-process "sed" debian-bug-status-file buf nil
			   "-n"
			   "/^Package: / { s///\nh\n}\n/^Version:/ { g\np\n}"))
	    (setq packages (+ packages
			      (count-lines (point-min) (point-max)))))
	(setq len (1- (ash 4 (logb packages))))
	(setq debian-bug-packages-obarray (make-vector len 0))
	(mapcar 'debian-bug-intern debian-bug-pseudo-packages)
	(mapcar 'debian-bug-intern (split-string (buffer-string)))
	(kill-buffer buf)
	(setq debian-bug-packages-date
	      (nth 5 (file-attributes debian-bug-status-file)))
	)))
  (vectorp debian-bug-packages-obarray))

(defun debian-bug-init-program ()
  (or debian-bug-program
      (setq debian-bug-program
	    (cond
	     ((zerop (call-process "bug" nil nil nil "--help"))
	      "bug")
	     ((zerop (call-process "reportbug" nil nil nil "--help"))
	      "reportbug")
	     (t
	      "sed")
	     ))))

(defun debian-bug-prefill-report (package severity)
  (cond

   ;; reportbug
   ((string= debian-bug-program "reportbug")
    (save-excursion
      (call-process debian-bug-program nil '(t t) nil
		    "-p" "-s" "" "-S" severity package))
    (delete-region (point) (progn (forward-line 5) (point)))
    (forward-line 4))

   ;; bug
   ((string= debian-bug-program "bug")
    (save-excursion
      (call-process debian-bug-program nil '(t t) nil
		    "-p" "-s" "" "-S" severity package))
    (forward-line 4))

   ;; neither reportbug nor bug
   ((string= debian-bug-program "sed")
    (insert "Package: " package
	    "\nVersion: ")
    (call-process "sed" debian-bug-status-file '(t t) nil
		  "-n" (concat "/^Package: " package
			       "$/,/^$/s/^Version: //p"))
    (or (bolp)
	(call-process "date" nil '(t t) nil "+N/A; reported %Y-%m-%d"))
    (insert "Severity: " severity
	    "\n\n\n\n-- System Information"
	    "\nDebian Release: ")
    (insert-file-contents-literally "/etc/debian_version")
    (goto-char (point-max))
    (insert "Kernel Version: ")
    (call-process "uname" nil '(t t) nil "-a")
    (forward-line -5))
   ))

(defun debian-bug (debug)
  "Submit via mail a bug report do Debian"

  (interactive "P")
  (let ((reporter-prompt-for-summary-p t)
	package severity)
    (if (and
	 (if (y-or-n-p
	      "Do you want to submit a bug report to the Debian maintainers? ")
	     t (message "") nil)
	 (require 'reporter)
	 (debian-bug-fill-packages-obarray)
	 (debian-bug-init-program)
	 (setq package (completing-read
			"Package name (or file name preceded by -f): "
			debian-bug-packages-obarray
			nil nil nil nil (current-word)))
	 (setq severity (completing-read
			 "Severity (default normal): "
			 debian-bug-severity-alist
			 nil t nil nil "normal"))
	 (reporter-submit-bug-report (if debug "pot" debian-bug-mail-address)
				     package
				     nil nil nil nil))
	(progn
	  (delete-region (point) (point-max))
	  (debian-bug-prefill-report package severity)
	  (set-window-start (selected-window) (point-min) t)
	  ))))
