;;; orig --- backup file on first modification

;;; Copyright (C) 1999 Frederic Lepied

;;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;; Version: $Id: orig.el,v 1.1 1999/11/10 17:14:26 fred Exp $
;;; Keywords: tools

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; Make a first backup copy for some files. When a file is saved, if its name
;; matches a regular expression and no backup copy exists, a backup copy is
;; created with a specific extension. This is very usefull when you work on
;; modifications in large tree of files and you don't want to have two copies
;; of the tree to generate the diff.
;;
;; To install it, put this file in a directory where emacs can
;; load it and then add the following lines to your .emacs file
;; to enable backups on files which the names contain /BUILD/:
;; 
;; (require 'orig)
;;
;; (setq orig-save-regex "/BUILD/")

;;; History:
;;
;; $Log: orig.el,v $
;; Revision 1.1  1999/11/10 17:14:26  fred
;; Initial revision
;;

;;; Code:

(defvar orig-version "$Id: orig.el,v 1.1 1999/11/10 17:14:26 fred Exp $")

(defvar orig-ext ".orig"
  "*Extension to append to file names to make a backup copy.")

(defvar orig-save-regex nil
  "*Regex to test if a backup of a file has to be created with `orig-save'.
If nil no backup is created.")

(defvar orig-use-truename nil
  "*if non nil, use the real name of the file (follow symlink)")

(defun orig-save()
  "Save a file in a backup with `orig-ext' appended if the file name
matches `orig-save-regex' and no backup already exists."
  (let ((real-name (or (and orig-use-truename
			    (file-truename buffer-file-name))
		       buffer-file-name)))
    (if (and orig-save-regex
	     (string-match orig-save-regex real-name))
	(let ((orig (concat real-name orig-ext)))
	  (if (not (file-exists-p orig))
	      (if (file-exists-p real-name)
		  (copy-file real-name orig nil t)
		(make-symbolic-link "/dev/null" orig)
		)))))
  ;; return nil to be used in write-file-hooks
  nil)

(defun orig-change-ext(s)
  "Change the backup extension used by `orig-save'."
  (interactive "sNew backup extension: ")
  (if (not (string-equal s ""))
      (setq orig-ext s)))

(add-hook 'write-file-hooks (function orig-save))

(provide 'orig)

;;; orig.el ends here
