;;; module.el --- 
;;
;; Copyright (C) 1997, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Created: Fri Jun 20 17:13:19 1997
;; Version: $Id$
;; Keywords: 
;;
;; LCD Archive Entry:
;; module|Frederic Lepied|Frederic.Lepied@sugix.frmug.org|
;; package one line description.|
;; $Date$|$Revision$|~/path/module.el.Z|
;; 
;; This file is NOT part of GNU Emacs but the same permissions apply.
;;
;; GNU Emacs  is free software;  you can redistribute it and/or modify
;; it under the terms of  the GNU General  Public License as published
;; by  the Free Software  Foundation;  either version  2, or (at  your
;; option) any later version.
;;
;; GNU  Emacs is distributed  in the hope that  it will be useful, but
;; WITHOUT    ANY  WARRANTY;  without even the     implied warranty of
;; MERCHANTABILITY or FITNESS FOR A  PARTICULAR PURPOSE.  See the  GNU
;; General Public License for more details.
;;
;; You should have received  a copy of  the GNU General Public License
;; along with GNU Emacs; see  the file COPYING.  If  not, write to the
;; Free Software Foundation, 675  Mass Ave, Cambridge, MA 02139,  USA.
;; This  program  is free  software;  you  can  redistribute it and/or
;; modify it  under  the terms of the  GNU  General Public License  as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;;; Commentary:

;;; Code:


;;;###autoload
(defvar module-alist nil
  "alist associating directories regex and hooks")

(defun module-find-file-hook ()
  (if buffer-file-name
      (module-process-alist buffer-file-name module-alist)))

(defun module-process-alist (filename alist)
  (if (not (null alist))
      (progn
	(if (string-match (caar alist) filename)
	    (module-process-variables (cdar alist))
	    )
	(module-process-alist filename (cdr alist))
	)))

(defun module-process-variables (variables)
  (if (not (null variables))
      (let ((pair (car variables)))
	(cond ((consp pair)
	       (make-local-variable (car pair))
	       (set (car pair) (cdr pair)) )
	      (t (pair)) )
	(module-process-variables (cdr variables)) ) ) )

;;;###autoload
(defun module-add-entry (regex &rest variables)
  (let ((pair (assoc regex module-alist)))
    (if pair
	(setcdr pair variables)
      (setq module-alist (cons (cons regex variables)
			       module-alist)) ) )
  module-alist)

(add-hook 'find-file-hooks 'module-find-file-hook t)

;;; module.el ends here
