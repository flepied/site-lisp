;---------------------------------------------------------------
;; Project         : Imagine
;; File            : vrac.el
;; Type		   : -*- emacs-lisp -*-
;; Version         : $Imagine$
;; Author          : Frederic Lepied
;; Created On      : Thu May 15 17:49:37 1997
;; Purpose         : 
;;---------------------------------------------------------------

;;---------------- Distribution and Copyright -------------------
;;
;; This software is copyright by the CENA/DGAC/FRANCE 
;;
;; All rights reserved.
;;
;; No part of the material protected by this copyright notice
;; may be reproduced or utilized for commercial use in any form
;; without written permission of the copyright owner.
;;
;; It may be reproduced or utilized for R&D use in Non Profit
;; Organization
;;
;;---------------------------------------------------------------

;;---------------- Disclaimer -----------------------------------
;;
;; This software and its documentation are provided "AS IS" and
;; without any expressed or implied warranties whatsoever.
;; No warranties as to performance, merchantability, or fitness
;; for a particular purpose exist.
;;
;; Because of the diversity of conditions and hardware under
;; which this software may be used, no warranty of fitness for
;; a particular purpose is offered.  The user is advised to
;; test the software thoroughly before relying on it.  The user
;; must assume the entire risk and liability of using this
;; software.
;;
;; In no event shall any person or organization of people be
;; held responsible for any direct, indirect, consequential
;; or inconsequential damages or lost profits.
;;                                                           
;;-----------------END-PROLOGUE----------------------------------

;;;
;;; Besoins de ce module.
;;;

;; from dewey@newvision.com (Dewey M. Sasser)

;;;###autoload
(defun apply-command-to-region (command)
  "Execute a command with the buffer narrowed to the current region"
  (interactive "CWhat Command? ")
  (save-restriction
    (narrow-to-region (region-beginning) (region-end))
    (goto-char (point-min))
    (call-interactively command)))

;;=============================================================================
;; narrow to a region for further editing.
;;=============================================================================
;;;###autoload
(defun recursive-edit-in-region (beg end)
  "Enter recursive edit with buffer narrowed to region.
Upon exit from recursive edit, restore restrictions, point and mark."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char beg)
      (recursive-edit))))

;;=============================================================================
;; load the content of a file into a string.
;;=============================================================================
;;;###autoload
(defun file->string (name &optional func)
  "Convert a file into a string"
  (interactive "fFile name : ")
  (let ((filename (expand-file-name name)))
    (if (not (file-readable-p filename))
	nil
      (let ((buf (create-file-buffer filename))
	    ret)
	(save-excursion
	  (set-buffer buf)
	  (insert-file-contents filename)
	  (if func
	      (funcall func))
	  (setq ret (buffer-string)))
	(kill-buffer buf)
	ret))))

;;=============================================================================
;;=============================================================================
;;;###autoload
(defun errors-on-region(beg end)
  "Create a new window to find compile errors in the restricted region"
  (interactive "r")
  (require 'compile)
  (let ((buf (make-indirect-buffer
	      (current-buffer)
	      (funcall (or compilation-buffer-name-function
			   (function (lambda (mode)
				       (concat "*" (downcase mode) "*"))))
		       "Compilation") )))
    (pop-to-buffer buf)
    (widen)
    (narrow-to-region beg end)
    (compilation-minor-mode)))

;;; end of vrac.el
