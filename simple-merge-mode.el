;;; simple-merge-mode.el --- Major mode to help resolve conflicts

;; Copyright (C) 1999  Peter Österlund
;; Copyright (C) 1999  Stefan Monnier

;; Author: Peter Österlund <peter.osterlund@mailbox.swipnet.se>
;; Author: Stefan Monnier <monnier@cs.yale.edu>
;; Keywords: merge diff3 cvs conflict

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
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
;; Major mode to help resolving conflicts in output generated
;; by the merge(1) or diff3(1) program.
;;
;; just add the following lines to your .emacs file:
;;
;;   (autoload 'simple-merge-mode "simple-merge-mode" nil t)
;;   (autoload 'simple-merge-minor-mode "simple-merge-mode" nil t)
;;
;; There are two modes of operation, "edit mode" and "fast mode".
;; Fast mode is the default mode.
;;
;; The default key bindings in "fast mode" are as follows:
;;
;;    e   Switch to edit mode
;;    n   Jump to next conflict mark
;;    p   Jump to previous conflict mark
;;    b   Keep 'base' version in current conflict region
;;    t   Keep 'theirs' version in current conflict region
;;    y   Keep 'yours' version in current conflict region
;;    m   Keep 'yours' version in current conflict region
;;    k   Keep current version (the version at the current cursor location)
;;        in current conflict region
;;    u   Undo previous command
;;    <   Diff 'base' and 'yours' version of current conflict region
;;    >   Diff 'base' and 'theirs' version of current conflict region
;;    =   Diff 'yours' and 'theirs' version of current conflict region
;;    l   Mark conflict regions
;;
;; In "edit" mode, commands have to be prefixed by
;; `simple-merge-command-prefix' (C-c by default).
;;

;; there is also a minor-mode alternative (corresponds to the edit mode).
;; you can even have it turned on automatically with the following
;; piece of code in your .emacs:
;;
;;   (defun sm-try-simple-merge ()
;;     (save-excursion
;;   	 (goto-char (point-min))
;;   	 (when (re-search-forward "^<<<<<<< " nil t)
;;   	   (simple-merge-minor-mode 1))))
;;   (add-hook 'find-file-hooks 'sm-try-simple-merge)

;;; History:


;;; Code:

(eval-when-compile (require 'cl))

(defvar simple-merge-version "1.5" "The simple-merge version number.")

(defgroup simple-merge ()
  "Major mode to help resolving conflicts."
  :group 'tools
  :prefix "simple-merge-")

(defcustom simple-merge-diff-buffer-name "*simple-merge-diff*"
  "Buffer name to use for diplaying diffs."
  :group 'simple-merge
  :type 'string)

(defcustom simple-merge-command-prefix "\C-c"
  "Command prefix for merge commands in edit mode."
  :group 'simple-merge
  :type 'string)

(defface simple-merge-yours-face
  '((t (:foreground "blue")))
  "Face for your code."
  :group 'simple-merge)
(defvar simple-merge-yours-face 'simple-merge-yours-face)

(defface simple-merge-theirs-face
  '((t (:foreground "darkgreen")))
  "Face for their code."
  :group 'simple-merge)
(defvar simple-merge-theirs-face 'simple-merge-theirs-face)

(defface simple-merge-base-face
  '((t (:foreground "red")))
  "Face for the base code."
  :group 'simple-merge)
(defvar simple-merge-base-face 'simple-merge-base-face)

(defface simple-merge-markers-face
  '((t))
  "Face for the conflict markers."
  :group 'simple-merge)
(defvar simple-merge-markers-face 'simple-merge-markers-face)

(defvar simple-merge-conflict-begin-mark "^<<<<<<< .*\n"
  "String used to mark the start of a conflict region.")

(defvar simple-merge-conflict-end-mark "^>>>>>>> .*\n"
  "String used to mark the end of a conflict region.")

(defvar simple-merge-conflict-base-mark "^||||||| .*\n"
  "String used to mark the beginning of the base version in a conflict region.")

(defvar simple-merge-conflict-their-mark "^=======\n"
  "String used to mark the beginning of their version in a conflict region.")


(defmacro simple-merge-defmap (var bindings &optional doc)
  `(defvar ,var
     (let ((m (make-sparse-keymap)))
       (dolist (b ,bindings)
	 (define-key m (car b) (cdr b)))
       m)
     ,doc))

(simple-merge-defmap simple-merge-basic-keymap
  '(("e" . simple-merge-set-edit-keymap)
    ("f" . simple-merge-set-fast-keymap)
    ("n" . simple-merge-jump-next-conflict)
    ("p" . simple-merge-jump-prev-conflict)
    ("b" . simple-merge-keep-base-version)
    ("t" . simple-merge-keep-theirs-version)
    ("y" . simple-merge-keep-yours-version)
    ("m" . simple-merge-keep-yours-version)
    ("k" . simple-merge-keep-current-version)
    ("u" . undo)
    ("<" . simple-merge-diff-base-yours)
    (">" . simple-merge-diff-base-theirs)
    ("=" . simple-merge-diff-yours-theirs)
    ("l" . simple-merge-mark-conflicts))
  "The base keymap for `simple-merge-mode'.")
(fset 'simple-merge-basic-keymap simple-merge-basic-keymap)

(simple-merge-defmap simple-merge-edit-keymap
  `((,simple-merge-command-prefix . simple-merge-basic-keymap))
  "Keymap of `simple-merge-mode' commands.
In `edit' mode, commands must be prefixed by \
\\<simple-merge-fast-keymap>\\[simple-merge-basic-keymap].")

(simple-merge-defmap simple-merge-fast-keymap
  `((,simple-merge-command-prefix . simple-merge-basic-keymap))
  "Local keymap used in simple-merge `fast' mode.
Makes `simple-merge-mode' commands directly available.")
(set-keymap-parent simple-merge-fast-keymap simple-merge-basic-keymap)

(defvar simple-merge-minor-mode-map simple-merge-edit-keymap
  "Keymap for `simple-merge-minor-mode'.")

(defconst simple-merge-font-lock-keywords
  '((simple-merge-find-conflict
     (1 simple-merge-yours-face prepend)
     (2 simple-merge-base-face prepend)
     (3 simple-merge-theirs-face prepend)
     (0 simple-merge-markers-face keep)))
  "Font lock patterns for `simple-merge-minor-mode'.")

;;;;
;;;; Actual code
;;;;

;;;###autoload
(easy-mmode-define-minor-mode
 simple-merge-minor-mode
 "Minor mode to simplify editing output from the diff3 program.
A minor mode variant of `simple-merge-mode'.
\\{simple-merge-minor-mode-map}"
 nil " SMerge")
;;(add-hook 'simple-merge-minor-mode-on-hook 'simple-merge-mark-conflicts)

(defun simple-merge-refontify ()
  (when font-lock-mode
    (save-excursion
      (goto-char (point-min))
      (while (simple-merge-find-conflict)
	(font-lock-fontify-region (match-beginning 0) (match-end 0) nil)))))

(defun simple-merge-minor-mode-on ()
  (font-lock-add-keywords nil simple-merge-font-lock-keywords 'append)
  (simple-merge-refontify))
(add-hook 'simple-merge-minor-mode-on-hook 'simple-merge-minor-mode-on)

(defun simple-merge-minor-mode-off ()
  ;; Here delete "should" be safe because the keywords were appended,
  ;; forcing a copy of the list.
  (dolist (k simple-merge-font-lock-keywords)
    (setq font-lock-keywords (delete k font-lock-keywords)))
  (simple-merge-refontify))
(add-hook 'simple-merge-minor-mode-off-hook 'simple-merge-minor-mode-off)


;;;###autoload
(defun simple-merge-mode ()
  "Major mode to simplify editing output from the diff3 program.

`simple-merge-mode' makes it easier to edit files containing
\"conflict marks\", generated for example by diff3 and CVS.

There are two modes of operation, \"edit mode\" and \"fast mode\".
Fast mode is the default mode.

Special key bindings in \"fast mode\":
\\{simple-merge-basic-keymap}

In `edit' mode, commands must be prefixed by \\[simple-merge-basic-keymap]."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'simple-merge-mode)
  (setq mode-name "SimpleMerge")

  (simple-merge-set-fast-keymap)
  (simple-merge-mark-conflicts)
  (run-hooks 'simple-merge-mode-hook))


(defun simple-merge-set-fast-keymap ()
  "Set simple-merge fast mode."
  (interactive)
  (use-local-map simple-merge-fast-keymap)
  (setq mode-line-process " Fast")
  (force-mode-line-update))

(defun simple-merge-set-edit-keymap ()
  "Set simple-merge edit mode."
  (interactive)
  (use-local-map simple-merge-edit-keymap)
  (setq mode-line-process " Edit")
  (force-mode-line-update))


(defun simple-merge-jump-next-conflict (&optional count)
  "Move cursor to the beginning of the next COUNT'th conflict region."
  (interactive)
  (unless count (setq count 1))
  (if (< count 0) (simple-merge-jump-prev-conflict (- count))
    (if (looking-at simple-merge-conflict-begin-mark) (incf count))
    (unless (re-search-forward simple-merge-conflict-begin-mark nil t count)
      (error "No more conflicts"))
    (goto-char (match-beginning 0))))

(defun simple-merge-jump-prev-conflict (&optional count)
  "Move cursor to the beginning of the previous COUNT'th conflict region."
  (interactive)
  (unless count (setq count 1))
  (if (< count 0) (simple-merge-jump-next-conflict (- count))
    (unless (re-search-backward simple-merge-conflict-begin-mark nil t count)
      (error "No previous conflict"))))


(defsubst simple-merge-keep-base-version ()
  "Keep 'base' version in current conflict region."
  (interactive)
  (simple-merge-match-conflict)
  (replace-match (match-string 2) t t))

(defsubst simple-merge-keep-theirs-version ()
  "Keep 'theirs' version in current conflict region."
  (interactive)
  (simple-merge-match-conflict)
  (replace-match (match-string 3) t t))

(defsubst simple-merge-keep-yours-version ()
  "Keep 'yours' version in current conflict region."
  (interactive)
  (simple-merge-match-conflict)
  (replace-match (match-string 1) t t))

(defun simple-merge-keep-current-version ()
  "Keep 'current' (under the cursor) version in current conflict region."
  (interactive)
  (simple-merge-match-conflict)
  (let ((i 3))
    (while (or (< (point) (match-beginning i))
	       (>= (point) (match-end i)))
      (decf i))
    (if (> i 0)
	(replace-match (match-string i) t t)
      (error "Not inside a version"))))

(defsubst simple-merge-diff-base-yours ()
  "Diff 'base' and 'yours' version in current conflict region."
  (interactive)
  (simple-merge-match-conflict)
  (simple-merge-create-diff (match-string 2) (match-string 1) "base" "yours"))

(defun simple-merge-diff-base-theirs ()
  "Diff 'base' and 'theirs' version in current conflict region."
  (interactive)
  (simple-merge-match-conflict)
  (simple-merge-create-diff (match-string 2) (match-string 3) "base" "theirs"))

(defun simple-merge-diff-yours-theirs ()
  "Diff 'yours' and 'theirs' version in current conflict region."
  (interactive)
  (simple-merge-match-conflict)
  (simple-merge-create-diff (match-string 1) (match-string 3) "yours" "theirs"))


(defun simple-merge-match-conflict ()
  "Get info about the conflict.  Puts the info in the `match-data'.
The submatches contain:
 0:  the whole conflict.
 1:  your code.
 2:  the base code.
 3:  their code.
An error is raised if not inside a conflict."
  (save-excursion
    (let ((orig-point (point)))
      (forward-line 1)
      (or
       (when (re-search-backward simple-merge-conflict-begin-mark nil t)
	 (let ((start (match-beginning 0))
	       (yours-start (match-end 0)))
	   (when (and (re-search-forward simple-merge-conflict-end-mark nil t)
		      (< orig-point (match-end 0)))
	     (let ((yours-end (match-beginning 0))
		   (base-start (match-beginning 0))
		   (base-end (match-beginning 0))
		   (theirs-start (match-beginning 0))
		   (theirs-end (match-beginning 0))
		   (end (match-end 0)))

	       (goto-char start)
	       (when (re-search-forward simple-merge-conflict-their-mark end t)
		 (setq yours-end (match-beginning 0))
		 (setq base-end (match-beginning 0))
		 (setq theirs-start (match-end 0)))

	       (goto-char start)
	       ;; BEWARE order: yours-end is assigned in both cases but
	       ;; this case should take precedence over the previous one
	       (when (re-search-forward simple-merge-conflict-base-mark end t)
		 (setq yours-end (match-beginning 0))
		 (setq base-start (match-end 0)))

	       ;;(setq yours-start (min yours-start yours-end))
	       (setq base-start (min base-start base-end))
	       ;;(setq theirs-start (min theirs-start theirs-end))

	       (store-match-data (list start end
				       yours-start yours-end
				       base-start base-end
				       theirs-start theirs-end))
	       t))))
       (error "Point not in conflict region")))))

(defun simple-merge-find-conflict (&optional limit)
  "Find and match a conflict region.  Intended as a font-lock MATCHER.
The submatches are the same as in `simple-merge-match-conflict'.
Returns non-nil if a match is found between the point and LIMIT.
The point is moved to the end of the conflict."
  (when (re-search-forward simple-merge-conflict-begin-mark limit t)
    (condition-case ()
	(progn (simple-merge-match-conflict)
	       (goto-char (match-end 0)))
      (error nil))))

(defun simple-merge-mark-conflicts ()
  "Mark conflict regions with different foreground colors."
  (interactive)
  (let ((modified (buffer-modified-p))
	(conflicts 0))
    (save-excursion
      (goto-char (point-min))
      (while (simple-merge-find-conflict)
	(incf conflicts)
	(put-text-property (match-beginning 0) (match-end 0)
			   'face 'simple-merge-markers-face)
	(put-text-property (match-beginning 1) (match-end 1)
			   'face 'simple-merge-yours-face)
	(put-text-property (match-beginning 2) (match-end 2)
			   'face 'simple-merge-base-face)
	(put-text-property (match-beginning 3) (match-end 3)
			   'face 'simple-merge-theirs-face)))
    (set-buffer-modified-p modified)
    (message "%d conflicts found" conflicts)))

;; Function to show the current version.
(defun simple-merge-version ()
  "Show the current simple-merge version."
  (interactive)
  (message "Simple merge version %s" simple-merge-version))

;; Emacs predefines this variable, XEmacs doesn't.
(defvar temporary-file-directory "/tmp/")

(defun simple-merge-create-diff (string1 string2 name1 name2)
  (let ((file1 (make-temp-name
		(expand-file-name "smerge1"
				  temporary-file-directory)))
	(file2 (make-temp-name
		(expand-file-name "smerge2"
				  temporary-file-directory))))
    (write-region string1 0 file1)
    (write-region string2 0 file2)
    (with-current-buffer (get-buffer-create simple-merge-diff-buffer-name)
      (setq buffer-read-only nil)
      (erase-buffer)
      (apply 'call-process "diff" nil t nil
	     (list "-du" "-L" name1 "-L" name2 file1 file2))
      (goto-char (point-min))
      (delete-file file1)
      (delete-file file2)
      (setq buffer-read-only t)
      (if (fboundp 'diff-mode) (diff-mode))
      (display-buffer (current-buffer) t))))


(provide 'simple-merge-mode)
;;; simple-merge-mode.el ends here
