;;; m4-mode.el --- m4 code editing commands for Emacs

;; Author: Andrew Csillag (drew@staff.prodigy.com)
;; Maintainer: Andrew Csillag (drew@staff.prodigy.com)
;; Keywords: languages, faces

;; This file is (not yet) part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; A smart editing mode for m4 macro definitions.  It seems to have most of the 
;; syntax right (sexp motion commands work, but function motion commands don't).
;; It also sets the font-lock syntax stuff for colorization

;; By Drew Csillag (drew@staff.prodigy.com)
;; $Id: m4-mode.el,v 1.6 1996/10/24 19:12:19 drew Exp $

;; History:

;; Date     Version Who Action
;; -------- ------- --- ----
;; 09/13/96 1.0	    DC  Created
;; 09/26/96 1.1	    DC  added syntax table stuff so now matches `'s
;;          		now can use C-c C-b to m4 on buffer,C-c C-r for region
;;                      added m4-comment-region (C-c C-c)
;; 09/27/96 1.2	    DC  Redid header comment
;;          1.3     DC  more cosmetic fixes 
;;          1.4     DC  removed m4-comment-region, using comment-region instead
;; 10/03/96         DC  added provide line
;; 10/24/96 1.5     DC  fixed keyword regexp

;; To Do's:

;; * want to make m4-m4-(buffer|region) look sorta like M-x compile look&feel ?
;; * sexp motion commands don't seem to work right

;; to autoload m4 lisp code:  
;; (autoload 'm4-mode "m4-mode" nil t)
;;
;; or can use (load "m4-mode") or (require 'm4-mode) to just load it 
;;
;; to try to "auto-detect" m4 files:
;; (setq auto-mode-alist 
;;	 (cons '(".*\\\.m4" . m4-mode)
;;	       auto-mode-alist))

;;; Code:

;;path to the m4 program
(defvar m4-program "/usr/local/bin/m4")

;;thank god for make-regexp.el!
(defvar m4-font-lock-keywords
  `(
    ("^\\\#.*" . font-lock-comment-face)
    ("\\\$\\\*" . font-lock-variable-name-face)
    ("\\\$[0-9]" . font-lock-variable-name-face)
    ("\\\$\\\#" . font-lock-variable-name-face)
    ("\\b\\(builtin\\|change\\(com\\|quote\\|word\\)\\|d\\(e\\(bug\\(file\\|mode\\)\\|cr\\|f\\(ine\\|n\\)\\)\\|iv\\(ert\\|num\\)\\|nl\\|umpdef\\)\\|e\\(rrprint\\|syscmd\\|val\\)\\|f\\(ile\\|ormat\\)\\|gnu\\|i\\(f\\(def\\|else\\)\\|n\\(c\\(lude\\|r\\)\\|d\\(ex\\|ir\\)\\)\\)\\|l\\(en\\|ine\\)\\|m\\(4\\(exit\\|wrap\\)\\|aketemp\\)\\|p\\(atsubst\\|opdef\\|ushdef\\)\\|regxep\\|s\\(hift\\|include\\|ubstr\\|ys\\(cmd\\|val\\)\\)\\|tra\\(ceo\\(ff\\|n\\)\\|nslit\\)\\|un\\(d\\(efine\\|ivert\\)\\|ix\\)\\)\\b" . font-lock-keyword-face) 
      "default font-lock-keywords")
)

;;this may still need some work
(defvar m4-mode-syntax-table nil
  "syntax table used in m4 mode")
(setq m4-mode-syntax-table (make-syntax-table))
(modify-syntax-entry ?` "('" m4-mode-syntax-table)
(modify-syntax-entry ?' ")`" m4-mode-syntax-table)
(modify-syntax-entry ?# "<\n" m4-mode-syntax-table)
(modify-syntax-entry ?\n ">#" m4-mode-syntax-table)
(modify-syntax-entry ?{  "_" m4-mode-syntax-table)
(modify-syntax-entry ?}  "_" m4-mode-syntax-table)
(modify-syntax-entry ?*  "w" m4-mode-syntax-table)

(defvar m4-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-b" 'm4-m4-buffer)
    (define-key map "\C-c\C-r" 'm4-m4-region)
    (define-key map "\C-c\C-c" 'comment-region)
    map))

(defun m4-m4-buffer ()
  "send contents of the current buffer to m4"
  (interactive)
  (start-process "m4process" "*m4 output*" m4-program "-e")
  (process-send-region "m4process" (point-min) (point-max))
  (process-send-eof "m4process")
  (switch-to-buffer "*m4 output*")
)

(defun m4-m4-region ()
  "send contents of the current region to m4"
  (interactive)
  (start-process "m4process" "*m4 output*" m4-program "-e")
  (process-send-region "m4process" (point) (mark))
  (process-send-eof "m4process")
  (switch-to-buffer "*m4 output*")
)

(defun m4-mode ()
  "A major-mode to edit m4 macro files
\\{m4-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map m4-mode-map)

  (make-local-variable 'comment-start)
  (setq comment-start "#")
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)


  (make-local-variable	'font-lock-defaults)  
  (setq major-mode 'm4-mode
	mode-name "m4"
	font-lock-defaults `(m4-font-lock-keywords nil)
	)
  (set-syntax-table m4-mode-syntax-table)
  (run-hooks 'm4-mode-hook))

(provide 'm4-mode)
;;stuff to play with for debugging
;(char-to-string (char-syntax ?`))
