;; 40scheme-prog.el --- 
;;
;; Copyright (C) 1996, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: scheme-prog.el,v 1.1 1999-12-09 07:34:42 flepied Exp $
;; Keywords: 
;;

(cfg-require 'prog)

;;=============================================================================
;; Autoloaded section.
;;=============================================================================

;;;***


;;=============================================================================
;; Configuration section.
;;=============================================================================
(add-hook 'inferior-scheme-mode-hook '(lambda () (define-key inferior-scheme-mode-map [f2] 'run-scheme)))
(add-hook 'scheme-mode-hook '(lambda () (define-key scheme-mode-map [f2] 'run-scheme)))

(defvar scheme-word-help-entry
  '("Scheme"
    (("r4rs" "Index")
     ("scm" "Procedure and Macro Index" "Variable Index" "Type Index")
     ("slib" "Procedure and Macro Index" "Variable Index"))
    (("[^][ ()\n\t.\"'#]+")) ) )

(eval-after-load "word-help" '(setq word-help-mode-alist (cons scheme-word-help-entry word-help-mode-alist)))

;; electric newline and delete
(defun scheme-indent-and-newline-and-indent()
  (interactive)
  (apply indent-line-function ())
  (newline)
  (apply indent-line-function ())
  )

(defvar scheme-delete-function 'backward-delete-char-untabify
  "*Function called by `scheme-electric-delete' when deleting chars")

(defun scheme-in-literal (&optional lim)
  ;; Determine if point is in a C++ literal
  (save-excursion
    (let* ((lim (or lim (save-excursion
			  (beginning-of-defun)
			  (point))))
	   (here (point))
	   (state (parse-partial-sexp lim (point))))
      (cond
       ((nth 3 state) 'string)
       ((nth 4 state) 'comment)
       (t nil)))))

(defun scheme-electric-delete (arg)
  (interactive "P")
  (if (or arg
	  (scheme-in-literal))
      (funcall scheme-delete-function (prefix-numeric-value arg))
    (let ((here (point)))
      (skip-chars-backward " \t\n")
      (if (/= (point) here)
	  (delete-region (point) here)
	(funcall scheme-delete-function (prefix-numeric-value arg))
	))))

(add-hook 'scheme-mode-hook (function (lambda()
					(define-key scheme-mode-map "\C-m"
					  'scheme-indent-and-newline-and-indent )
					(define-key scheme-mode-map "\177" 'scheme-electric-delete)
					)))

;; fontlock addons

(if window-system
    (progn
(defvar font-lock-setter-face 'font-lock-setter-face "")
(defvar font-lock-test-face 'font-lock-test-face "")
(setq scheme-font-lock-keywords
      (list
     ;;
     ;; Declarations.  Hannes Haug <hannes.haug@student.uni-tuebingen.de> says
     ;; this works for SOS, STklos, SCOOPS, Meroon and Tiny CLOS.
     (list (concat "(\\(define\\("
		   ;; Function names.
		   "\\(\\|-\\(generic\\(\\|-procedure\\)\\|method\\)\\)\\|"
		   ;; Macro names, as variable names.  A bit dubious, this.
		   "\\(-syntax\\)\\|"
		   ;; Class names.
		   "\\(-class\\)"
		   "\\)\\)\\>"
		   ;; Any whitespace and declared object.
		   "\\s *(?"
		   "\\(\\sw+\\)?")
	   '(1 font-lock-keyword-face)
	   '(8 (cond ((match-beginning 3) font-lock-function-name-face)
		     ((match-beginning 6) font-lock-variable-name-face)
		     (t font-lock-type-face))
	       nil t))
     ;;
     ;; Control structures.
     (cons
      (concat "\\<\\("
;; The result of the following expression is inserted just after.
;; 	      (make-regexp '("begin" "call-with-current-continuation" "call/cc"
;; 			     "call-with-input-file" "call-with-output-file" "case" "cond"
;; 			     "do" "else" "for-each" "if" "lambda"
;; 			     "let\\*?" "let-syntax" "letrec" "letrec-syntax"
;; 			     ;; Hannes Haug <hannes.haug@student.uni-tuebingen.de> wants:
;; 			     "and" "or" "delay"
;; 			     ;; Stefan Monnier <stefan.monnier@epfl.ch> says don't bother:
;; 			     "quasiquote" "quote" "unquote" "unquote-splicing"
;; 			     "map" "syntax" "syntax-rules"
;; 			     ;; personal taste
;; 			     "not" "slot-ref" "#t" "#f" "cons" "list-ref" "car" "cdr" "display"
;; 			     "write"
;; 			     ))
	      "#[ft]\\|and\\|begin\\|c\\(a\\(ll\\(-with-\\(current-continuation\\|input-file\\|output-file\\)\\|/cc\\)\\|r\\|se\\)\\|dr\\|on[ds]\\)\\|d\\(elay\\|isplay\\|o\\)\\|else\\|for-each\\|if\\|l\\(ambda\\|et\\(-syntax\\|\\*?\\|rec\\(\\|-syntax\\)\\)\\|ist-ref\\)\\|map\\|not\\|or\\|qu\\(asiquote\\|ote\\)\\|s\\(lot-ref\\|yntax\\(\\|-rules\\)\\)\\|unquote\\(\\|-splicing\\)\\|write"
	      "\\)\\>")
      'font-lock-keyword-face)
     ;;
     ;; David Fox <fox@graphics.cs.nyu.edu> for SOS/STklos class specifiers.
     '("\\<<\\sw+>\\>" . font-lock-type-face)
     ;;
     ;; Scheme `:' keywords as references.
     '("\\<:\\sw+\\>" . font-lock-reference-face)

     ;; setters
     '("\\<\\sw+[!]\\>" . font-lock-setter-face)

     ;; test
     '("\\<\\sw+[?]\\>" . font-lock-test-face)

     ;; require
      '("(\\(require\\)\\s-+'\\s-*\\(\\_+\\)\\>" (1 font-lock-reference-face) (2 font-lock-string-face))
     ))))

;; imenu addons

(add-hook 'scheme-mode-hook
 	  (function
 	   (lambda ()
	     (setq imenu-generic-expression '((nil "^(def[^ \t\n]+\\s *(?\\s *\\([^) \t\n]+\\)" 1)))
	     )))

;; dabbrev addons

(add-hook 'inferior-scheme-mode-hook
	  (function (lambda()
		      (set (make-local-variable 'dabbrev-friend-buffer-function)
			   (lambda (buffer)
			     (save-excursion
			       (set-buffer buffer)
			       (memq major-mode '(inferior-scheme-mode scheme-mode))))))))

;;; 40scheme-prog.el ends here
