;; 40c-prog.el --- 
;;
;; Copyright (C) 1996, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: c-prog.el,v 1.1 1999-12-09 07:34:42 flepied Exp $
;; Keywords: 
;;

(cfg-require 'prog)

;;=============================================================================
;; Autoloaded section.
;;=============================================================================
(autoload 'completion-mode "completion"
  "Toggles whether or not to add new words to the completion database."
  t nil)


;;;###

;;;***

;;;### (autoloads (global-cwarn-mode turn-on-cwarn-mode cwarn-mode) "cwarn" "../cwarn.el" (14227 12969))
;;; Generated autoloads from ../cwarn.el

(autoload (quote cwarn-mode) "cwarn" "\
Hightlight suspicious C and C++ constructions.
This is a minor mode that can't be turned off.

Note, in addition to enabling this minor mode, the major mode must
be included in the variable `cwarn-configuration'.  By default C and
C++ are included." t nil)

(autoload (quote turn-on-cwarn-mode) "cwarn" "\
Turn on CWarn mode.

This function is designed to be added to hooks, for example:
  (add-hook 'c-mode-hook 'turn-on-cwarn-mode)" nil nil)

(autoload (quote global-cwarn-mode) "cwarn" "\
Hightlight suspicious C and C++ constructions in all buffers.

With arg, turn CWarn mode on globally if and only if arg is positive." t nil)

;;;***


;;=============================================================================
;; Fabrication de tables d'index de fonctions C et C++ avec imenu.
;;=============================================================================
(defvar imenu-generic-c++-expression
  (` 
   ((nil
     (, 
      (concat
       "^"				  ; beginning of line is required
       "\\(template[ \t]*<[^>]+>[ \t]*\\)?" ; there may be a "template <...>"
       "\\([a-zA-Z0-9_:]+[ \t]+\\)?"	  ; type specs; there can be no
       "\\([a-zA-Z0-9_:]+[ \t]+\\)?"	  ; more than 3 tokens, right?
       
       "\\("				  ; last type spec including */&
       "[a-zA-Z0-9_:]+"
       "\\([ \t]*[*&]+[ \t]*\\|[ \t]+\\)"	  ; either pointer/ref sign or whitespace
       "\\)?"				  ; if there is a last type spec
       "\\("			      ; name; take that into the imenu entry
       "[a-zA-Z0-9_:~]+"		      ; member function, ctor or dtor...
					; (may not contain * because then 
					; "a::operator char*" would become "char*"!)
       "\\|"
       "\\([a-zA-Z0-9_:~]*::\\)?operator"
       "[^a-zA-Z1-9_][^(]*"	      ; ...or operator
       " \\)"
       "[ \t]*([^)]*)[ \t\n]*[^	      ;]" ; require something other than a ; after
					; the (...) to avoid prototypes. Can't
					; catch cases with () inside the parentheses
					; surrounding the parameters
					; (like "int foo(int a=bar()) {...}"
       
       )) 6)    
    ("Class" 
     (, (concat 
	 "^"				   ; beginning of line is required
	 "\\(template[ \t]*<[^>]+>[ \t]*\\)?" ; there may be a "template <...>"
	 "class[ \t]+"
	 "\\([a-zA-Z0-9_]+\\)"                ; this is the string we want to get
	 "[ \t]*[:{]"
	 )) 2)
;; Example of generic expression for finding prototypes, structs, unions, enums.
;; Uncomment if You want to find these too. It will be at bit slower gathering
;; the indexes.
   ("Prototypes"
    (, 
     (concat
      "^"				  ; beginning of line is required
      "\\(template[ \t]*<[^>]+>[ \t]*\\)?" ; there may be a "template <...>"
      "\\([a-zA-Z0-9_:]+[ \t]+\\)?"	  ; type specs; there can be no
      "\\([a-zA-Z0-9_:]+[ \t]+\\)?"	  ; more than 3 tokens, right?
       
      "\\("				  ; last type spec including */&
      "[a-zA-Z0-9_:]+"
      "\\([ \t]*[*&]+[ \t]*\\|[ \t]+\\)"	  ; either pointer/ref sign or whitespace
      "\\)?"				  ; if there is a last type spec
      "\\("			      ; name; take that into the imenu entry
      "[a-zA-Z0-9_:~]+"		      ; member function, ctor or dtor...
					; (may not contain * because then 
					; "a::operator char*" would become "char*"!)
      "\\|"
      "\\([a-zA-Z0-9_:~]*::\\)?operator"
      "[^a-zA-Z1-9_][^(]*"	      ; ...or operator
      " \\)"
      "[ \t]*([^)]*)[ \t\n]*;" 	; require ';' after
					; the (...) Can't
					; catch cases with () inside the parentheses
					; surrounding the parameters
					; (like "int foo(int a=bar());"       
      )) 6)    
   ("Struct"
    (, (concat
	 "^"				; beginning of line is required
	 "\\(static[ \t]+\\)?"		; there may be static or const.
	 "\\(const[ \t]+\\)?"
	 "struct[ \t]+"
	 "\\([a-zA-Z0-9_]+\\)"		; this is the string we want to get
	 "[ \t]*[{]"
	 )) 3)
   ("Enum"
    (, (concat
	 "^"				; beginning of line is required
	 "\\(static[ \t]+\\)?"		; there may be static or const.
	 "\\(const[ \t]+\\)?"
	 "enum[ \t]+"
	 "\\([a-zA-Z0-9_]+\\)"		; this is the string we want to get
	 "[ \t]*[{]"
	 )) 3)
   ("Union"
    (, (concat
	 "^"				; beginning of line is required
	 "\\(static[ \t]+\\)?"		; there may be static or const.
	 "\\(const[ \t]+\\)?"
	 "union[ \t]+"
	 "\\([a-zA-Z0-9_]+\\)"		; this is the string we want to get
	 "[ \t]*[{]"
	 )) 3)
    ))
  "imenu generic expression for C++ mode in the form
(PATTERN), where PATTERN is a list containing entries of the form 
(MENU-TITLE REGEXP INDEX). See `imenu-generic-expression'.")

(add-hook 'c-mode-hook (function (lambda()
				   (setq imenu-prev-index-position-function nil
					 imenu-generic-expression imenu-generic-c++-expression)
;				   (imenu-add-to-menubar "Index")
				   )))

;;=============================================================================
;; Configuration section.
;;=============================================================================

;;=============================================================================
;; Warnings sur les constructions C et C++ douteuses.
;;=============================================================================
(global-cwarn-mode 1)

;;=============================================================================
;; Expansion automatique d'abbreviations.
;;=============================================================================

(define-skeleton for-skeleton "For loop skeleton"
  "Loop var: "
  "for(" str _ @ "=0; " str @ "; " str @ ") {" \n
  @ _ \n
  "}" >
  )

(defconst c-expand-list '(("ife" "if () {\n \n} else {\n \n}" (5 10 21))
			  ("ifn" "if () {}" (5 8))
;			  ("uns" "unsigned ")
			  ("for" "for(; ; ) {\n\n}" (5 7 9 13))
;			  ("for" for-skeleton)
			  ("switch" "switch () {\n\n}" (9 13))
			  ("case" "case :\n\nbreak;\n" (6 8 16))
			  ("do" "do {\n\n} while ();" (6 16))
			  ("while" "while () {\n\n}" (8 12))
			  ("default" "default:\n\nbreak;" 10)
			  ("main" "int\nmain(int argc, char * argv[])\n{\n\n}\n" 37))
  "")

(add-hook 'c-mode-hook
	  (function
	   (lambda()
	     (expand-add-abbrevs c-mode-abbrev-table c-expand-list)
	     (expand-mode)
	     (setq fold-end-regexp "}"
		   fold-begin-regexp "{"
		   fold-end-or-begin-regexp "[{}]")
	     (foldingo-activation)
	     (define-key c-mode-map "\C-a" 'beginning-of-code-line)
	     (define-key c-mode-map "\C-e" 'end-of-code-line)
	     )))

(add-hook 'c++-mode-hook
          (function
           (lambda()
             (expand-add-abbrevs c++-mode-abbrev-table c-expand-list)
             (expand-mode)
	     (foldingo-activation)
	     (define-key c++-mode-map "\C-a" 'beginning-of-code-line)
	     (define-key c++-mode-map "\C-e" 'end-of-code-line)
	     )))

;; to prevent fill paragraph to eat the last comment line
(setq c-hanging-comment-ender-p nil)

;;==============================================================================
;; ff-find-other-file
;; cherche un fichier associe grace a l'extension
;;==============================================================================
(setq cc-other-file-alist
  '(
    ("\\.cc$"  (".hh" ".h"))
    ("\\.hh$"  (".cc" ".C" ".CC" ".cxx" ".cpp"))

    ("P\\.h$"  (".c"))
    ("\\.c$"   ("P.h" ".h"))
    ("\\.h$"   (".c" ".cc" ".C" ".CC" ".cxx" ".cpp"))

    ("\\.C$"   (".H"  ".hh" ".h"))
    ("\\.H$"   (".C"  ".CC"))

    ("\\.CC$"  (".HH" ".H"  ".hh" ".h"))
    ("\\.HH$"  (".CC"))

    ("\\.cxx$" (".hh" ".h"))
    ("\\.cpp$" (".hh" ".h"))
    )
)

;;; 40c-prog.el ends here
