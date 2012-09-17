;; 80template.el --- 
;;
;; Copyright (C) 1996, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: template.el,v 1.4 2006-02-17 18:18:01 fred Exp $
;; Keywords: 
;;

;;=============================================================================
;; Autoloaded section.
;;=============================================================================


;;;###

;;;***


;;=============================================================================
;; Configuration section.
;;=============================================================================
(require 'defaultcontent)
(require 'defaultbanner)

(defvar project-name "")
(defvar project-version-string (concat "$Id" "$"))

(setq dc-auto-insert-directory (expand-file-name "~/templates/"))

(setq dc-auto-insert-alist
      '(("test_.*\\.cc$" . "test.cc")
	("\\.c$"        . "temp.c")
	("\\.h$"        . "temp.h")
	("\\.sh$"	. "temp.sh")
	("[0-9][0-9].*\\.el$" . "config.el")
	("test_.*\\.py$" . "test.py")
	("\\.py$"       . "temp.py")
	("local.rules"  . "local.rules")
	("\\.spec$"     . "temp.spec")
	("spec[0-9]+.xml$" . "temp.spec.xml")
	))

(setq dc-auto-insert-mode-alist
      '((emacs-lisp-mode . "temp.el")
	(scheme-mode     . "temp.scm")
	(perl-mode       . "temp.pl")
	))

(setq dc-expandable-variables-alist
  '(( "@BASEFILENAME@"  (file-name-nondirectory buffer-file-name) )
    ( "@BASEFILENAME_WITHOUT_EXT@"  (file-name-without-ext buffer-file-name) )
    ( "@BASEFILENAME_WITHOUT_TEST@"  (file-name-without-test buffer-file-name) )
    ( "@FILENAME@"      buffer-file-name )
    ( "@DATE@"          (current-time-string) )
    ( "@YEAR@"		(substring (current-time-string) 20 24) )
    ( "@HOST@"          (getenv "HOST") )
    ( "@AUTHOR@"        (capitalize (getenv "USER")) )
    ( "@EMAIL@"		user-mail-address )
    ( "@COMMENT-START@" (if comment-start comment-start "") )
    ( "@COMMENT-END@"   (if comment-end comment-end "") )
    ( "@VERSION@"	(version (directory-of-file buffer-file-name)))
    ( "@PACKAGE@"	(package (directory-of-file buffer-file-name)))

    ( "@SPECNUMBER@"	(number (file-name-nondirectory buffer-file-name)))
    
    ( "@DOT@"           (setq dc-initial-dot-position (match-beginning 0))
                        "" )
    ( "@UPPER_CASE_FILE_NAME@"	(cpp-filename (file-name-nondirectory buffer-file-name)))
    ( "@CAMELIZED_FILE_NAME@"	(camelized-filename (file-name-nondirectory buffer-file-name)))
    ( "@FILE_NAME@"		(file-name-nondirectory (file-name-nondirectory buffer-file-name)))
    ( "@CLASS_NAME@"		(class-name (file-name-nondirectory buffer-file-name)))
    ( "@INCLUDE_FILE_NAME@"	(include-file-name (file-name-nondirectory buffer-file-name)))
    ( "@USER_FULL_NAME@"	(user-full-name))
    ( "@CURRENT_TIME@"		(current-time-string))
    ( "@PROJECT@"		project-name)
    ( "@VERSION_STRING@"	project-version-string)
    ( "@APPLICATION@"		(directory-of-file buffer-file-name))
    ( "@MODE@"			(mode))
    ( "@@"              "@")
    ;; You can use that default to access all variables from shell env
    ;; but only if dc-fast-variable-handling is false to enable use of
    ;; regexps.
    ;( "@\\(.*\\)@"         (let (name value)
    ;                         (setq name (buffer-substring 
    ;                                     (match-beginning 1)
    ;                                     (match-end 1) ))
    ;                         (setq value (getenv name))
    ;                         (if value value "") ) )
    ;; Another possible extension is to have executable code between @
    ;; Courtesy of Luc Moreau:
    ( "@\\(.*\\)@"         (let (sexp value (here (point)))
                             (goto-char (+ 1 (match-beginning 0)))
                             (setq sexp (read-closest-sexp))
                             (if sexp (setq value (eval sexp)))
                             (goto-char here)
                             (if value value "") ) )    
    ))


(defun version (dir)
  "return the extracted version from path/package-version"
  (message "version %s" dir)
  (if (string-match ".*-\\([0-9p.]+$\\)" dir)
      (substring dir (match-beginning 1) (match-end 1))
    "") )

(defun number (dir)
  "return the extracted version from path/package-version"
  (message "version %s" dir)
  (if (string-match "\\([0-9]+\\)" dir)
      (substring dir (match-beginning 1) (match-end 1))
    "") )

(defun package (dir)
  "return the extracted version from path/package-version"
  (message "package %s" dir)
  (if (string-match "\\([^/]+\\)-[0-9p.]+$" dir)
      (substring dir (match-beginning 1) (match-end 1))
    "") )

(defun file-name-without-ext (name)
  "return the file name without the extension. ex toto.el => toto"
  (let ((filename (file-name-nondirectory name)))
    (if (string-match "\\(.+\\)\\.[^.]*" filename)
	(substring filename (match-beginning 1) (match-end 1))
      filename)))

(defun file-name-without-test (name)
  "return the file name without the test prefix. ex test_toto.cc => toto"
  (let ((filename (file-name-nondirectory name)))
    (if (string-match "test_\\(.+\\)\\.[^.]*" filename)
	(substring filename (match-beginning 1) (match-end 1))
      filename)))

(defun mode()
  "return the major mode without -mode"
  (let ((name (symbol-name major-mode)))
    (substring name 0 (- (length name) 5))))

(defun cpp-filename (filename)
"return an uppercase string in the form _FILE_NAME"
  (upcase
   (concat "_"
	   ;; make sure only word constituents are used in #ifdef's
	   (mapcar '(lambda (val) (if (and (eq (char-syntax val) ?w)
					   (not (eq val ?.)))
				      val ?_))
		   filename))))

(defun capitalize-first-letter (s)
  "return the word with the first letter capitalized"
  (concat (capitalize (substring s 0 1))
	  (substring s 1)))

(defun camelized-filename (s)
  "return the word with each first letter after a _ capitalized: Ex: test_toto.c: TestToto"
  (let ((name (file-name-without-ext s)))
    (apply (function concat) (mapcar (function capitalize-first-letter) (split-string name "_")))
    ))

(defun class-name (file)
  "return a class name based on a file name.
toto.h => aToto"
  (let ((first (if (string-match (substring file 0 1) "aeiouy")
		   "an"
		 "a")))
    (concat first
	    (capitalize-first-letter
	     (if (string-match "\\.[^./]+$" file)
		 (substring file 0 (match-beginning 0))
	       file)))))

(defun directory-of-file (file)
  "return the directory the file name.
/xx/yy/zz/toto => zz"
  (if (string-match "\\([^/]+\\)\\(/[^/]+$\\)" (expand-file-name file))
	      (substring file (match-beginning 1) (match-beginning 2))
	    ""))

(defun include-file-name (file)
  "return a fine name based on a file name.
toto.XX => toto.h"
  (concat (if (string-match "\\.[^./]+$" file)
	      (substring file 0 (match-beginning 0))
	    file) ".h"))

(if (file-directory-p (expand-file-name "~/emacs/yasnippet"))
    (progn
      (add-to-list 'load-path
		   "~/emacs/yasnippet")
      (require 'yasnippet)
      (yas/global-mode 1)
      ))

;;; 80template.el ends here
