;;; rpm-spec-mode.el --- RPM spec file editing commands for XEmacs
;; Copyright (C) 1997,98 Stig Bjørlykke, <stigb@tihlde.hist.no>

;; Authors:  Stig Bjørlykke, <stigb@tihlde.hist.no> and
;;           Steve Sanbeg, <sanbeg@dset.com>
;; Keywords: unix, languages
;; Version:  0.08a

;; This file is not yet part of FSF Emacs or XEmacs.

;; Emacs/XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Emacs/XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs/XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Synched up with:  both in FSF Emacs and XEmacs.

;;; Thanx:

;;   to Tore Olsen <toreo@tihlde.hist.no> for some general fixes. 

;;; ToDo:

;; - different buffernames for different rpm builds.
;; - rewrite function names and shortcuts.
;; - autofill changelog entries.
;; - customize rpm-tags-list and rpm-group-tags-list.
;; - get values from `rpm --showrc'.
;; - ssh/rsh for compile.
;; - finish integrating the new navigation functions in with existing stuff. 
;; - use a single prefix conistently (internal)

;;; Commentary:

;; This mode is used for editing spec files used for building RPM
;; packages.

;; Put this in your .emacs file to enable autoloading of rpm-spec-mode,
;; and auto-recognition of ".spec" files.
;;
;;  (autoload 'rpm-spec-mode "rpm-spec-mode.el" "RPM spec mode." t)
;;  (setq auto-mode-alist (append '(("\\.spec" . rpm-spec-mode))
;;                                auto-mode-alist))
;;
;;------------------------------------------------------------
;; Adapted by Chmouel Boudjnah <chmouel@mandrakesoft.com> for Mandrake



;;; Code:

(defgroup rpm-spec nil
  "RPM Spec mode with XEmacs enhancements."
  :prefix "rpm-spec"
  :group 'languages)

(defcustom rpm-spec-add-attr nil
  "Add %attr entry for filelistings or not."
  :type 'boolean
  :group 'rpm-spec)

(defcustom rpm-spec-short-circuit nil
  "Skip straight to specified stage.
(ie, skip all stages leading up to the specified stage).  Only valid
in \"%build\" and \"%install\" stage."
  :type 'boolean
  :group 'rpm-spec)

(defcustom rpm-spec-timecheck "0"
  "Set the \"timecheck\" age (0 to disable).
The timecheck value expresses, in seconds, the maximum age of a file
being packaged.  Warnings will be printed for all files beyond the
timecheck age."
  :type 'integer
  :group 'rpm-spec)

(defcustom rpm-spec-clean nil
  "Remove the build tree after the packages are made."
  :type 'boolean
  :group 'rpm-spec)

(defcustom rpm-spec-test nil
  "Do not execute any build stages.  Useful for testing out spec files."
  :type 'boolean
  :group 'rpm-spec)

(defcustom rpm-spec-sign-pgp nil
  "Embed a PGP signature in the package.
This signature can be used to verify the integrity and the origin of
the package."
  :type 'boolean
  :group 'rpm-spec)

(defcustom rpm-initialize-sections t
  "Automatically add empty section headings to new spec files"
  :type 'boolean
  :group 'rpm-spec)

(defcustom rpm-insert-version t
  "Automatically add version in a new changelog entry"
  :type 'boolean
  :group 'rpm-spec)

(defgroup rpm-spec-faces nil
  "Font lock faces for RPM Spec mode."
  :group 'rpm-spec
  :group 'faces)

;;------------------------------------------------------------
;; variables used by navigation functions.
(defconst rpm-sections 
  '("preamble" "description" "prep" "setup" "build" "install" "clean"
    "changelog" "files")
  "Partial list of section names.")
(defconst rpm-scripts 
  '("pre" "post" "preun" "postun" "trigger" "triggerun" "triggerpostun")
  "List of rpm scripts")
(defconst rpm-section-seperate "^%\\(\\w+\\)\\s-")
(defconst rpm-section-regexp 
  "^%\\(\\(description\\)\\|\\(prep\\)\\|\\(build\\)\\|\\(install\\)\\|\\(files\\)\\|\\(clean\\)\\|\\(pre\\|post\\(un\\)?\\)\\|\\(trigger\\(post\\)?\\([iu]n\\)?\\)\\)\\b"
  "Regular expression to match beginning of a section.")

;;------------------------------------------------------------

(defface rpm-spec-tag-face
    '(( ((class color) (background light)) (:foreground "Blue3") )
      ( ((class color) (background dark)) (:foreground "Blue3") ))
  "*The face used for tags."
  :group 'rpm-spec-faces)

(defface rpm-spec-macro-face
    '(( ((class color) (background light)) (:foreground "orange3") )
      ( ((class color) (background dark)) (:foreground "orange3") ))
  "*The face used for macros."
  :group 'rpm-spec-faces)

(defface rpm-spec-doc-face
    '(( ((class color) (background light)) (:foreground "#6920ac") )
      ( ((class color) (background dark)) (:foreground "#6920ac") ))
  "*The face used for document files."
  :group 'rpm-spec-faces)

(defface rpm-spec-dir-face
    '(( ((class color) (background light)) (:foreground "green4") )
      ( ((class color) (background dark)) (:foreground "green4") ))
  "*The face used for directories."
  :group 'rpm-spec-faces)

(defface rpm-spec-package-face
    '(( ((class color) (background light)) (:foreground "red3") )
      ( ((class color) (background dark)) (:foreground "red3") ))
  "*The face used for files."
  :group 'rpm-spec-faces)

(defface rpm-spec-ghost-face
    '(( ((class color) (background light)) (:foreground "gray60") )
      ( ((class color) (background dark)) (:foreground "gray60") ))
  "*The face used for ghost tags."
  :group 'rpm-spec-faces)

;;; GNU emacs font-lock needs these...
(defvar rpm-spec-macro-face 'rpm-spec-macro-face "*Face for macros")
(defvar rpm-spec-tag-face 'rpm-spec-tag-face "*Face for tags")
(defvar rpm-spec-package-face 'rpm-spec-package-face "*Face for package tag")
(defvar rpm-spec-dir-face 'rpm-spec-dir-face "*Face for directory entries")
(defvar rpm-spec-doc-face 'rpm-spec-doc-face "*Face for documentation entries")
(defvar rpm-spec-ghost-face 'rpm-spec-ghost-face "*Face for %ghost files")

;;------------------------------------------------------------

(defvar rpm-no-pgp nil "Tell rpm not to sign package.")

(defvar rpm-tags-list
  '(("Autoreqprov")
    ("Buildroot")
    ("Conflicts")
    ("Copyright")
    ("%description")
    ("Distribution")
    ("Excludearch")
    ("Excludeos")
    ("Exclusivearch")
    ("Exclusiveos")
    ("%files")
    ("Group")
    ("Icon")
    ("Name")
    ("Nopatch")
    ("Nosource")
    ("%package")
    ("Packager")
    ("Patch")
    ("Prefix")
    ("Provides")
    ("Release")
    ("Requires")
    ("Serial")
    ("Source")
    ("Summary")
    ("Url")
    ("Vendor")
    ("Version"))
  "List which elements are valid tags.")


(defvar rpm-group-tags-list
  '(
    ("Amusements")
    ("Amusements/Games")
    ("Applications")
    ("Applications/Communications")
    ("Applications/Editors")
    ("Applications/Emulators")
    ("Applications/Engineering")
    ("Applications/File")
    ("Applications/Finance")
    ("Applications/Graphics")
    ("Applications/Internet")
    ("Applications/Multimedia")
    ("Applications/Publishing")
    ("Applications/Sound")
    ("Applications/System")
    ("Applications/Text")
    ("Base")
    ("Development")
    ("Development/Languages")
    ("Development/Libraries")
    ("Development/Tools")
    ("Documentation")
    ("Interface")
    ("Interface/Desktops")
    ("Libraries")
    ("Networking")
    ("Networking/Daemons")
    ("Shells")
    ("System Environment")
    ("System Environment/Base")
    ("System Environment/Daemons")
    ("System Environment/Libraries")
    ("System Environment/Shells")
    ("User Interface")
    ("User Interface/Desktops")
    ("User Interface/X Hardware Support")
    ("Utilities")
    ("Utilities/Archiving")
    ("Utilities/System")
    ("X11")
    ("X11/Applications")
    ("X11/Applications/Networking")
    ("X11/Games")
    ("X11/Games/Strategy")
    )
  "List which elements is valid group tags.")

(defvar rpm-spec-mode-syntax-table nil
  "Syntax table in use in RPM-Spec-mode buffers.")
(unless rpm-spec-mode-syntax-table
  (setq rpm-spec-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" rpm-spec-mode-syntax-table)
  (modify-syntax-entry ?\n ">   " rpm-spec-mode-syntax-table)
  (modify-syntax-entry ?\f ">   " rpm-spec-mode-syntax-table)
  (modify-syntax-entry ?\# "<   " rpm-spec-mode-syntax-table)
  (modify-syntax-entry ?/ "." rpm-spec-mode-syntax-table)
  (modify-syntax-entry ?* "." rpm-spec-mode-syntax-table)
  (modify-syntax-entry ?+ "." rpm-spec-mode-syntax-table)
  (modify-syntax-entry ?- "." rpm-spec-mode-syntax-table)
  (modify-syntax-entry ?= "." rpm-spec-mode-syntax-table)
  (modify-syntax-entry ?% "_" rpm-spec-mode-syntax-table)
  (modify-syntax-entry ?< "." rpm-spec-mode-syntax-table)
  (modify-syntax-entry ?> "." rpm-spec-mode-syntax-table)
  (modify-syntax-entry ?& "." rpm-spec-mode-syntax-table)
  (modify-syntax-entry ?| "." rpm-spec-mode-syntax-table)
  (modify-syntax-entry ?\' "." rpm-spec-mode-syntax-table))

(defvar rpm-spec-mode-map nil
  "Keymap used in RPM Spec mode.")
(unless rpm-spec-mode-map
  (setq rpm-spec-mode-map (make-sparse-keymap))
  (and (functionp 'set-keymap-name)
       (set-keymap-name rpm-spec-mode-map 'rpm-spec-mode-map))
  (define-key rpm-spec-mode-map "\C-cp" 'rpm-build-bp)
  (define-key rpm-spec-mode-map "\C-cl" 'rpm-build-bl)
  (define-key rpm-spec-mode-map "\C-cc" 'rpm-build-bc)
  (define-key rpm-spec-mode-map "\C-ci" 'rpm-build-bi)
  (define-key rpm-spec-mode-map "\C-cb" 'rpm-build-bb)
  (define-key rpm-spec-mode-map "\C-ca" 'rpm-build-ba)
  (define-key rpm-spec-mode-map "\C-ct" 'rpm-change-timecheck)
  (define-key rpm-spec-mode-map "\C-c\C-r" 'rpm-spec-increase-release-tag-2)
  (define-key rpm-spec-mode-map "\C-cr" 'rpm-spec-increase-release-tag)
  (define-key rpm-spec-mode-map "\C-ce" 'rpm-add-change-log-entry)
  ;;May be better to have movement commands on \C-ck, and build on \C-c\C-k
  (define-key rpm-spec-mode-map "\C-c\C-e" 'rpm-insert-tag)
  (define-key rpm-spec-mode-map "\C-c\C-n" 'rpm-forward-section)
  (define-key rpm-spec-mode-map "\C-c\C-p" 'rpm-backward-section)
  (define-key rpm-spec-mode-map "\C-c\C-g" 'rpm-goto-section)
  (define-key rpm-spec-mode-map "\C-u\C-c\C-e" 'rpm-change-tag)
  (define-key rpm-spec-mode-map "\C-c\C-id" 'rpm-insert-description)
  (define-key rpm-spec-mode-map "\C-c\C-ic" 'rpm-insert-package)
  (define-key rpm-spec-mode-map "\C-c\C-if" 'rpm-insert-files)
  (define-key rpm-spec-mode-map "\C-c\C-is" 'rpm-insert-source)
  (define-key rpm-spec-mode-map "\C-c\C-us" 'rpm-change-source)
  (define-key rpm-spec-mode-map "\C-c\C-ip" 'rpm-insert-patch)
  (define-key rpm-spec-mode-map "\C-c\C-up" 'rpm-change-patch)
  (define-key rpm-spec-mode-map "\C-c\C-ia" 'rpm-insert-packager)
  (define-key rpm-spec-mode-map "\C-c\C-ua" 'rpm-change-packager)
  (define-key rpm-spec-mode-map "\C-c\C-iu" 'rpm-insert-url)
  (define-key rpm-spec-mode-map "\C-c\C-uu" 'rpm-change-url)
  (define-key rpm-spec-mode-map "\C-c\C-ib" 'rpm-insert-buildroot)
  (define-key rpm-spec-mode-map "\C-c\C-ub" 'rpm-change-buildroot)
  (define-key rpm-spec-mode-map "\C-c\C-ig" 'rpm-insert-group)
  (define-key rpm-spec-mode-map "\C-c\C-ug" 'rpm-change-group)
  (define-key rpm-spec-mode-map "\C-c\C-ir" 'rpm-insert-prefix)
  (define-key rpm-spec-mode-map "\C-c\C-ur" 'rpm-change-prefix)
  (define-key rpm-spec-mode-map "\C-c\C-ff" 'rpm-insert-file)
  (define-key rpm-spec-mode-map "\C-c\C-fc" 'rpm-insert-config)
  (define-key rpm-spec-mode-map "\C-c\C-fd" 'rpm-insert-doc)
  (define-key rpm-spec-mode-map "\C-c\C-fg" 'rpm-insert-ghost)
  (define-key rpm-spec-mode-map "\C-c\C-dd" 'rpm-insert-dir)
  (define-key rpm-spec-mode-map "\C-c\C-do" 'rpm-insert-docdir)
  (define-key rpm-spec-mode-map "\C-q" 'indent-spec-exp)
  (define-key rpm-spec-mode-map "\t" 'sh-indent-line)
  (define-key rpm-spec-mode-map "\C-c\C-n" 'rpm-insert-true-prefix)
  )

(defconst rpm-spec-mode-menu
  (purecopy '("RPM-Spec"
	      ("Insert Environment  (C-c C-e)"
	       ["Buildroot..."          rpm-insert-buildroot    t]
	       ["%description..."       rpm-insert-description  t]
	       ["%files..."             rpm-insert-files        t]
	       ["Group..."              rpm-insert-group        t]
	       ["%package..."           rpm-insert-package      t]
	       ["Packager..."           rpm-insert-packager     t]
	       ["Patch..."              rpm-insert-patch        t]
	       ["Prefix..."             rpm-insert-prefix       t]
	       ["Requires..."           rpm-insert-requires     t]
	       ["Source..."             rpm-insert-source       t]
	       ["URL..."                rpm-insert-url          t])
	      ("Change Environment  (C-u C-c C-e)"
	       ["Buildroot..."          rpm-change-buildroot    t]
	       ["Group..."              rpm-change-group        t]
	       ["Packager..."           rpm-change-packager     t]
	       ["Patch..."              rpm-change-patch        t]
	       ["Prefix..."             rpm-change-prefix       t]
	       ["Requires..."           rpm-change-requires     t]
	       ["Source..."             rpm-change-source       t]
	       ["URL..."                rpm-change-url          t])
	      ("Add file entry"
		["Regular file..."        rpm-insert-file       t]
		["Config file..."         rpm-insert-config     t]
		["Document file..."       rpm-insert-doc        t]
		["Ghost file..."          rpm-insert-ghost      t]
		"---"
		["Directory..."           rpm-insert-dir        t]
		["Document directory..."  rpm-insert-docdir     t]
		"---"
		["Default add \"%attr\" entry" (setq rpm-spec-add-attr
						    (not rpm-spec-add-attr))
		 :style toggle :selected rpm-spec-add-attr])
	      "---"
	      ["Go to section..."     rpm-mouse-goto-section t]
	      ["Forward section"      rpm-forward-section    t]
	      ["Backward sectoin"     rpm-backward-section   t]
	      "---"
	      ["Execute \"%prep\" stage"   rpm-build-bp      t]
	      ["Do a \"list check\""       rpm-build-bl      t]
	      ["Do the \"%build\" stage"   rpm-build-bc      t]
	      ["Do the \"%install\" stage" rpm-build-bi      t]
	      ["Build binary package"      rpm-build-bb      t]
	      ["Build binary and source"   rpm-build-ba      t]
	      "---"
	      ["Add changelog entry..."    rpm-add-change-log-entry      t]
	      ["Change timecheck value..." rpm-change-timecheck          t]
	      ["Increase release-tag"      rpm-spec-increase-release-tag t]
	      "---"
	      ["Short Circuit" (setq rpm-spec-short-circuit
				     (not rpm-spec-short-circuit))
	       :style toggle :selected rpm-spec-short-circuit]
	      ["Clean" (setq rpm-spec-clean (not rpm-spec-clean))
	       :style toggle :selected rpm-spec-clean]
	      ["Testing only" (setq rpm-spec-test (not rpm-spec-test))
	       :style toggle :selected rpm-spec-test]
	      ["PGP Sign" (setq rpm-spec-sign-pgp (not rpm-spec-sign-pgp))
	       :style toggle :selected rpm-spec-sign-pgp]
	      "---"
	      ["About rpm-spec-mode"       rpm-about-rpm-spec-mode        t]
	      )))

(defvar rpm-spec-font-lock-keywords
  '(
    ("%[a-zA-Z-]+" 0 rpm-spec-macro-face)
    ("^\\([a-zA-Z0-9]+\\):" 1 rpm-spec-tag-face)
    ("%\\(define\\|files\\|package\\|description\\)[ \t]+\\([^ \t\n-]+\\)"
     (2 rpm-spec-package-face))
    ("%configure " 0 rpm-spec-macro-face)
    ("%dir[ \t]+\\([^ \t\n]+\\)[ \t]*" 1 rpm-spec-dir-face)
    ("%doc\\(\\|dir\\)[ \t]+\\(.*\\)\n" 2 rpm-spec-doc-face)
    ("%\\(ghost\\|config\\)[ \t]+\\(.*\\)\n" 2 rpm-spec-ghost-face)
    ("^%.+-[a-zA-Z][ \t]+\\([a-zA-Z0-9\.-]+\\)" 1 rpm-spec-doc-face)
    ("^\\(.+\\)(\\([a-zA-Z]\\{2,2\\}\\)):" 
     (1 rpm-spec-tag-face)
     (2 rpm-spec-doc-face))
    ("^\\*\\(.\\{16,16\\}\\).\\(.*\\)\\(<.*>\\)\n"
     (1 rpm-spec-dir-face)
     (2 rpm-spec-package-face)
     (3 rpm-spec-tag-face))
    ("%{[a-zA-Z_-]+}" 0 rpm-spec-doc-face)
    )
  "Additional expressions to highlight in RPM Spec mode.")

;;Initialize font lock for xemacs
(put 'rpm-spec-mode 'font-lock-defaults '(rpm-spec-font-lock-keywords))

(defvar rpm-spec-mode-abbrev-table nil
  "Abbrev table in use in RPM-Spec-mode buffers.")
(define-abbrev-table 'rpm-spec-mode-abbrev-table ())

;;------------------------------------------------------------

;;;###autoload
(defun rpm-spec-mode ()
  "Major mode for editing spec files.
This is much like C mode except for the syntax of comments.  It uses
the same keymap as C mode and has the same variables for customizing
indentation.  It has its own abbrev table and its own syntax table.

Turning on RPM Spec mode calls the value of the variable `rpm-spec-mode-hook'
with no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (condition-case nil
      (require 'shindent)
    (error
     (require 'sh-script)))
  (require 'cc-mode)
  (use-local-map rpm-spec-mode-map)
  (setq major-mode 'rpm-spec-mode)
  (setq mode-name "RPM-SPEC")
  (setq rpm-buffer-name "*RPM Background*")
  (setq local-abbrev-table rpm-spec-mode-abbrev-table)
  (set-syntax-table rpm-spec-mode-syntax-table)

  (require 'easymenu)
  (easy-menu-define rpm-spec-call-menu rpm-spec-mode-map 
		    "Post menu for rpm-spec-mode" rpm-spec-mode-menu)
  (easy-menu-add rpm-spec-mode-menu)

  (if (= (buffer-size) 0)
      (rpm-spec-initialize))

  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
;  (make-local-variable 'indent-line-function)
;  (setq indent-line-function 'c-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "#+ *")
;  (make-local-variable 'comment-indent-function)
;  (setq comment-indent-function 'c-comment-indent)
  ;;Initialize font lock for GNU emacs.
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(rpm-spec-font-lock-keywords nil t))
  (run-hooks 'rpm-spec-mode-hook))

(defun rpm-command-filter (process string)
  "Filter to process normal output."
  (save-excursion
    (set-buffer (process-buffer process))
    (save-excursion
      (goto-char (process-mark process))
      (insert-before-markers string)
      (set-marker (process-mark process) (point)))))

;;------------------------------------------------------------

(defun rpm-add-change-log-entry (&optional change-log-entry)
  "Find change log and add an entry for today."
  (interactive "p")
  (save-excursion
    (goto-char (point-min))
    (if (search-forward-regexp "^%changelog[ \t]*$" nil t)
	(let ((string (concat "* " (substring (current-time-string) 0 11)
			      (substring (current-time-string) -4) " "
			      (user-full-name) " <" user-mail-address "> "
			      (or (and rpm-insert-version
				   (rpm-find-spec-version))
				  "")
				   )))
	  (if (not (search-forward string nil t))
	      (insert "\n" string "\n\n")
	    (next-line 2)
	    (beginning-of-line))
	  (if (eq change-log-entry 1)
	      (insert "- " (read-from-minibuffer "Changelog entry: ") "\n")
	    (insert "- " change-log-entry)))
      (message "No \"%%changelog\" entry found..."))))

;;------------------------------------------------------------

(defun rpm-insert-f (&optional filetype filename)
  "Insert new %files entry."
  (save-excursion
    (goto-char (point-min))
    (if (search-forward-regexp "^%files[ ]*$" nil t)
	(progn
	  (if (or (eq filename 1) (not filename))
	      (insert "\n" (read-file-name
			    (concat filetype "filename: ") "" "" nil))
	    (insert "\n" filename))
	  (beginning-of-line)
	  (if rpm-spec-add-attr (insert "%attr(-, root, root) "))
	  (insert filetype))
      (message "No \"%%files\" entry found..."))))
    

(defun rpm-insert-file (&optional filename)
  "Insert regular file."
  (interactive "p")
  (rpm-insert-f "" filename))
  
(defun rpm-insert-config (&optional filename)
  "Insert config file."
  (interactive "p")
  (rpm-insert-f "%config " filename))

(defun rpm-insert-doc (&optional filename)
  "Insert doc file."
  (interactive "p")
  (rpm-insert-f "%doc " filename))

(defun rpm-insert-ghost (&optional filename)
  "Insert ghost file."
  (interactive "p")
  (rpm-insert-f "%ghost " filename))

(defun rpm-insert-dir (&optional dirname)
  "Insert directory."
  (interactive "p")
  (rpm-insert-f "%dir " dirname))

(defun rpm-insert-docdir (&optional dirname)
  "Insert doc directory."
  (interactive "p")
  (rpm-insert-f "%docdir " dirname))

;;------------------------------------------------------------

(defun rpm-insert (&optional what file-completion)
  "Insert given tag.  Use file-completion if argument is t."
  (beginning-of-line)
  (if (not what)
      (setq what (completing-read "Tag: " rpm-tags-list)))
  (if (string-match "^%" what)
      (setq read-text (concat "Packagename for " what ": ")
	    insert-text (concat what " "))
    (setq read-text (concat what ": ")
	  insert-text (concat what ": ")))
  (cond
   ((string-equal what "Group")
    (rpm-insert-group))
   ((string-equal what "Source")
    (rpm-insert-n "Source"))
   ((string-equal what "Patch")
    (rpm-insert-n "Patch"))
   (t
    (if file-completion
	(insert insert-text (read-file-name (concat read-text) "" "" nil) "\n")
      (insert insert-text (read-from-minibuffer (concat read-text)) "\n")))))

(defun rpm-insert-n (what &optional arg)
  "Insert given tag with possible number."
  (save-excursion
    (goto-char (point-max))
    (if (search-backward-regexp (concat "^" what "\\([0-9]*\\):") nil t)
	(let ((release (1+ (string-to-int (match-string 1)))))
	  (next-line 1)
	  (beginning-of-line)
	  (let ((default-directory "/usr/src/redhat/SOURCES/"))
	    (insert what (int-to-string release) ": "
		    (read-file-name (concat what "file: ") "" "" nil) "\n")))
      (goto-char (point-min))
      (rpm-end-of-section)
      (insert what ": " (read-from-minibuffer (concat what "file: ")) "\n"))))

(defun rpm-change (&optional what arg)
  "Update given tag."
  (save-excursion
    (if (not what)
	(setq what (completing-read "Tag: " rpm-tags-list)))
    (cond
     ((string-equal what "Group")
      (rpm-change-group))
     ((string-equal what "Source")
      (rpm-change-n "Source"))
     ((string-equal what "Patch")
      (rpm-change-n "Patch"))
     (t
      (goto-char (point-min))
      (if (search-forward-regexp (concat "^" what ":[ ]*\\(.*\\)$") nil t)
	  (replace-match
	   (concat what ": " (read-from-minibuffer
			      (concat "New " what ": ") (match-string 1))))
	(message (concat what " tag not found...")))))))

(defun rpm-change-n (what &optional arg)
  "Change given tag, possible numbers."
  (save-excursion
    (goto-char (point-min))
    (let ((number (read-from-minibuffer (concat what " number: "))))
      (if (search-forward-regexp
	   (concat "^" what number ":[ ]*\\(.*\\)") nil t)
	  (let ((default-directory "/usr/src/redhat/SOURCES/"))
	    (replace-match
	     (concat what number ": "
		     (read-file-name (concat "New " what number " file: ")
					 "" "" nil (match-string 1)))))
	(message (concat what " number \"" number "\" not found..."))))))

(defun rpm-insert-group (&optional arg)
  "Insert Group tag."
  (interactive "p")
  (beginning-of-line)
  (insert "Group: " (completing-read "Group: " rpm-group-tags-list) "\n"))

(defun rpm-change-group (&optional arg)
  "Update Group tag."
  (interactive "p")
  (save-excursion
    (goto-char (point-min))
    (if (search-forward-regexp "^Group: \\(.*\\)$" nil t)
	(replace-match
	 (concat "Group: "
		 (insert (completing-read "Group: " rpm-group-tags-list
					  nil nil (match-string 1)))))
      (message "Group tag not found..."))))

(defun rpm-insert-tag (&optional arg)
  "Insert a tag."
  (interactive "p")
  (rpm-insert))

(defun rpm-change-tag (&optional arg)
  "Change a tag."
  (interactive "p")
  (rpm-change))

(defun rpm-insert-source (&optional arg)
  "Insert Source tag."
  (interactive "p")
  (rpm-insert-n "Source"))

(defun rpm-change-source (&optional arg)
  "Change Source tag."
  (interactive "p")
  (rpm-change-n "Source"))

(defun rpm-insert-patch (&optional arg)
  "Insert Patch tag."
  (interactive "p")
  (rpm-insert-n "Patch"))

(defun rpm-change-patch (&optional arg)
  "Change Source tag."
  (interactive "p")
  (rpm-change-n "Patch"))

(defun rpm-insert-url (&optional arg)
  "Insert URL tag."
  (interactive "p")
  (rpm-insert "Url"))

(defun rpm-change-url (&optional arg)
  "Update URL tag."
  (interactive "p")
  (rpm-change "Url"))

(defun rpm-insert-buildroot (&optional arg)
  "Insert Buildroot tag."
  (interactive "p")
  (rpm-insert "Buildroot" t))

(defun rpm-change-buildroot (&optional arg)
  "Update Buildroot tag."
  (interactive "p")
  (rpm-change "Buildroot"))

(defun rpm-insert-requires (&optional arg)
  "Insert Requires tag."
  (interactive "p")
  (rpm-insert "Requires" t))

(defun rpm-change-requires (&optional arg)
  "Update Requires tag."
  (interactive "p")
  (rpm-change "Requires"))

(defun rpm-insert-summary (&optional arg)
  "Insert Summary tag."
  (interactive "p")
  (rpm-insert "Summary"))

(defun rpm-change-summary (&optional arg)
  "Update Summary tag."
  (interactive "p")
  (rpm-change "Summary"))

(defun rpm-insert-description (&optional arg)
  "Insert %description tag."
  (interactive "p")
  (rpm-insert "%description"))

(defun rpm-insert-files (&optional arg)
  "Insert %files tag."
  (interactive "p")
  (rpm-insert "%files"))

(defun rpm-insert-package (&optional arg)
  "Insert %package tag."
  (interactive "p")
  (rpm-insert "%package"))

(defun rpm-insert-packager (&optional arg)
  "Insert Packager tag."
  (interactive "p")
  (beginning-of-line)
  (insert "Packager: " (user-full-name) " <" user-mail-address ">\n"))

(defun rpm-change-packager (&optional arg)
  "Update Packager tag."
  (interactive "p")
  (rpm-change "Packager"))

;;------------------------------------------------------------

(defun rpm-current-section nil
  (interactive)
  (save-excursion
    (rpm-forward-section)
    (rpm-backward-section)
    (if (bobp) "preamble" 
      (buffer-substring (match-beginning 1) (match-end 1)))))

(defun rpm-backward-section nil
  "Move backward to the beginning of the previous section.
Go to beginning of previous section."
  (interactive)
  (or (re-search-backward rpm-section-regexp nil t)
      (goto-char (point-min))))

(defun rpm-beginning-of-section nil
  "Move backward to the beginning of the current section.
Go to beginning of current section."
  (interactive)
  (or (and (looking-at rpm-section-regexp) (point))
      (re-search-backward rpm-section-regexp nil t)
      (goto-char (point-min))))

(defun rpm-forward-section nil
  "Move forward to the beginning of the next section."
  (interactive)
  (forward-char)
  (if (re-search-forward rpm-section-regexp nil t)
      (progn (forward-line 0) (point))
    (goto-char (point-max))))

(defun rpm-end-of-section nil
  "Move forward to the end of this section."
  (interactive)
  (forward-char)
  (if (re-search-forward rpm-section-regexp nil t)
      (forward-line -1)
    (goto-char (point-max)))
  (while (looking-at paragraph-separate)
    (forward-line -1))
  (forward-line 1)
  (point))

(defun rpm-goto-section (section)
  "Move point to the beginning of the specified section; 
leave point at previous location."
  (interactive "ssection:")
  (push-mark)
  (goto-char (point-min))
  (or 
   (equal section "preamble")
   (re-search-forward (concat "^%" section "\\b") nil t)
   (let ((s (cdr rpm-sections)))
     (while (not (equal section (car s)))
       (re-search-forward (concat "^%" (car s) "\\b") nil t)
       (setq s (cdr s)))
     (if (re-search-forward rpm-section-regexp nil t)
	 (forward-line -1) (goto-char (point-max)))
     (insert "\n%" section "\n"))))

(defun rpm-mouse-goto-section (&optional section)
  (interactive 
   (x-popup-menu
    last-nonmenu-event 
    (list "sections" 
	  (cons "sections" (mapcar (lambda (e) (list e e)) rpm-sections))
	  (cons "scripts" (mapcar (lambda (e) (list e e)) rpm-scripts))
	  )))
  (and section ;if user doesn't pick a section, exit quietly.
       (if (member section rpm-sections)
	   (rpm-goto-section section)
	 (goto-char (point-min))
	 (or (re-search-forward (concat "^%" section "\\b") nil t)
	     (and (re-search-forward "^%files\\b" nil t) (forward-line -1))
	     (goto-char (point-max))))))

(defun rpm-insert-true-prefix () 
  (interactive)
  (insert "%{prefix}"))

;;------------------------------------------------------------

(defun rpm-build (buildoptions)
  "Build this rpm-package."
  (rpm-process-check rpm-buffer-name)

  (if (get-buffer rpm-buffer-name)
      (kill-buffer rpm-buffer-name))
  (create-file-buffer rpm-buffer-name)
  (display-buffer rpm-buffer-name)
  (setq buildoptions (list buildoptions buffer-file-name))
  (if (or rpm-spec-short-circuit rpm-spec-test)
      (setq rpm-no-pgp t))
  (if rpm-spec-clean
      (setq buildoptions (cons "--clean" buildoptions)))
  (if rpm-spec-short-circuit
      (setq buildoptions (cons "--short-circuit" buildoptions)))
  (if rpm-spec-test
      (setq buildoptions (cons "--test" buildoptions)))
  (if (and rpm-spec-sign-pgp (not rpm-no-pgp))
      (setq buildoptions (cons "--sign" buildoptions)))
  ;; FL Thu Oct  7 13:18:16 1999
  (save-excursion
    (set-buffer (get-buffer rpm-buffer-name))
    (goto-char (point-max)))
  (let ((process
         (apply 'start-process "rpm" rpm-buffer-name "rpm" buildoptions)))
    (if (and rpm-spec-sign-pgp (not rpm-no-pgp))
	(let ((rpm-passwd-cache (read-passwd "PGP passphrase: ")))
	  (process-send-string process (concat rpm-passwd-cache "\n"))))
    (set-process-filter process 'rpm-command-filter)))

(defun rpm-build-bp (&optional arg)
  "Run a `rpm -bp'."
  (interactive "p")
  (if rpm-spec-short-circuit
      (message "Cannot run `rpm -bp' with short-circuit")
    (setq rpm-no-pgp t)
    (rpm-build "-bp")))

(defun rpm-build-bl (&optional arg)
  "Run a `rpm -bl'."
  (interactive "p")
  (if rpm-spec-short-circuit
      (message "Cannot run `rpm -bl' with short-circuit")
    (setq rpm-no-pgp t)
    (rpm-build "-bl")))

(defun rpm-build-bc (&optional arg)
  "Run a `rpm -bc'."
  (interactive "p")
  (setq rpm-no-pgp t)
  (rpm-build "-bc"))

(defun rpm-build-bi (&optional arg)
  "Run a `rpm -bi'."
  (interactive "p")
  (setq rpm-no-pgp t)
  (rpm-build "-bi"))

(defun rpm-build-bb (&optional arg)
  "Run a `rpm -ba'."
  (interactive "p")
  (if rpm-spec-short-circuit
      (message "Cannot run `rpm -bb' with short-circuit")
    (setq rpm-no-pgp nil)
    (rpm-build "-bb")))

(defun rpm-build-ba (&optional arg)
  "Run a `rpm -ba'."
  (interactive "p")
  (if rpm-spec-short-circuit
      (message "Cannot run `rpm -ba' with short-circuit")
    (setq rpm-no-pgp nil)
    (rpm-build "-ba")))

(defun rpm-process-check (buffer)
  "Check if BUFFER has a running process.
If so, give the user the choice of aborting the process or the current
command."
  (let ((process (get-buffer-process (get-buffer buffer))))
    (if (and process (eq (process-status process) 'run))
	(if (yes-or-no-p (concat "Process `" (process-name process)
				 "' running.  Kill it? "))
	    (delete-process process)
	  (error "Cannot run two simultaneous processes ...")))))

;;------------------------------------------------------------

(defun rpm-change-timecheck (&optional arg)
  "Change the value for timecheck."
  (interactive "p")
  (setq rpm-spec-timecheck
	(read-from-minibuffer "New timecheck: " rpm-spec-timecheck)))

(defun rpm-spec-increase-release-tag (&optional arg)
  "Increase the release tag by 1."
  (interactive "p")
  (save-excursion
    (goto-char (point-min))
    (if (search-forward-regexp "^Release: \\([0-9]+\\)\\(.*\\)" nil t)
	(let ((release (1+ (string-to-int (match-string 1)))))
	  (setq release (concat (int-to-string release) (match-string 2)))
	  (replace-match (concat "Release: " release))
	  (message (concat "Release tag changed to " release ".")))
      (message "No Release tag found..."))))

(defun rpm-spec-increase-release-tag-2 (&optional arg)
  "Increase the release tag by 1 [2]."
  (interactive "p")
  (save-excursion
    (goto-char (point-min))
    (if (search-forward-regexp "^%define release \\([0-9]+\\)\\(.*\\)" nil t)
	(let ((release (1+ (string-to-int (match-string 1)))))
	  (setq release (concat (int-to-string release) (match-string 2)))
	  (replace-match (concat "%define release " release))
	  (message (concat "Release tag changed to " release ".")))
      (message "No %define release tag found..."))))


;;------------------------------------------------------------

(defun rpm-spec-initialize ()
  "Create a default spec file if one does not exist or is empty."
  (let (file name version (release "1"))
    (setq file (if (buffer-file-name)
		   (file-name-nondirectory (buffer-file-name))
		 (buffer-name)))
    (cond
     ((eq (string-match "\\(.*\\)-\\([^-]*\\)-\\([^-]*\\).spec" file) 0)
      (setq name (match-string 1 file))
      (setq version (match-string 2 file))
      (setq release (match-string 3 file)))
     ((eq (string-match "\\(.*\\)-\\([^-]*\\).spec" file) 0)
      (setq name (match-string 1 file))
      (setq version (match-string 2 file)))
     ((eq (string-match "\\(.*\\).spec" file) 0)
      (setq name (match-string 1 file))))
    
    (insert
	    "%define name " (or name "") 
	    "\n%define version " (or version "VERSION")
	    "\n%define release " (or release "")"mdk"
	    "\n\nSummary: "
	    "\nName: %{name}"
	    "\nVersion: %{version}" 
	    "\nRelease: %{release}"
	    "\nSource0: %{name}-%{version}.tar.bz2"
	    "\nCopyright: \nGroup: "
	    "\nBuildRoot: /tmp/%{name}-buildroot\nPrefix: %{_prefix}"
	    "\n\n%description\n"
	    "\n%prep\n%setup\n\n%build\n\n%install\nrm -rf $RPM_BUILD_ROOT"
	    "\n\n\n%clean\nrm -rf $RPM_BUILD_ROOT"
	    "\n\n%files\n%defattr(-,root,root,0755)\n" 
	    "\n\n%changelog\n"
	    "\n# end of file\n")
    (rpm-add-change-log-entry "First spec file for Mandrake distribution.\n"))
  )

;;------------------------------------------------------------
(defun rpm-spec-field-value (field max)
  (save-excursion
    (let ((str (progn
		 (goto-char (point-min))
		 (search-forward-regexp (concat field ": *\\(.+\\).*$") max)
		 (match-string 1) )))
      (if (string-match "%{?\\(.*\\)}?" str)
	  (progn
	    (goto-char (point-min))
	    (search-forward-regexp (concat "%define +" (substring str (match-beginning 1)
								  (match-end 1) )
					   " +\\(.*\\)"))
	    (match-string 1) )
	str) ) ) )

(defun rpm-find-spec-version ()
  (save-excursion
    (goto-char (point-min))
    (let* ((max (search-forward-regexp rpm-section-regexp))
	   (version (rpm-spec-field-value "Version" max))
	   (release (rpm-spec-field-value "Release" max)) )
      (concat version "-" release))))

;;------------------------------------------------------------

(defun rpm-about-rpm-spec-mode (&optional arg)
  "About rpm-spec-mode."
  (interactive "p")
  (message "Made by Stig Bjørlykke, <stigb@tihlde.hist.no>"))

(provide 'rpm-spec-mode)

;;; rpm-spec-mode.el ends here
