;;; desktop-phase.el --- desktop extension to throw out old files.

;; Copyright (c) 1995, 1996, 1997 by Juergen A. Erhard

;; Author: Juergen A. Erhard <jae@laden.ilk.de>
;; Version: $Revision: 1.7 $ 

;; This file is NOT YET part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;    This file implements an extension to desktop.el. If activated,
;;    desktop 'phases out' visited files that haven't been selected
;;    (displayed) for a configurable number of sessions. It does this
;;    by just not saving any information about them in
;;    '.emacs.desktop'.
;;
;;    This really is no very well-designed piece of software. I rather
;;    'hacked' it when I wanted it. So there. Because of this
;;    'quickie' nature, and given the fact that I know a little more
;;    about Emacs-Lisp programming now than I did when I wrote this,
;;    probably a lot of the things in here could be improved upon.
;;
;;    Feel free to do so! And, please, let me know about what you did
;;    with it... Also look at the TODO section below.

;;  @ Install:
;;  ==========
;;
;;   Simply insert the marked line at that position:
;;
;;     (load "desktop")
;;     (desktop-load-default)
;;     (load "desktop-phase") ;; <--- has to be loaded before the buffers are 
;;                            ;;      created...
;;     (desktop-read)
;;
;;   If you've added anything in this space, you have to figure out where
;;   to put the (load ...) yourself. But I don't know what it should conflict
;;   with.
;;

;;  @ Configuration:
;;  ================
;;
;;  The main configuration variable is desktop-buffer-age-limit. It
;;  sets the number of sessions a buffer can live without being
;;  touched. A value of nil tells desktop-phase not to phase out any
;;  buffer.
;;
;;  Set it in your .emacs (or site-lisp.el, if appropriate) as
;;
;;    (setq desktop-buffer-age-limit 2)
;;
;;  This variable is automatically local to any buffer that sets
;;  it. So, to override the global value (as set in .emacs, or in
;;  desktop-phase.el itself), add a line like, for example,
;;
;;    desktop-buffer-age-limit: nil
;;
;;  to the Local Variables section (add one if it none exist) of any
;;  buffer you NEVER want expired.
;;
;;  The default for desktop-buffer-age-limit is 1, so any buffer used
;;  in one session will be present the next session.
;;
;;  Word Of Warning: Always keep in mind that the higher this value,
;;  the more 'junk' buffers will collect in your Emacs.  So, if your
;;  Emacs-sessions are usually rather long (like, entering right after
;;  login and staying till right before logout... Emacs as a login
;;  shell ;-)), you better make this a small value.
;;
;;  Finally, if you want to have the old desktop.el behavior (never
;;  forget anything except when explicitly 'C-x k'-ed), set the
;;  global value to nil.
;;
;;  If you want to forget a buffer immediately (never carry it over to
;;  the next session), set its age limit to 0.

;;  TODO: 
;;  =====
;;  There's still soooo much we can do with this. (Take it, Milo!)
;;  This list is just stuff I scribbled as it came to me... so if you
;;  have Qs about something, ask me.
;;
;;  ###COMMENT START###
;;  ###BEGIN INSERT insert "TODO" (start-after "desktop\\.el desktop-phase\\.el") (end-before "[^ \t\n]") (offset ";;") (time 34 51 5 19 5 1997 1 t 7200)###
;;
;;  Have some desktop.el ideas in here, too... I'll integrate desktop.el
;;  and -phase.el soon... I hope. A little pleading might help ;-)
;;
;;  - with buffer-displayed-count: kill the ages alist... every buffer
;;    will have a local var `desktop-buffer-age', which is simply saved
;;    via desktop-locals-to-save. Of course this means the incrementing
;;    of these requires (set-buffer), but we need this anyway for
;;    desktop-buffers-not-to-save (and for getting at
;;    buffer-displayed-count ;-))
;;
;;  + have it automatically pop a buffer associated with another
;;    buffer... like DMS could use.  DONE: there's a list,
;;    desktop-real-buffer-to-pop-hooks, whose elements are functions
;;    telling us the 'real' buffer associated with the buffer in
;;    question. Or nil, if the fn doesn't feel responsible ;_)
;;
;;  - maybe have desktop-buffer-age-limit depend on the real age of a
;;    buffer, in days... and/or make it depending on the size of a file,
;;    like, the larger the file, the sooner the buffer is phased out.
;;
;;  - desktop.el could have a buffer-size-limit... so's to not keep
;;    files larger than this threshold. Like, when I load an RMAIL file
;;    of >10M... DON'T KEEP THIS, PLEASE!
;;
;;  -H remove non-saved buffers (like, all "*...*" ones)
;;
;;  -T install elisp-fixes patch... but modify the renaming, in that we
;;     don't rename when the buffer ended with <n>, and now doesn't!
;;
;;  -T integrate into desktop.el (suggested by RMS)
;;
;;  -H work ~/elisp/emacs-state.el into desktop, that walk-windows,
;;     especially! Or *is* this necessary? Anyone listening and tell me?
;;     (I di experiment with saving the window layout... could be done,
;;     but is complicated... is it *needed*?)
;;
;;  -T! fix desktop-phase so it uses 19.35s (20?)
;;      buffer-displayed-count... best make it so that it uses it if
;;      available. Problem is: the advices are needed for pre-19.35, but
;;      shouldn't even be installed for 19.35. (load) a separate file,
;;      maybe?
;;
;;  -Q wouldn't a buffer-accessed-count var be necessary, or at least be
;;     better?
;;
;;  - what's with real-buffer-to-pop? need a hook for that... or do
;;    'linked buffers'... but that's rather deep, right? NO!!! We just
;;    use the -count of X for Y (real-buffer-to-take-count-from...
;;    whatta name!)
;;
;;  - it's not enough to just throw buffers away after N sessions... if
;;    I use a buffer very often, but then don't use it for one or two
;;    sessions, I might still use it very often the next session. And it
;;    has higher prio than a buffer I use every session, but very
;;    rarely. Oh, and the reload/rebuild time is an important factor
;;    too.
;;
;;  -Q made desktop-buffer-age-limit buffer local (auto). Now, how about
;;     buffers that cannot have Local Variables?
;;
;;     Idea: make a desktop-buffer-age-limit-list, where elts are
;;     (REGEXP . LIMIT) pairs. A buffer whose name matches REGEXP is
;;     considered having desktop-buffer-age-limit of LIMIT.
;;
;;     And, because Emacs' regexps are pretty limited (sorely lacking: a
;;     NOT operator), we could allow not just regexp for REGEXP, but
;;     fns, too... and will do.
;;
;;  -I desktop-memory-limit: if this is exceeded by the set of
;;     buffers to save, kill the least often used ones... (and factor
;;     in the size??? Like, 'kill one 10M buffer, and we're under it?
;;     do it!'
;;
;;  -T desktop.el: post desktop-buffers-not-to-save-hook hack.
;;
;;     desktop-buffers-not-to-save-hook 
;;
;;     If it is a symbol, the function bound to it is executed with the
;;     buffer to be tested as current-buffer, and returns t if this
;;     buffer is NOT to save.
;;
;;     If it is a list, the list is traversed (with mapc) and every item
;;     is treated as above (so, you can simply add your function to this
;;     list). Any regexp-match or any function returning t terminates.
;;
;;     After trying the following for Martin's request, I found that we
;;     *might* run into awful problems with code that assumes
;;     desktop-buffers-not-to-save to be a regexp... if we initially
;;     have a regexp, but then make it a list, a subsequent 'concat' by
;;     old code (to add something to the regexp string) will fail, and
;;     horribly at that. So, I changed it to
;;     desktop-buffers-not-to-save-list, with
;;     desktop-buffers-not-to-save being a string (but an empty one)
;;     that is just considered a part of the list by our code. One
;;     question raises itself, then: when do we check that regexp, after
;;     or before the list?
;;
;;     Martin's problem:
;;
;;       (setq desktop-buffers-not-to-save-hook
;;	     (append desktop-buffers-not-to-save-hook
;;		     '(desktop-ignore-non-existing-dirs)))
;;
;;       (defun desktop-ignore-non-existing-dirs ()
;;	 (if (not (eq major-mode 'dired-mode))
;;	     nil
;;	   ;; okay, this is a dired buffer... check whether the directory exists
;;           (not (file-directory-p dired-directory))))
;;
;;  ###END INSERT insert###
;;
;; End:
;; 

;;; Code:

(require 'assoc)  ; hasn't been too useful... can/will probably
		  ; go... when I get around to clean this sucker up
		  ; ;-)
(require 'advice)
(require 'desktop)
(require 'cl)

(add-hook 'after-init-hook 'desktop-activate-phase-out-extension)
(add-hook 'desktop-save-hook 'desktop-phase-out-buffers)

;; the age has to be saved... and the age limit, too (so it keeps a
;; manual setting... until it is phased out or explicitly killed)
(setq-default desktop-locals-to-save 
	      (append desktop-locals-to-save 
		      '(desktop-buffer-age desktop-buffer-age-limit)))

(defvar desktop-show-logs nil
  "*If true, desktop-phase shows all buffer changes logged, with buffer.")

(defvar desktop-buffer-age-limit 1
  "*Number of sessions a buffer can live unused before it is phased out.
If this is nil, buffers will NEVER be thrown out!
Is automatically local if set in a buffer.")

;; the age limit is local... so some buffers can have longer stays!
(make-variable-buffer-local 'desktop-buffer-age-limit)

;; and of course it's also permanent...
(put 'desktop-buffer-age-limit 'permanent-local t)

(defvar desktop-buffer-age-limit-list nil
  "*List of regexps or functions to determine age limit.
Elements are either (REGEXP . LIMIT) or FUNCTION.
If a REGEXP matches the filename of a buffer, the LIMIT specifies the
age limit.
If a FUNCTION returns non-nil, the age limit is the returned value
\(with t being replaced by nil).
The first matching REGEXP or FUNCTION returning non-nil terminates.")

(defvar desktop-buffer-age 0
  "Age in sessions of current buffer. Automatically local if set.")

;; the age is local, of course...
(make-variable-buffer-local 'desktop-buffer-age)

;; and of course it's also permanent...
(put 'desktop-buffer-age 'permanent-local t)

(defvar desktop-real-buffer-to-pop-hooks nil
  "List of functions to call to find out whether a buffer has a REAL buffer.
Every function returns either nil or the real buffer associated.
First function returning non-nil makes the deal.")

;;(if (not (and (string-match "GNU Emacs" (emacs-version))
;;	      (or (> emacs-major-version 19)
;;		  (and (= emacs-major-version 19)
;;		       (> emacs-minor-version 34)))))
;;    (progn

(defun desktop-activate-phase-out-extension ()
  "Activate desktop phase-out. (load \"desktop-phase\") does this
automatically."
  (message "activating desktop phase-out...")
  (desktop-touch-buffer (current-buffer))
  (ad-enable-regexp "^desktop-log-")
  (ad-activate-regexp "^desktop-log-")
  (message "activating desktop phase-out... done.")
  )

(defadvice pop-to-buffer (around desktop-log-buffer-switch disable)
  "Log buffer switches in " 
  ad-do-it
  (if desktop-show-logs
      (message "pop-to-buffer: %S" (current-buffer)))
  (desktop-touch-buffer (current-buffer))
  )

(defadvice switch-to-buffer (around desktop-log-buffer-switch disable)
  "Log buffer switches"
  ad-do-it
  (if desktop-show-logs
      (message "switch-to-buffer: %S" (current-buffer)))
  (desktop-touch-buffer (current-buffer))
  )

(defadvice display-buffer (around desktop-log-buffer-switch disable)
  "Log buffer switches"
  ad-do-it
  (if desktop-show-logs
      (message "display-buffer: %S" buffer))
  (desktop-touch-buffer buffer)
  )

(defadvice set-window-buffer (around desktop-log-buffer-switch disable)
  "Log buffer switches"
  ad-do-it
  (if desktop-show-logs
      (message "set-window-buffer: %S" buffer))
  (desktop-touch-buffer buffer)
  )

(defun desktop-touch-buffer (buffer)
  "Set age of BUFFER to 0."
  (if desktop-show-logs
      (message "desktop-touch-buffer: %S" buffer))
  (save-excursion
    (set-buffer buffer)
    (setq desktop-buffer-age 0))
  ;; now we check whether this buffer is really some other
  ;; buffer(?). Which is what DMS Edit buffers are. They are in
  ;; fact... but read dms.el... as soon as it's released, that is ;-)
  (let ((temp desktop-real-buffer-to-pop-hooks)
	real-buffer-to-pop)
    ;; go through list of hooks
    (while (and temp
		(not real-buffer-to-pop))
      ;; call a hook. It returns non-nil (a buffer) when there is a
      ;; real buffer.
      (setq real-buffer-to-pop (funcall (car temp) buffer))
      (setq temp (cdr temp)))
    ;; now kill those 'hooks... so we are faster on the recursive exec
    (if real-buffer-to-pop
	(let ((desktop-real-buffer-to-pop-hooks nil))
	  (save-window-excursion
	    (switch-to-buffer real-buffer-to-pop))))))

;; activate maybe -> disables advices
;;))

(defun desktop-buffer-age (&optional buffer)
  (save-excursion
    (if buffer
	(set-buffer buffer))
    ;; FIXME: for buffer-displayed-count, we have to
    ;; reset the age here if -count >0
    desktop-buffer-age))

;; something nice for the cl.el fanatics (like me ;-))
(defsetf desktop-buffer-age (&optional buffer) (val) 
  `(if ,buffer 
       (save-excursion 
	 (set-buffer ,buffer) 
	 (setq desktop-buffer-age ,val)) 
     (setq desktop-buffer-age ,val)))

(defun desktop-buffer-age-limit (&optional buffer)
  (save-excursion
    (if buffer
	(set-buffer buffer))
    (if (local-variable-p 'desktop-buffer-age-limit (current-buffer))
	;; has a local value: take it
	desktop-buffer-age-limit
      ;; go through desktop-buffer-age-limit-list
      (let ((age-limit-list desktop-buffer-age-limit-list)
	    found
	    value)
	(while (and age-limit-list
		    (not found))
	  (let ((head (car age-limit-list)))
	    (cond ((and (consp head)
			(stringp (car head)))
		   (when (string-match (concat "^" (expand-file-name (car head)))
				       (or buffer-file-name
					   (and (eq major-mode 'dired-mode)
						(expand-file-name dired-directory))
					   ""))
		     (setq found t)
		     (setq value (cdr head))))
		  ((and (symbolp head)
			(fboundp head))
		   (setq value (funcall head (desktop-buffer-age)))
		   (setq found (or (not value)
				   (integerp value))))
		  (t
		   (error "Illegal element in desktop-buffer-age-limit-list, %S" head))))
	  (setq age-limit-list (cdr age-limit-list)))
	(if found
	    value
	  desktop-buffer-age-limit)))))

(defun desktop-phase-out-buffers ()
  "Throw out all buffers that are older than desktop-buffer-age-limit."
  ;; now go through buffer list and collect all buffers older than the
  ;; limit in phase-out-buffers.
  (let ((buffers (buffer-list))
	phase-out-buffers)
    (while buffers
      (let* ((buffer (car buffers))
	     (buffer-name (buffer-name buffer))
	     (age-limit (desktop-buffer-age-limit buffer))
	     ;; FIXME: for buffer-displayed-count, we have to
	     ;; reset the age here if -count >0
	     (age (incf (desktop-buffer-age buffer))))
       	(if (and age-limit
		 (> age age-limit))
	    (setq phase-out-buffers (cons buffer phase-out-buffers))))
      (setq buffers (cdr buffers)))
    ;; now kill the old suckers...
    (mapcar
     (lambda (elem)
       (kill-buffer elem))
     phase-out-buffers))
  ;; we phased out the buffers... they are killed, so they can't be
  ;; saved by desktop
  )

(defun desktop-show-buffer-ages ()
  "Show all buffers logged with their ages in \"*Buffer Ages*\"."
  (interactive)    
  (let ((outbuffer (get-buffer-create "*Buffer Ages*"))
	(line-format "%"))
    (save-excursion 
      (set-buffer outbuffer)
      (erase-buffer)
      (princ "Buffer                Age\n" outbuffer)
      (princ "------                ---\n" outbuffer) 
      ;;sort the alist on age!!!!!
      (let (ages)
	(mapc (function (lambda (buffer)
			  (save-excursion
			    (set-buffer buffer)
			    (if (desktop-save-buffer-p buffer-file-name
						       (buffer-name)
						       major-mode)
				(setq ages (cons `(,desktop-buffer-age
						   ,(buffer-name buffer)
						   ,(desktop-buffer-age-limit))
						 ages))))))
	      (buffer-list))
	(setq ages
	      (sort ages
		    (lambda (x y)
		      (or (< (car x) (car y))
			  (string-lessp (second x) (second y))))))
	(mapcar (lambda (x) 

		  (princ (format "%-20s  %3d%s (%S)\n" 
				 (second x)
				 (first x)
				 (if (and (third x)
					  (>= (first x) (third x)))
				     "*"
				   " ")
				 (third x))
			 outbuffer))
		ages))
      (beginning-of-buffer))
    (display-buffer outbuffer)))

(provide 'desktop-phase)


;;; desktop-phase.el ends here
