;;; granny.el --- Colorful display of the `cvs annotate' command

;; Copyright (C) 1997 Martin Lorentzson.

;; Author:  Martin Lorentzson <Martin.Lorentzson@emw.ericsson.se>
;; Version: $Revision: 1.11 $
;; Date: $Date: 1997/03/03 15:33:58 $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to kyle@cs.odu.edu) or from
;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;; 02139, USA.

;;; Commentary:

;; Running `granny' will display the result of the cvs command `cvs
;; annotate' using colors to enhance the age of the line. Red is new
;; (hot) and blue is old (cold).

;; granny-mode is the major mode for the buffer that display the
;; result of the cvs annotate command. The mode specific menu will
;; enable expansion/compression of the time-span in variable
;; `granny-color-map'. Menu elements are defined by the variable
;; `granny-menu-elements'.

;; This was inspired by Granny version 1.0 by J. Gabriel Foster
;; (gabe@sgrail.com)

;;; Installation:

;; (cond (window-system
;; 	  (autoload 'granny "granny"
;;     "Display the result of the cvs annotate command using colors.
;;   New lines are displayed in red, old in blue. To use granny, press
;;   \\[granny] when placed in a file controlled by CVS."
;; 	    t)))

;; You can bind granny to a key like this:

;;  (define-key mode-specific-map "g" 'granny)

;;; Usage:

;; When in a CVS controlled file, use M-x granny.

;;; Code:

;;; User configurable variables

(defvar granny-color-map '(( 26.3672 . "#FF0000")
			   ( 52.7344 . "#FF3800")
			   ( 79.1016 . "#FF7000")
			   (105.4688 . "#FFA800")
			   (131.8359 . "#FFE000")
			   (158.2031 . "#E7FF00")
			   (184.5703 . "#AFFF00")
			   (210.9375 . "#77FF00")
			   (237.3047 . "#3FFF00")
			   (263.6719 . "#07FF00")
			   (290.0391 . "#00FF31")
			   (316.4063 . "#00FF69")
			   (342.7734 . "#00FFA1")
			   (369.1406 . "#00FFD9")
			   (395.5078 . "#00EEFF")
			   (421.8750 . "#00B6FF")
			   (448.2422 . "#007EFF"))
  "*Association list for time and color. Time is given in 2**-16 secs.
Default is eighteen steps using a twenty day increment. ")

(defvar granny-very-old-color "#0046FF"
  "*Color for lines older than CAR of last cons in `granny-color-map'.")

(defvar granny-background "black"
  "*Background color. Default color is used if nil.")

(defvar granny-menu-elements '(2 0.5 0.1 0.01)
  "*Menu elements. List of factors, used to expand/compress the time scale.")

;;;###autoload
(defvar granny-mode-hook nil
  "*Hooks to run when granny-mode is turned on.")

;;; End of user configurable variables


(defvar granny-mode nil
  "Variable indicating if Granny mode is active.")

(defvar granny-mode-map ()
  "Local keymap used for granny-mode.")

;; Syntax Table
(defvar granny-mode-syntax-table nil
  "Syntax table used in granny-mode buffers.")

(defun granny-mode-variables ()
  (if (not granny-mode-syntax-table)
      (progn   (setq granny-mode-syntax-table (make-syntax-table))
	       (set-syntax-table granny-mode-syntax-table)))
  (if (not granny-mode-map)
      (setq granny-mode-map (make-sparse-keymap))))

;;;###autoload
(defun granny-mode ()
  "Major mode for granny buffers.

You can use the mode-specific menu to alter the time-span of the used
colors."
  (interactive)
  (kill-all-local-variables)		; Recommended by RMS.
  (granny-mode-variables)		; This defines various variables.
  (use-local-map granny-mode-map)	; This provides the local keymap.
  (set-syntax-table granny-mode-syntax-table)
  (setq major-mode 'granny-mode)	; This is how `describe-mode'
					;   finds out what to describe.
  (setq mode-name "Granny")		; This goes into the mode line.
  (granny-add-menu)			; Add a menu item
  (if (string-match "XEmacs" emacs-version)
      (easy-menu-add granny-mode-menu))
  (run-hooks 'granny-mode-hook))	; This permits the user to use a
					;   hook to customize the mode.

(require 'easymenu)

(defun granny-menu-list (menu-elements)
  "Construct a menu list from menu-elements."
  (let ((element (car menu-elements)))
    (if (not (eq 'nil menu-elements))
	(cons (vector(concat "Span "
			     (number-to-string 
			      (round (* element 
					(* (granny-car-last-cons granny-color-map) 
					   0.7585))))
			     " days")
		     (list 'granny-display '(get-buffer (buffer-name))
			   (list 'granny-time-span 'granny-color-map element)) t)
	      (granny-menu-list (cdr menu-elements))))))
    

(defun granny-add-menu ()
  "Adds the menu 'Granny' to the menu bar in Granny mode."
  (easy-menu-define granny-mode-menu granny-mode-map 
		    "Menu keymap for Granny mode."
		    (cons "Granny" 
			  (cons ["Default" (granny-display (get-buffer (buffer-name))) t]
				(cons ["------------------" nil nil]
				      (granny-menu-list granny-menu-elements))))))
;;;###autoload
(defun granny()
  "Display the result of the cvs annotate command using colors.
New lines are displayed in red, old in blue. To use granny, press
\\[granny] when placed in a file controlled by CVS."
  (interactive)
  (message "granny...")
  (let ((temp-buffer-name (concat "*cvs annotate " (buffer-name) "*"))
	(temp-buffer-show-function 'granny-display))
    (with-output-to-temp-buffer temp-buffer-name
      (call-process "cvs" nil (get-buffer temp-buffer-name) nil
		    "annotate" (file-name-nondirectory (buffer-file-name)))))
  (message "granny... done"))

(defun granny-car-last-cons (assoc-list)
  "Return car of last cons in assoc-list"
  (if (not (eq nil (cdr assoc-list)))
      (granny-car-last-cons (cdr assoc-list))
    (car (car assoc-list))))

(defun granny-time-span (assoc-list span &optional quantize)
  "Return an association list with span factor applied to the
time-span of assoc-list.  Optionaly quantize to the factor of
quantize."
  ;; Apply span to each car of every cons
  (if (not (eq nil assoc-list)) 
      (append (list (cons (* (car (car assoc-list)) span)
			  (cdr (car assoc-list))))
	      (granny-time-span (nthcdr (cond (quantize) ; optional
					      (1)) ; Default to cdr
					assoc-list) span quantize))))

(defun granny-compcar (threshold &rest args)
  "Test successive cars of ARGS against THRESHOLD.
Return the first cons which CAR is not less than THRESHOLD, nil otherwise"
  ;; If no list is exhausted,
  (if (and (not (memq 'nil args)) (< (car (car (car args))) threshold))
      ;; apply to CARs.
      (apply 'granny-compcar threshold
	     ;; Recurse for rest of elements.
	     (mapcar 'cdr args))
    ;; Return the proper result
    (car (car args))))

(defun granny-display (buffer &optional color-map)
  "Do the granny display in buffer using color-map"

  ;; We need a list of months and their corresponding numbers.
  (let* ((local-month-numbers 
	  '(("Jan" . 1) ("Feb" .  2) ("Mar" .  3) ("Apr" .  4)
	    ("May" . 5) ("Jun" .  6) ("Jul" .  7) ("Aug" .  8) 
	    ("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12)))
	 ;; XEmacs use extents, GNU Emacs overlays.
	 (overlay-or-extent (if (string-match "XEmacs" emacs-version)
				(cons 'make-extent 'set-extent-property)
			      (cons 'make-overlay 'overlay-put)))
	 (make-overlay-or-extent (car overlay-or-extent))
	 (set-property-overlay-or-extent (cdr overlay-or-extent)))

    (set-buffer buffer)
    (display-buffer buffer)
    (if (not granny-mode)		; Turn on granny-mode if not done
	(granny-mode))
    (goto-char (point-min))		; Position at the top of the buffer.
    (while (re-search-forward 
	    "^[0-9]+\\(\.[0-9]+\\)*\\s-+(\\sw+\\s-+\\([0-9]+\\)-\\(\\sw+\\)-\\([0-9]+\\)): "
	    nil t)

      (let* (;; Unfortunately, order is important. match-string will
             ;; be corrupted by extent functions in XEmacs. Access
             ;; string-matches first.
	     (day (string-to-number (match-string 2)))
             (month (cdr (assoc (match-string 3) local-month-numbers)))
	     (year-tmp (string-to-number (match-string 4)))
	     (year (+ (if (> 100 year-tmp) 1900 0) year-tmp)) ; Possible millenium problem
	     (high (- (car (current-time))
		      (car (encode-time 0 0 0 day month year))))
	     (color (cond ((granny-compcar high (cond (color-map)
						      (granny-color-map))))
			  ((cons nil granny-very-old-color))))
	     ;; substring from index 1 to remove any leading `#' in the name
	     (face-name (concat "granny-face-" (substring (cdr color) 1)))
	     ;; Make the face if not done.
	     (face (cond ((intern-soft face-name))
			 ((make-face (intern face-name)))))
	     (point (point))
	     (foo (forward-line 1))
	     (overlay (cond ((if (string-match "XEmacs" emacs-version)
				 (extent-at point)
			       (car (overlays-at point ))))
			    ((apply make-overlay-or-extent point (point) nil)))))

	(if granny-background
	    (set-face-background face granny-background))
	(set-face-foreground face (cdr color))
	(apply set-property-overlay-or-extent overlay
	       'face face nil)))))

(provide 'granny)
;; granny ends here.
