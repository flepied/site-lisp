;; 50calendar.el --- 
;;
;; Copyright (C) 1996, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: calendar.el,v 1.1 1999-12-09 07:34:42 flepied Exp $
;; Keywords: 
;;

;;=============================================================================
;; Autoloaded section.
;;=============================================================================
(autoload 'compute-cra "cra"
  "Compute the number of worked days in the current month" t)

(autoload 'edit-diary-mode "edit-diary-mode"
  "Major mode for editing .diary files.
	\\{edit-diary-mode-map}" t)

(autoload 'todo-show "todo-mode" "Show TODO list." t)
(autoload 'todo-cmd-inst "todo-mode" "Insert new TODO list entry." t)

(autoload 'steno-view "steno"
  "Show a steno pad and start a new entry.
If called interactively, the user will be prompted for the pad name.
For more information, see steno-mode." t)


;;;###

;;;***

;;;### (autoloads (timeclock-when-to-leave-string timeclock-workday-elapsed-string timeclock-workday-remaining-string timeclock-reread-log timeclock-query-out timeclock-change timeclock-out timeclock-in timeclock-modeline-display) "timeclock" "../timeclock.el" (14411 21548))
;;; Generated autoloads from ../timeclock.el

(autoload (quote timeclock-modeline-display) "timeclock" "\
Toggle display of the amount of time left today in the modeline.
If `timeclock-use-display-time' is non-nil, the modeline will be
updated whenever the time display is updated.  Otherwise, the
timeclock will use its own sixty second timer to do its updating.
With prefix ARG, turn modeline display on if and only if ARG is
positive.  Returns the new status of timeclock modeline display
\(non-nil means on)." t nil)

(autoload (quote timeclock-in) "timeclock" "\
Clock in, recording the current time moment in the timelog.
With a numeric prefix arg, record the fact that today has only that
many hours in it to be worked.  If arg is a non-numeric prefix arg
\(non-nil, but not a number), then 0 is assumed.  *If this function is
not being called interactively, ARG should be the number of _seconds_
worked today*.  This feature only has effect for the first clock in." t nil)

(autoload (quote timeclock-out) "timeclock" "\
Clock out, recording the current time moment in the timelog.
With a prefix ARG, consider PROJECT as having been completed.  PROJECT
is the name of the project you're clocking out of.  If PROJECT is nil,
and FIND-PROJECT is non-nil, call the function stored in
`timeclock-get-project-function' to discover the name of the current
project.  This function will also be called if PROJECT is nil and
`timeclock-out' is called interactively." t nil)

(autoload (quote timeclock-change) "timeclock" "\
Change to working on a different project, by clocking in then out.
With a prefix ARG, consider the previous project as having been
finished at the time of changeover.  PROJECT is the name of the last
project you were working on." t nil)

(autoload (quote timeclock-query-out) "timeclock" "\
Ask the user before clocking out.
This is a useful function for adding to `kill-emacs-hook'." nil nil)

(autoload (quote timeclock-reread-log) "timeclock" "\
Re-read the timeclock, to account for external changes.
Returns the new value of `timeclock-discrepancy'." t nil)

(autoload (quote timeclock-workday-remaining-string) "timeclock" "\
Return a string representing the amount of time left today.
Display second resolution if SHOW-SECONDS is non-nil.  If TODAY-ONLY
is non-nil, the display will be relative only to time worked today.
See `timeclock-relative' for more information about the meaning of
\"relative to today\"." t nil)

(autoload (quote timeclock-workday-elapsed-string) "timeclock" "\
Return a string representing the amount of time worked today.
Display seconds resolution if SHOW-SECONDS is non-nil.  If RELATIVE is
non-nil, the amount returned will be relative to past time worked." t nil)

(autoload (quote timeclock-when-to-leave-string) "timeclock" "\
Return a string representing at what time the workday ends today.
This string is relative to the value of `timeclock-workday'.  If
NO-MESSAGE is non-nil, no messages will be displayed in the
minibuffer.  If SHOW-SECONDS is non-nil, the value printed/returned
will include seconds.  If TODAY-ONLY is non-nil, the value returned
will be relative only to the time worked today, and not to past time.
This argument only makes a difference if `timeclock-relative' is
non-nil." t nil)

;;;***

;;;### (autoloads (todo-print todo-top-priorities) "todo-mode" "todo-mode.el" (13386 65114))
;;; Generated autoloads from todo-mode.el

(autoload (quote todo-top-priorities) "todo-mode" "\
List top priorities for each category.

Number of entries for each category is given by NOF-PRIORITIES which
defaults to 'todo-show-priorities'.

If CATEGORY-PR-PAGE is non-nil, a page separator '^L' is inserted
between each category." t nil)

(autoload (quote todo-print) "todo-mode" "\
Print todo summary using \\[todo-print-function].
If CATEGORY-PR-PAGE is non-nil, a page separator '^L' is inserted
between each category.

Number of entries for each category is given by
'todo-print-priorities'." t nil)

;;;***


;;=============================================================================
;; Configuration section.
;;=============================================================================
(add-hook 'calendar-load-hook (lambda()
				(european-calendar)
				(setq auto-mode-alist
				      (cons (cons (file-name-nondirectory diary-file) 'edit-diary-mode) auto-mode-alist))
;				(define-key calendar-mode-map "C" 'compute-cra)
				))

;(require 'calendar)			; european-calendar n'est pas autoload

;;=============================================================================
;; gestion du calendrier
;;=============================================================================
(defvar french-holidays
  '((holiday-fixed 1 1 "Nouvel an")
    (holiday-float 4 1 1 "Lundi de Paques")
    (holiday-fixed 5 1 "Fête du travail")
    (holiday-fixed 5 8 "Victoire 1945")
    (holiday-float 5 4 3 "Ascension")
    (holiday-float 5 1 -1 "Lundi de Pentecôte")
    (holiday-float 6 0 1 "Fête des mères")
    (holiday-float 6 0 3 "Fête des pères")
    (holiday-fixed 7 14 "Fête Nationale")
    (holiday-fixed 8 15 "Assomption")
    (holiday-fixed 11 1 "Toussaint")
    (holiday-fixed 11 11 "Armistice 1918")
    (holiday-fixed 12 25 "Noël"))
  "French holidays")

(setq view-diary-entries-initially	t
      european-calendar-style		t
      view-calendar-holidays-initially	t
      mark-diary-entries-in-calendar	t
      mark-holidays-in-calendar		t
      view-calendar-holidays-initially	t
      calendar-week-start-day		1
      calendar-latitude			+1
      calendar-longitude		+47
      calendar-holidays			french-holidays
      )

(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'diary-hook 'appt-make-list)
(add-hook 'list-diary-entries-hook 'sort-diary-entries)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

; to lauch calendar use :
(autoload 'calendar-two-frame-setup "cal-x" t)
(autoload 'calendar-one-frame-setup "cal-x" t)

;; todo mode bindings
;(global-set-key "\C-ct" 'todo-show)     ; switch to TODO buffer
(global-set-key "\C-ci" 'todo-cmd-inst) ; insert new item

;; steno bindings
(global-set-key "\C-c\C-s" 'steno-view)

;; timeclock bindings
(define-key ctl-x-map "ti" 'timeclock-in)
(define-key ctl-x-map "to" 'timeclock-out)
(define-key ctl-x-map "tc" 'timeclock-change)
(define-key ctl-x-map "tr" 'timeclock-reread-log)
(define-key ctl-x-map "tu" 'timeclock-update-modeline)
(define-key ctl-x-map "tw" 'timeclock-when-to-leave-string)

;;; 50calendar.el ends here
