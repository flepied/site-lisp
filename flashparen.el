;;; flashparen.el --- flash matching parens a la Zmacs

;; Copyright (C) 1995, 1997, 1999 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: extensions
;; Status: Works in Emacs 19 and later
;; Created: 1995-03-03

;; $Id: flashparen.el,v 1.12 1999/10/25 21:12:54 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Loading this makes emacs's paren blinking behavior more closely
;; approximate the behavior of Zmacs.  It should work under X or on ascii
;; terminals.

;; To use this program, load this file and do: (flash-matching-mode 1)

;;; Code:

;; Use idle timers if available in the version of emacs running.
(or (featurep 'timer)
    (load "timer" t))

(defgroup flashparen nil
  "flash matching parens a la Zmacs"
  :group 'extensions
  :group 'editing)

(defcustom flash-matching-mode nil
  "*If non-nil, then flash corresponding matching character on display.
It's best to call the function of the same name, since there are other
things to be done by side effect when enabling this feature."
  :type 'boolean
  :group 'flashparen)

(defcustom flash-matching-delay
  (cond (window-system 0.2)
        ((> baud-rate 19200) 0.2)
        ((>= baud-rate 9600) 0.5)
        (t 1))
  "Interval (in seconds) for flash delay.
This number may be a floating-point number in instances of emacs that
support floating point arguments to `sit-for'."
  :type 'number
  :group 'flashparen)

;; No user options below here.

;; This is not a customizable user option because the correct value it
;; should take depends on window system and version details.
(defvar flash-matching-method
  (cond ((or window-system
             (string-lessp "20.5" emacs-version))
         'flash-matching-do-flash-with-overlay)
        (t
         'flash-matching-do-flash-with-buffer-modification))
  "Method used to do flashing.")

;; Idle timers are supported in Emacs 19.31 and later.
(defconst flash-use-idle-timer-p (fboundp 'run-with-idle-timer))

;; timer object, if using idle timers
(defvar flash-timer nil)

;; idle time delay currently in use by timer.
;; This is used to determine if flash-matching-delay is changed by the user.
(defvar flash-matching-current-delay flash-matching-delay)


;;;###autoload
(defun flash-matching-mode (&optional prefix)
  "Enable or disable flashing-parenthesis mode.

If called interactively with no prefix argument, toggle current condition
of the mode.
If called with a positive or negative prefix argument, enable or disable
the mode, respectively."
  (interactive "P")

  (cond (flash-use-idle-timer-p
         (add-hook 'post-command-hook 'flash-schedule-timer))
        (t
         ;; Use post-command-idle-hook if defined, otherwise use
         ;; post-command-hook.  The former is only proper to use in Emacs
         ;; 19.30; that is the first version in which it appeared, but it
         ;; was obsolesced by idle timers in Emacs 19.31.
         (let* ((hook (if (boundp 'post-command-idle-hook)
                          'post-command-idle-hook
                        'post-command-hook))
                (h (memq 'flash-matching-char (symbol-value hook))))
           (cond ((null h)
                  (add-hook hook 'flash-matching-char 'append))
                 ((cdr h)
                  (remove-hook hook 'flash-matching-char)
                  (add-hook hook 'flash-matching-char 'append))))))

  (setq flash-matching-mode (if prefix
                                (>= (prefix-numeric-value prefix) 0)
                              (not flash-matching-mode)))

  (and (interactive-p)
       (if flash-matching-mode
           (message "flash-matching-mode is enabled")
         (message "flash-matching-mode is disabled")))
  flash-matching-mode)

;; Idle timers are part of Emacs 19.31 and later.
(defun flash-schedule-timer ()
  (or (and flash-timer
           (memq flash-timer timer-idle-list))
      (setq flash-timer
            (run-with-idle-timer flash-matching-delay t
                                 'flash-matching-char)))

  ;; If user has changed the idle delay, update the timer.
  (cond ((not (= flash-matching-delay flash-matching-current-delay))
         (setq flash-matching-current-delay flash-matching-delay)
         (timer-set-idle-time flash-timer flash-matching-delay t))))


;; Verify that an even number of quoting characters precede char at point.
(defsubst flash-matching-even-quoting-p (point)
  (let ((p (point)))
    (if (= point (point-min))
        t
      (= 1 (logand 1 (- point
                        (progn
                          (goto-char point)
                          (forward-char -1)
                          (skip-syntax-backward "/\\" (point-min))
                          (prog1
                              (point)
                            (goto-char p)))))))))

(defun flash-matching-char ()
  (and flash-matching-mode

       ;; keyboard macros run a sequence of interactive commands, each one
       ;; of which will cause a call to post-command-hook; so as long as
       ;; the keyboard macro is still executing, do nothing.
       (not executing-kbd-macro)

       ;; prefix args do strange things with commands; it seems that
       ;; running post-command-hook after invoking one of these is delayed
       ;; until the command is finished, then the hook is run twice.
       ;; It's undesirable to wait for user input twice before returning to
       ;; the top command loop, so skip this the first time.
       (not (memq (if flash-use-idle-timer-p last-command this-command)
                  '(digit-argument universal-argument)))

       (let* ((saved-point (point))
              (cho (char-after saved-point))
              (chc (char-after (1- saved-point)))
              ch)
         (cond
          ((or (and (numberp cho)
                    (= (char-syntax cho) ?\()
                    (< saved-point (window-end))
                    (flash-matching-even-quoting-p saved-point)
                    (setq ch cho))
               (and (numberp chc)
                    (= (char-syntax chc) ?\))
                    (> saved-point (window-start))
                    (flash-matching-even-quoting-p saved-point)
                    (setq ch chc)))

           (let ((parse-sexp-ignore-comments t)
                 ;; this beginning of line is not necessarily the same as
                 ;; the one of the matching char `line-beg', below.
                 (bol-point (progn
                              (beginning-of-line)
                              (point)))
                 match-point)

             ;; should be at bol now
             ;; If we're inside a comment already, turn off ignoring comments.
             (and comment-start
                  (looking-at (concat "^[ \t]*" (regexp-quote comment-start)))
                  (setq parse-sexp-ignore-comments nil))

             ;; Find matching paren position, but don't search any further
             ;; than the visible window.
             (save-restriction
               (condition-case ()
                   (progn
                     (narrow-to-region (window-start) (window-end))
                     (cond
                      ((= (char-syntax ch) ?\()
                       (setq match-point (1- (scan-sexps saved-point 1))))
                      (t
                       (setq match-point (scan-sexps saved-point -1)))))
                 (error nil)))

             ;; Matched char must be the corresponding character for the
             ;; char at the saved point, not just another paired delimiter.
             ;; This can happen when parens and brackets are mismatched,
             ;; for example.  Also don't be fooled by things in an
             ;; open/close syntax class but with no defined matching
             ;; character.
             (and match-point
                  (flashparen-matching-paren ch)
                  (not (= (char-after match-point)
                          (flashparen-matching-paren ch)))
                  (setq match-point nil))

             ;; match char must be horizontally visible on display.
             ;; Unfortunately we cannot just use pos-visible-in-window-p
             ;; since that returns t for things that are actually off the
             ;; display horizontally.
             (and truncate-lines
                  match-point
                  (let ((window-hstart (window-hscroll))
                        (match-column (progn
                                        (goto-char match-point)
                                        (current-column))))
                    (if (or (< match-column window-hstart)
                            (> match-column (+ window-hstart (window-width))))
                        (setq match-point nil))))

             (goto-char saved-point)
             (cond (match-point
                    ;; I added this to remove messages left over from
                    ;; blink-matching-open, but it also causes messages
                    ;; returned by eval-expression, etc. not to appear if
                    ;; point is right after a sexp, which is too annoying.
                    ;;(message nil)
                    (funcall flash-matching-method saved-point match-point))
                   (t
                    (and chc
                         (= (char-syntax chc) ?\))
                         ;; blink-matching-open can sometimes signal an
                         ;; error if the function name is outside of a
                         ;; narrowed region---this can happen in C, perl,
                         ;; and other languages where the function label is
                         ;; outside the starting block character, depending
                         ;; on how one's narrow-to-defun function is defined.
                         (condition-case ()
                             (blink-matching-open)
                           (error nil)))))))))))

(defun flash-matching-do-flash-with-overlay (opoint mpoint)
  (let* ((flash-matching-visible-p nil)
         (ovl (make-overlay mpoint (1+ mpoint)))
         (background-color (flash-matching-background-at-pos mpoint))
         (face-props (list (cons 'foreground-color background-color))))
    (unwind-protect
        (while (and (= (point) opoint)
                    (sit-for flash-matching-delay))
          (overlay-put ovl 'face (if flash-matching-visible-p nil face-props))
          (setq flash-matching-visible-p (not flash-matching-visible-p)))
      (delete-overlay ovl))))

;; Emacs 20.4 and earlier cannot display overlay properties on tty frames.
;; This alternative method is fraught with peril.
(defun flash-matching-do-flash-with-buffer-modification
  (flash-matching-opoint flash-matching-mpoint)
  ;; Deactivate the mark now if not using idle timers and deactivate-mark
  ;; is set in transient mark mode.  Normally the command loop does this
  ;; itself, but if this function is run from post-command-hook,
  ;; deactivation is delayed and causes noticable, undesirable effects on
  ;; the display.  The only time I've noticed this to be of consequence is
  ;; when point is right before a sexp and you insert a character.
  ;; Otherwise, this function doesn't get called again because after
  ;; modifying the buffer, point is no longer at the beginning or end of a
  ;; sexp.
  (and flash-use-idle-timer-p
       transient-mark-mode
       deactivate-mark
       (deactivate-mark))

  (let* ((modp (buffer-modified-p))
         (buffer-file-name buffer-file-name)
         (buffer-auto-save-file-name buffer-auto-save-file-name)
         (auto-save-hook (and (boundp 'auto-save-hook)
                              auto-save-hook))
         ;; Don't make any undo records while flashing.
         ;; If this is nil, new undo records are appended.
         ;; Setting it to t avoids consing any records at all.
         (buffer-undo-list t)
         (before-change-function nil)
         (after-change-function nil)
         (before-change-functions nil)
         (after-change-functions nil)
         ;; buffer modification messes with transient mark mode.
         (deactivate-mark nil)
         ;; These variables have long names because they may be referenced
         ;; by a function in the auto-save-hook even if the current buffer
         ;; isn't this one (e.g. because a process filter was running at the
         ;; time).
         (flash-matching-buffer (current-buffer))
         (flash-matching-char (buffer-substring flash-matching-mpoint
                                                (1+ flash-matching-mpoint)))
         (flash-char-props (and (fboundp 'text-properties-at)
                                (text-properties-at 0 flash-matching-char)))
         (flash-replace-char (copy-sequence " "))
         (flash-matching-visible-p t))
    (cond
     ((null buffer-file-name))
     (modp
      ;; If buffer is already modified, do not try to disable locking or
      ;; autosaving, but make sure flashed char is in the buffer exactly
      ;; when autosaving occurs.
      (add-hook 'auto-save-hook
                (function
                 (lambda ()
                   (or flash-matching-visible-p
                       (save-excursion
                         (set-buffer flash-matching-buffer)
                         (let ((buffer-read-only nil)
                               (inhibit-read-only t))
                           (goto-char flash-matching-mpoint)
                           (insert-before-markers-and-inherit
                            flash-matching-char)
                           (forward-char -1)
                           (delete-char -1)
                           (setq flash-matching-visible-p t)
                           (goto-char flash-matching-opoint))))))))
     (t
      ;; Defeat file locking.  Don't try this at home, kids!
      (setq buffer-file-name nil)
      (setq buffer-auto-save-file-name nil)))

    ;; We insert-before-markers-and-inherit one char after the one to
    ;; delete, just in case things like window-start, process-mark,
    ;; etc. are at the point of interest.
    ;; Make sure points are markers, in case process output
    ;; moves them.
    (setq flash-matching-mpoint
          (flash-make-marker (1+ flash-matching-mpoint) nil t))
    (setq flash-matching-opoint
          (flash-make-marker flash-matching-opoint nil t))
    (goto-char flash-matching-opoint)

    ;; Copy text properties of original string to replacement string.
    ;; We need to do this because insert-before-markers-and-inherit does
    ;; not work with rear-nonsticky properties.
    (and flash-char-props
         (set-text-properties 0 1 flash-char-props flash-replace-char))

    (unwind-protect
        (let ((orig-buffer-read-only buffer-read-only)
              (buffer-read-only buffer-read-only)
              (inhibit-read-only t))
          (while (and (= (point) flash-matching-opoint)
                      (sit-for flash-matching-delay))
            (setq buffer-read-only nil)
            (goto-char flash-matching-mpoint)
            ;; Insert char before deleting existing one, to avoid
            ;; complications having to do with overlays and text
            ;; properties on a region.
            (if flash-matching-visible-p
                (insert-before-markers-and-inherit flash-replace-char)
              (insert-before-markers-and-inherit flash-matching-char))
            (forward-char -1)
            (delete-char -1)
            (setq flash-matching-visible-p
                  (not flash-matching-visible-p))
            (or modp (set-buffer-modified-p modp))
            (setq buffer-read-only orig-buffer-read-only)
            (goto-char flash-matching-opoint)))
      (or flash-matching-visible-p
          (let ((buffer-read-only nil)
                (inhibit-read-only t))
            (goto-char flash-matching-mpoint)
            (insert-before-markers-and-inherit flash-matching-char)
            (forward-char -1)
            (delete-char -1)
            (or modp (set-buffer-modified-p modp))
            (goto-char flash-matching-opoint))))))


(defun flash-matching-background-at-pos (pos)
  (let ((prop (get-char-property pos 'face)))
    (cond ((and prop
                (symbolp prop)
                (face-background prop)))
          ((cdr (assq 'background-color prop)))
          ((and (setq prop (face-background 'default))
                (not (string= prop ""))
                prop))
          ((cdr (assq 'background-color (frame-parameters))))
          (t "black"))))

;; Copy existing marker, or make a new one from point.
;; Emacs 19.30 and later can create markers which are advanced if text is
;; inserted before them, without needing to call insert-before-markers
;; explicitly.
(defun flash-make-marker (pos &optional buffer insertion-type)
  (let ((new-marker nil))
    (cond ((markerp pos)
           (setq new-marker (copy-marker pos))
           (and buffer
                (set-marker new-marker (marker-position pos) buffer)))
          (t
           (setq new-marker (make-marker))
           (set-marker new-marker pos buffer)))
    (and (fboundp 'set-marker-insertion-type)
         (set-marker-insertion-type new-marker insertion-type))
    new-marker))

;; matching-paren wasn't defined in emacs until version 19.26.
(if (fboundp 'matching-paren)
    (defalias 'flashparen-matching-paren 'matching-paren)
  (defun flashparen-matching-paren (c)
    (and (memq (char-syntax c) '(?\( ?\)))
         (lsh (aref (syntax-table) c) -8))))

(provide 'flashparen)

;;; flashparen.el ends here

