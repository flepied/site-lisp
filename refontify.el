;;
;; copy of hilit-auto-rehighlight variable
;;
(defvar font-lock-auto-fontify t
  "* If this is non-nil, then font-lock-fontify-command will
  fontify part or all of the current buffer.  T will fontify the
  whole buffer, a NUMBER will fontify that many lines before and after
  the cursor, and the symbol 'visible' will fontify only the visible
  portion of the current buffer.  This variable is buffer-local.")

(make-variable-buffer-local 'font-lock-auto-fontify)

;;
;; copy of hilit-repaint-command
;;
(defun font-lock-fontify-command (arg)
  "Fontifies according to the value of font-lock-auto-fontify, or the
prefix argument if that is specified.
\t\\[font-lock-fontify-command]\t\tfontify according to font-lock-auto-fontify
\t^U \\[font-lock-fontify-command]\tfontify entire buffer
\t^U - \\[font-lock-fontify-command]\tfontify visible portion of buffer
\t^U n \\[font-lock-fontify-command]\tfontify n lines to either side of point"
  (interactive "P")
  (let (st en quietly)
    (or arg (setq arg font-lock-auto-fontify))
    (cond ((or (eq  arg 'visible) (eq arg '-))
           (setq st (window-start) en (window-end) quietly t))
          ((numberp arg)
           (setq st (save-excursion (forward-line (- arg)) (point))
                 en (save-excursion (forward-line arg) (point))))
          (arg
           (font-lock-fontify-buffer)))
    (if st
          (font-lock-fontify-region st en quietly))))

(defun font-lock-fontify-recenter (arg)
  "re-fontify and recenter"
  (interactive "P")
  (font-lock-fontify-command arg)
  (recenter))

;; ------------------------------------------------------------
;; Examples of usage:
;;
;; Now let's make a more personalized form of turning on font-lock:
;;
(defun my-turn-on-font-lock ()
  "Locally rebind C-S-l to font-lock-fontify-command then turn on font-lock"
  (interactive)
  ;; locally override the hilit19 setting of S-C-l
  (local-set-key [?\C-\S-l] 'font-lock-fontify-command)
  (turn-on-font-lock)
  )


;;
;; and here is an example of using that personalized function in hooks to have
;; font-lock in some modes and hilit19 in others:
;;
;; (cond (window-system
;;        (setq font-lock-maximum-decoration 3)
;;        (add-hook 'c-mode-hook 'my-turn-on-font-lock)
;;        (add-hook 'c++-mode-hook 'my-turn-on-font-lock)
;;        (add-hook 'f90-mode-hook 'my-turn-on-font-lock)
;;        (add-hook 'fortran-mode-hook 'my-turn-on-font-lock)
;;        (setq   hilit-mode-enable-list
;; 	       '(not text-mode c-mode c++-mode f90-mode fortran-mode))
;;        (setq   hilit-background-mode   'dark); comment out for light backgrounds
;;        (setq   hilit-inhibit-rebinding t)    ; Don't want hilit to rebind keys!
;;        (setq   hilit-inhibit-hooks     nil)
;;        (setq   hilit-face-check        nil)  ; for speed if no face modification
;;        (require 'hilit19)
;;        ))
