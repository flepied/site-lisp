;; 30color.el --- 
;;
;; Copyright (C) 1996, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: color.el,v 1.3 2001-10-22 16:06:10 flepied Exp $
;; Keywords: 
;;

(cfg-require 'base)

;;=============================================================================
;; Autoloaded section.
;;=============================================================================

(autoload 'font-lock-fontify-command "refontify"
  "Fontifies according to the value of font-lock-auto-fontify, or the
prefix argument if that is specified.
\t\\[font-lock-fontify-command]\t\tfontify according to font-lock-auto-fontify
\t^U \\[font-lock-fontify-command]\tfontify entire buffer
\t^U - \\[font-lock-fontify-command]\tfontify visible portion of buffer
\t^U n \\[font-lock-fontify-command]\tfontify n lines to either side of point" t)


;;;###

;;;***

;;;### (autoloads (blank-mode-off blank-mode-on blank-mode blank-mode-customize) "blank-mode" "../blank-mode.el" (14451 466))
;;; Generated autoloads from ../blank-mode.el

(autoload (quote blank-mode-customize) "blank-mode" "\
Customize blank-mode options." t nil)

(autoload (quote blank-mode) "blank-mode" "\
Toggle blank visualization.

If ARG is null, toggle blank visualization.
If ARG is a number and is greater than zero, turn on visualization; otherwise,
turn off visualization." t nil)

(autoload (quote blank-mode-on) "blank-mode" "\
Turn on blank visualization." t nil)

(autoload (quote blank-mode-off) "blank-mode" "\
Turn off blank visualization." t nil)

;;;***

;;;### (autoloads (flash-matching-mode) "flashparen" "../flashparen.el" (13057 54182))
;;; Generated autoloads from ../flashparen.el

(autoload (quote flash-matching-mode) "flashparen" "\
*If non-nil, then flash corresponding matching character on display." t nil)

;;;***

;;;### (autoloads (ps-hilit-extend-face ps-hilit-extend-face-list ps-hilit-new-faces ps-hilit-spool-region-with-faces ps-hilit-print-region-with-faces ps-hilit-spool-buffer-with-faces ps-hilit-print-buffer-with-faces) "ps-hilit" "../ps-hilit.el" (13205 401))
;;; Generated autoloads from ../ps-hilit.el

;; (defalias (quote ps-hilit-print-buffer) (quote ps-print-buffer))
;; 
;; (defalias (quote ps-hilit-spool-buffer) (quote ps-spool-buffer))
;; 
;; (defalias (quote ps-hilit-print-region) (quote ps-print-region))
;; 
;; (defalias (quote ps-hilit-spool-region) (quote ps-spool-region))
;; 
;; (autoload (quote ps-hilit-print-buffer-with-faces) "ps-hilit" "\
;; Generate and print a PostScript image of the buffer.
;; 
;; Like `ps-print-buffer', but includes font, color, and underline
;; information in the generated image." t nil)
;; 
;; (autoload (quote ps-hilit-spool-buffer-with-faces) "ps-hilit" "\
;; Generate and spool a PostScript image of the buffer.
;; 
;; Like `ps-spool-buffer', but includes font, color, and underline
;; information in the generated image.
;; 
;; Use the command `ps-despool' to send the spooled images to the printer." t nil)
;; 
;; (autoload (quote ps-hilit-print-region-with-faces) "ps-hilit" "\
;; Generate and print a PostScript image of the region.
;; 
;; Like `ps-print-region', but includes font, color, and underline
;; information in the generated image." t nil)
;; 
;; (autoload (quote ps-hilit-spool-region-with-faces) "ps-hilit" "\
;; Generate a PostScript image of the region and spool locally.
;; 
;; Like `ps-spool-region', but includes font, color, and underline
;; information in the generated image.
;; 
;; Use the command `ps-despool' to send the spooled images to the printer." t nil)
;; 
;; (autoload (quote ps-hilit-new-faces) "ps-hilit" "\
;; Create new faces from FACE-LIST.
;; 
;; The FACE-LIST elements should be like a `font-lock-face-attributes' element,
;; so that the face name is the first item in the list.
;; 
;; The FACE-LIST elements are added to `font-lock-face-attributes'.
;; If optional OVERRIDE-P is non-nil, faces that already exist in
;; `font-lock-face-attributes' are overrided.
;; 
;; The optional argument FACE-EXTENSION is a list whose elements are:
;; 
;;    (FACE-NAME EXTENSION...)
;; 
;; FACE-NAME is a face name that must be in `font-lock-face-attributes';
;; otherwise, this element is ignored.
;; 
;; EXTENSION is some valid extension symbol (see `ps-hilit-extend-face').
;; 
;; FACE-EXTENSION extends the faces in `ps-hilit-face-extension-alist'.
;; 
;; See `font-lock-make-face'." nil nil)
;; 
;; (autoload (quote ps-hilit-extend-face-list) "ps-hilit" "\
;; Extend face in `ps-hilit-face-extension-alist', if it exists.
;; 
;; The elements in FACE-EXTENSION-LIST is like those for `ps-hilit-extend-face'.
;; 
;; See `ps-hilit-extend-face' for documentation." nil nil)
;; 
;; (autoload (quote ps-hilit-extend-face) "ps-hilit" "\
;; Extend face in `ps-hilit-face-extension-alist'.
;; 
;; The elements of FACE-EXTENSION list have the form:
;; 
;;    (FACE-NAME EXTENSION...)
;; 
;; FACE-NAME is a face name symbol.
;; 
;; EXTENSION is one of the following symbols:
;;    bold      - use bold font.
;;    italic    - use italic font.
;;    underline - put a line under text.
;;    strikeout - like underline, but the line is in middle of text.
;;    overline  - like underline, but the line is over the text.
;;    shadow    - text will have a shadow.
;;    box       - text will be surrounded by a box.
;;    outline   - only the text border font will be printed.
;; 
;; If EXTENSION is any other symbol, it is ignored." nil nil)

;;;***


;;=============================================================================
;; Configuration section.
;;=============================================================================

;;=============================================================================
;; Coloriage suivant le mode par font-lock.
;;=============================================================================
(if (or window-system is-xemacs (>= emacs-major-version 21))
    (progn
      (setq font-lock-support-mode 'lazy-lock-mode)
      (if (fboundp 'global-font-lock-mode)
	  (global-font-lock-mode 1 t)
	(add-hook 'find-file-hooks (function (lambda() (turn-on-font-lock))) t))

;;      (and (not is-xemacs)
;;	   (require 'font-lock-menu))
      (add-hook 'font-lock-mode-hook
		(function (lambda()
			    (local-set-key [?\C-l] 'fontify-recenter))))
      (setq-default font-lock-maximum-decoration t
		    font-lock-maximum-size nil	; size is irrelevant
		    )
     )
)

(defun fontify-recenter (arg)
  "Recenter and refontify a buffer"
  (interactive "P")
  (recenter arg)
  (font-lock-fontify-command arg))

;;=============================================================================
;; Parenthèses clignotantes en mode terminal et coloriées sous X.
;;=============================================================================
(setq show-paren-mode t)
(if (not (or is-xemacs window-system))
    (flash-matching-mode 1)
  (load "paren" 1)
  )

;;; 30color.el ends here
