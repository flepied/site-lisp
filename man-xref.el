;;; man-xref.el --- cross reference selection functions for man mode

;; Author:  Mark Hood <hood@eng.sun.com>
;; @(#)man-xref.el	1.6	

;; This package is an add-on to the man.el that comes with Emacs
;; 19.34.  It renders manpage cross references in bold, sets them up
;; for mouse highlighting, and allows selection via keystrokes or
;; mouse.  All strings matching Man-reference-regexp in the text of
;; the man page are set up, in addition to the ones in the See Also
;; section.

;; See the comment in Man-mouseify-xrefs for information about invoking
;; completion on cross reference strings.

;; To use this package, put something like the following in your Emacs
;; initialization file.  This example causes tab and M-tab to go to
;; the next and previous manual cross references, causes carriage
;; return to display a man page for the reference under point, and
;; allows mouse button 2 to invoke a man page display.

;; (add-hook 'Man-mode-hook
;;	  '(lambda ()
;;	     (Man-mouseify-xrefs)
;;	     (define-key Man-mode-map "\r" 'Man-do-manual-reference)
;;	     (define-key Man-mode-map "\t" 'Man-next-manual-reference)
;;	     (define-key Man-mode-map "\e\t" 'Man-prev-manual-reference)
;;	     (define-key Man-mode-map [mouse-2] 'Man-mouse-manual-reference)
;;	     ))
;;
;; (autoload 'Man-mouseify-xrefs "~/emacs/man-xref")

(defconst Man-word-syntax "w_()")

(defun Man-current-word ()
  "Return word under point and move to the end of the word."
  (let ((s (progn (skip-syntax-backward Man-word-syntax) (point))))
    (skip-syntax-forward Man-word-syntax)
    (buffer-substring s (point))))

(defun Man-prev-word-check-hyphen ()
  "Move to the end of the previous word.  If previous word is hyphenated, go
to the beginning of that word instead and return point; otherwise return nil."
  (let ((s (point)))
    (skip-syntax-backward Man-word-syntax)
    (skip-chars-backward " \t")
    (if (string-equal "-\n" (buffer-substring (- (point) 2) (point)))
	(progn (backward-char)
	       (skip-syntax-backward Man-word-syntax)
	       (point)))))

(defun Man-next-manual-reference ()
  "Move point to the beginning of the next manual reference."
  (interactive)
  (let ((s (point)))
    (re-search-forward (concat "[ \t]" Man-reference-regexp) nil t)
    (if (Man-prev-word-check-hyphen)
	(if (not (> (point) s))
	    ;; point was at the start of a hyphenated xref.
            ;; Man-reference-regexp found the post-hyphen portion of xref and
	    ;; Man-prev-word-check-hyphen returned point to the beginning.
	    (progn
	      (re-search-forward Man-reference-regexp nil t 2)
	      (goto-char (or (Man-prev-word-check-hyphen)
			     (match-beginning 0)))))
      (goto-char (1+ (match-beginning 0))))))

(defun Man-prev-manual-reference ()
  "Move point to the beginning of the previous manual reference."
  (interactive)
  (re-search-backward (concat "[ \t]" Man-reference-regexp) nil t)
  (goto-char (or (Man-prev-word-check-hyphen)
		 (1+ (match-beginning 0)))))

(defun Man-mouseify-xrefs ()
  "Render man cross references in bold font and set up mouse highlighting."
  (let (start end xref)
    (goto-char (point-min))
    (forward-line 1)
    (while (re-search-forward Man-reference-regexp nil t)
      (setq end (match-end 0))
      (setq start (match-beginning 0))
      (setq xref (buffer-substring start end))
      (if (Man-prev-word-check-hyphen)
	  (progn
	    (setq start (point))
	    (setq xref (concat (substring (Man-current-word) 0 -1) xref))))
      (Man-boldify-mouse-region start end)
      ;; (aput 'Man-refpages-alist xref) should be used next to allow the "r"
      ;; command (Man-follow-manual-reference) to recognize the xref and
      ;; generate completions.  However, aput is too slow for huge man pages
      ;; with zillions of xrefs (such as intro(3) under Solaris).
      ;;
      ;; If Man-refpages-alist isn't updated, then the only xrefs recognized
      ;; by Man-follow-manual-reference are the ones listed in the See Also
      ;; section of the man page (clicking or typing a carriage return on any
      ;; xref will still work).
      ;;
      ;; Uncomment the next form to allow the "r" command to work with
      ;; embedded xrefs not listed in the See Also section.  In that case
      ;; Man-refpages-alist may contain duplicate entries, which would then
      ;; show up when invoking completion.
      ;; (setq Man-refpages-alist (cons (list xref) Man-refpages-alist))
      (goto-char end))
    (goto-char (point-min))
    (forward-line 1)))

(defun Man-mouse-manual-reference (mouse)
  "Move point to mouse position and run man on cross reference there."
  (interactive "e")
  (select-window (car (car (cdr mouse))))
  (goto-char (car (cdr (car (cdr mouse)))))
  (Man-do-manual-reference))

(defun Man-do-manual-reference ()
  "Run man on cross reference under point."
  (interactive)
  (save-excursion
    (let ((xref (Man-current-word)))
      (cond ((string-equal "" xref))
	    ((string-equal "-" (substring xref -1))
	     (skip-syntax-forward " ")
	     (setq xref (concat (substring xref 0 -1) (Man-current-word))))
	    ((Man-prev-word-check-hyphen)
	     (setq xref (concat (substring (Man-current-word) 0 -1) xref))))
      (if (string-match Man-reference-regexp xref)
	  (Man-getpage-in-background
	   (Man-translate-references
	    (substring xref (match-beginning 0) (match-end 0))))
	(message "No cross-reference found under point.")))))

(defun Man-boldify-mouse-region (beg end)
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'face 'bold)
    (overlay-put overlay 'mouse-face 'highlight)
    (overlay-put overlay 'hilit t)))

