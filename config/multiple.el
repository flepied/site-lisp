;; multiple.el --- 
;;
;; Copyright (C) 1999, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: multiple.el,v 1.1 1999-12-09 07:34:42 flepied Exp $
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
(and window-system
     (not (string-match "^ncd.*" (getenv "DISPLAY")))
     (setq special-display-buffer-names
           (append (list "ChangeLog" "*scheme*" "*igrep*" "*grep*" "*cvs*"
                         "*compilation*" '("*Help*" 50 60)
                         "*Buffer List*" "*W3-WARNINGS*"
                         "*Compile-Log*" "*BBDB*" "*info*"
                         "*Calendar*"
                         )
                   special-display-buffer-names))
     (setq special-display-regexps
           (append (list "*CVS\\|*gud-\\|^\\*[Mm]an" "Diary")
                   special-display-regexps))
     (remove-hook 'same-window-buffer-names "*info*")
     )       

;;; multiple.el ends here
