;;; comicscript.el --- A major mode for editing comic script files.

;; Can help the comic book author format a script more or less in
;; the way Dark Horse Comics likes to see them

;; This file is based on V. L. Simpson's screenplay major mode

;; $Id: screenplay.el,v 0.1.0 2012/07/09 03:27:42 jmk Exp $

;; Copyright (C) 2000, 2001, 2002, 2003, 2004  Vance L. Simpson

;; Author: V. L. Simpson <vls@freeshell.org>
;; Updated: J. M. Kersey <jim.kersey@gmail.com>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(setq debug-on-error 1)

(defconst comicscript-version "0.1.0"
  "Current Emacs Comicscript Mode version number.")
(defconst screenplay-author-name  "V. L. Simpson")
(defconst screenplay-author-email "vls@freeshell.org")
(defconst screenplay-web-page     "http://www.nongnu.org/screenplay/")
(defconst comicscript-web-page     "http://oversimplified.net/comicscript/")
(defconst vls-homepage "http://vls.freeshell.org")
(defconst jmk-homepage "http://oversimplified.net")
(defconst comicscript-bug-address
  "jim.kersey@gmail.com"
  "Bug reports for Comicscript Mode go here.")

(defgroup comicscript nil
  "Comicscript editing."
  :group 'applications
  :link '(emacs-commentary-link :tag "Help" "comicscript"))

;; FIXME: Not supposed to do this but easiest way to handle filling at
;; the moment.  May implement the old re-filling code from old version.
(defcustom comicscript-mode-hook 'auto-fill-mode
  "List of functions to call when entering Comicscript Mode."
  :type 'hook
  :group 'comicscript)

(defcustom comicscript-left-margin 0 "margin" :type 'integer :group 'comicscript)
(defcustom comicscript-right-margin 50 "margin" :type 'integer :group 'comicscript)
(defvar comicscript-dialog-left-margin 10)
(defvar comicscript-dialog-right-margin 40)
(defvar comicscript-page-id 0)
(defvar comicscript-pages ())

;; This is pretty lame
(setq comicscript-page-spelled '("ONE" "TWO" "THREE" "FOUR"
	"FIVE" "SIX" "SEVEN" "EIGHT" "NINE" "TEN"
	"ELEVEN" "TWELVE" "THIRTEEN" "FOURTEEN" 
	"FIFTEEN" "SIXTEEN" "SEVENTEEN"
	"EIGHTEEN" "NINETEEN" "TWENTY" "TWENTY-ONE" "TWENTY-TWO"
	"TWENTYTHREE" "TWENTYFOUR"))

;; I'll give internal variables and defuns 'scrn' prefix.
(defvar scrn-scene-hist () "History list for scene headings.")
(defvar scrn-dialog-name-hist () "History list for dialog block name attribute.")

;; Some syntax highlighting
(setq myKeywords
 '(("Panel [0-9]*\." . font-lock-function-name-face)
   ("PAGE .*" . font-lock-constant-face)
   ("^.*:" . font-lock-comment-face)
  )
)

(define-derived-mode comicscript-mode fundamental-mode "Comicscript"
  "Major mode for editing comicscripts."
  (setq font-lock-defaults '(myKeywords))
  (define-key comicscript-mode-map "\C-x\o" 'comicscript-do-repaginate)
  (define-key comicscript-mode-map ":" 'comicscript-dialog)
  (define-key comicscript-mode-map "\t\r" 'comicscript-add-new-page)
  (define-key comicscript-mode-map "\t\t\r" 'comicscript-add-new-panel)
  (define-key comicscript-mode-map "\t\t\t\r" 'comicscript-dialog-block)
  (make-local-variable 'comicscript-right-margin)
  (make-local-variable 'comicscript-left-margin)
  (make-local-variable 'scrn-dialog-name-hist)
  (make-local-variable 'comicscript-panel-number)
  (make-local-variable 'comicscript-page-number)
  )

(defun get-panel-id-substring(start end)
"get the substring"
(interactive)
(save-excursion
  (+ (string-to-number (buffer-substring-no-properties (+ start 6) ( - end 1))
		       ) 1 )))

(defun update-panel-count (back-count)
  "fix panel count"
  nil
)

(defun comicscript-repanelate (start end)
"list based repanelation"
(goto-char start)
(save-excursion
  (while (< (+ 20 (point)) end)
    (setq panel-point (re-search-forward "^Panel \\([0-9]*\\)" nil t))
    (if (not panel-point) (setq panel-point (+ end 5)))
    (if (< panel-point end)
	(comicscript-repanelate-inner)
      (insert "-")
      )
    )
  )
)

(defun comicscript-repanelate-inner () 
  (interactive)
  (setq start (- (point) 2))
  (setq reg-end (re-search-forward "\\." nil t))
  (delete-region start reg-end)
  (setq comicscript-panel-id (+ comicscript-panel-id 1))
  (insert " ")
  (insert (number-to-string comicscript-panel-id))
  (insert ".")

)

(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (insert (number-to-string (car list)))
    (insert " : ")
    (setq list (cdr list)))
)

(defun comicscript-do-repaginate() 
"run repaginate"
(interactive)
 (save-excursion
   (setq comicscript-page-id -1)
   (goto-char (point-min))
   (setq start-point (re-search-forward "^PAGE " nil t))
   (comicscript-repaginate start-point)
   (setq comicscript-pages (nreverse comicscript-pages))
   )
 (save-excursion
   (while comicscript-pages
     (setq start (car comicscript-pages))
     (setq comicscript-pages (cdr comicscript-pages))
     (setq end (car comicscript-pages))
     (setq comicscript-panel-id 0)
     (if end
	 (comicscript-repanelate start end)
       )
     )
   )
)
 
    
(defun comicscript-repaginate (keep-going)
  (interactive)
  (setq comicscript-pages ())
  (while keep-going 
    (setq comicscript-pages 
	  (cons keep-going comicscript-pages))
    (delete-region (point) (line-end-position))
    (setq comicscript-page-id (+ comicscript-page-id 1))
    (insert (nth comicscript-page-id comicscript-page-spelled))
    (setq keep-going (re-search-forward "^PAGE " nil t))
    )
    (setq comicscript-pages
	  (cons (point-max) comicscript-pages)
    
))

(defun scrn-margins ()
  "Set left-margin and fill-column for page and action blocks."
  (setq left-margin comicscript-left-margin)
  (setq fill-column comicscript-right-margin))

(defun scrn-edit-page ()
  (cond (current-prefix-arg
         (scrn-margins)
         nil)
        (t
         (comicscript-read-page))))

(defun comicscript-add-new-page ()
  "Insert a page page"
  (interactive)
  (newline 2)
  (scrn-margins)
  (indent-to-left-margin)
  (insert "PAGE ")
  (newline 2)
  (comicscript-do-repaginate)
)

(defun comicscript-panel-block ()
  "Insert a panel"
  ;; Search backward for previous id, search forward to see if
  ;; the ids need to be reset
  ;; update number of panels for page
  (interactive)
  (save-excursion
    (setq panel-point (point))
    (setq back-boundary (re-search-backward "^PAGE \\([0-9]*\\)" nil t))
    (setq next-panel (get-next-panel back-boundary))
    )
  (if back-boundary (update-panel-count back-boundary)
    (
    (insert "Page 1 (1 panels)")
    (newline 2)
    ))
  (insert "Panel ")
  (insert (number-to-string next-panel))
  (insert ".  ")
)

(transient-mark-mode 1)

(defun comicscript-dialog ()
  "back up, capitalize, and return"
  (interactive)
  (save-excursion
    (upcase-region (point) (re-search-backward "^.")))
    (insert ":")
    (newline 1)
)

(defun scrn-dialog-margins ()
  (setq left-margin comicscript-dialog-left-margin)
  (setq fill-column comicscript-dialog-right-margin))

(defun scrn-edit-dialog ()
  (cond (current-prefix-arg
         (scrn-dialog-margins)
         (use-hard-newlines 1 t)
         nil)
        (t
         (comicscript-dialog-char-name))))

(defun comicscript-version ()
  "Display current program version in echo area."
  (interactive)
  (message "Comicscript Mode Version %s" comicscript-version))

(defun comicscript-submit-bug-report ()
  "Submit a bug report for Comicscript Mode."
  (interactive)
  (require 'reporter)
  (reporter-submit-bug-report
   comicscript-bug-address
   (concat "comicscript-" comicscript-version)
   nil
   nil
   nil
   "Please make your report as detailed as possible.
I'll try to fix it as soon as possible.

Thanks,
Jim
Emacs Comicscript Mode
http://oversimplified/comicscript/"))

(provide 'comicscript)
;;; comicscript.el ends here
