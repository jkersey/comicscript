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

(defcustom comicscript-left-margin 0
  "margin"
  :type 'integer
  :group 'comicscript)

(defcustom comicscript-right-margin 50
  "margin"
  :type 'integer
  :group 'comicscript)

(defcustom comicscript-panel-number 1
  "Current panel number"
  :type 'integer
  :group 'comicscript)

(defcustom comicscript-page-number 0
  "Current page index"
  :type 'integer
  :group 'comicscript)

(defvar comicscript-dialog-left-margin 10)
(defvar comicscript-dialog-right-margin 40)


;; This is pretty lame
(setq comicscript-page-spelled '("ONE" "TWO" "THREE" "FOUR"
	"FIVE" "SIX" "SEVEN" "EIGHT" "NINE" "TEN"
	"ELEVEN" "TWELVE" "THIRTEEN" "FOURTEEN" 
	"FIFTEEN" "SIXTEEN" "SEVENTEEN"
	"EIGHTEEN" "NINETEEN" "TWENTY" "TWENTY-ONE" "TWENTY-TWO"
	"TWENTYTHREE" "TWENTYFOUR"))

;; I'll give internal variables and defuns 'scrn' prefix.
(defvar scrn-scene-hist ()
  "History list for scene headings.")



(defvar scrn-dialog-name-hist ()
  "History list for dialog block name attribute.")

;; Some syntax highlighting
(setq myKeywords
 '(("Panel [0-9]*\." . font-lock-function-name-face)
;;   ("PAGE .*" . font-lock-constant-face)
   ("^.*:" . font-lock-comment-face)
  )
)

(define-derived-mode comicscript-mode fundamental-mode "Comicscript"
  (setq font-lock-defaults '(myKeywords))
  "Major mode for editing comicscripts.
\\{comicscript-mode-map}"
  (define-key comicscript-mode-map "C-x o" 'reload-comicscript)
  (define-key comicscript-mode-map ":" 'comicscript-dialog)
  (define-key comicscript-mode-map "\t\r" 'comicscript-page)
  (define-key comicscript-mode-map "\t\t\r" 'comicscript-panel-block)
  (define-key comicscript-mode-map "\t\t\t\r" 'comicscript-dialog-block)
  (make-local-variable 'scrn-scene-hist)
  (make-local-variable 'comicscript-right-margin)
  (make-local-variable 'comicscript-left-margin)
  (make-local-variable 'scrn-dialog-name-hist)
  (make-local-variable 'comicscript-panel-number)
  (make-local-variable 'comicscript-page-number)
  )

(defun get-substring(start end)
  "get the clean substring" 
  (interactive)
  (save-excursion
    (+ (string-to-number (buffer-substring-no-properties (+ start 5) ( - end 2))
			 ) 1)))

(defun get-next-page ()
"get the next page number"
  (save-excursion
    (setq start (re-search-backward "^PAGE \\([0-9]*\\)" nil t))
    (setq end (re-search-forward " \(" nil t))
    (if start (get-substring start end) 1)))

(defun scrn-margins ()
  "Set left-margin and fill-column for page and action blocks."
  (setq left-margin comicscript-left-margin)
  (setq fill-column comicscript-right-margin))

(defun comicscript-reset-script ()
  (setq comicscript-page-number 0))

(defun comicscript-read-page ()
  "Get scene heading.
Returns scene heading in upper-case format."
  (let ((scene-heading 
         (let ((prompt "Enter scene heading: "))
           (read-from-minibuffer prompt 
                                 nil           ;initial-contents
                                 nil           ;keymap
                                 nil           ;read
                                 'scrn-scene-hist   ;hist
                                 nil           ;default
                                 nil))))       ;inherit-input-method
    scene-heading))

(defun scrn-edit-page ()
  (cond (current-prefix-arg
         (scrn-margins)
         nil)
        (t
         (comicscript-read-page))))

(defun reload-comicscript ()
  "reload"
  (interactive)
  (require 'comicscript)
  (comicscript-mode)
)


(defun comicscript-page ()
  "Insert a page heading.
To edit an existing page heading, put the cursor on that line
and call this function with a prefix-arg, i.e, C-u TAB-RET."
  (interactive)
  (setq next-page (get-next-page))
  (newline 2)
  (scrn-margins)
  (indent-to-left-margin)
  (insert "PAGE ")
  (insert (number-to-string next-page))
  (insert " (0 panels) ")
)

(defun comicscript-panel-block ()
  "A panel"
  ;; Search backward for previous id, search forward to see if
  ;; the ids need to be reset
  ;; update number of panels for page
  (interactive)
  (cond (current-prefix-arg
         (scrn-margins))
        (t
         (newline 2)
         (scrn-margins)
	 (insert "Panel ")
	 ;;(setq next-page (re-search-forward "Panel \\([0-9]*\\)")) ;;[^/r]*PAGE"))
	 (insert (prin1-to-string comicscript-panel-number))
	 (insert ".  ")
         (use-hard-newlines -1)
         (indent-to-left-margin)
	 (setq comicscript-panel-number (+ comicscript-panel-number 1))
	 )))

(transient-mark-mode 1)

(defun comicscript-dialog ()
  "back up, capitalize, and return"
  (interactive)
  (save-excursion
    (upcase-region (point) (re-search-backward "^.")))
    (insert ":")
    (newline 1)
)

(defun comicscript-dialog-char-name ()
"Return uppercase dialog block character tag."
  (let ((char-name
         (let ((prompt "Enter character name: "))
           (read-from-minibuffer prompt
                                 nil
                                 nil
                                 nil
                                 'scrn-dialog-name-hist
                                 nil
                                 nil))))
    (upcase char-name)))


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

(defun comicscript-dialog-block (name)
  "Edit dialog block."
  (interactive (list (scrn-edit-dialog)))
  (cond ((not name)
         nil)
        (t
         (use-hard-newlines 1 t)
         (newline 2)
         (setq left-margin 0)
         (indent-to-left-margin)
         (insert name)
	 (insert ":")
         (newline 1)
         (setq left-margin 00)
         (setq fill-column 40)
         (indent-to-left-margin))))

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
