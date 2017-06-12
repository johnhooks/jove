;;;; jove-mode.el --- A Jove Mode -*- lexical-binding: t; -*-

;;; Copyright (C) 2017 John Hooks

;; Auther: John Hooks
;; URL: https://github.com/johnhooks/jove
;; Version: 0.3.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: languages, javascript

;; This file is part of Jove Mode.
;;
;; Jove Mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Jove Mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Jove Mode.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A JavaScript Mode.

;;; Code:

(require 'cc-mode)
(require 'js)

(require 'jove-identifier)
(require 'jove-lexer)
(require 'jove-parser)
(require 'jove-polyfill)
(require 'js-align)

(defgroup jove-mode nil
  "A JavaScript mode."
  :group 'languages)

;;; Variables

(defvar jove-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?$ "w" table)
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?` "\"" table)
    (modify-syntax-entry '(?0 . ?9) "_" table)
    (dolist (char jove-syntax-word-chars)
      (modify-syntax-entry char "w" table))
    (dolist (char jove-syntax-symbol-chars)
      (modify-syntax-entry char "_" table))
    table)
  "Syntax table for `jove-mode'.
The word syntax class is used for JavaScript Identifier start characters,
and the symbol syntax class is used for Identifier part characters ")

;;; Idle Reparse Timer Functions

(defun jove--reset-timer ()
  "Cancel any existing parse timer and schedule a new one."
  (when jove--idle-timer
    (cancel-timer jove--idle-timer))
  (let ((timer (timer-create)))
    (setq jove--idle-timer timer)
    (timer-set-function timer 'jove--idle-reparse (list (current-buffer)))
    (timer-set-idle-time timer jove-idle-timer-delay)
    (timer-activate-when-idle timer nil)))

(defun jove--idle-reparse (buffer)
  "Run `jove--reparse' if BUFFER is the current buffer, or schedule
it to be reparsed when the buffer is selected."
  (cond
   ((eq buffer (current-buffer))
    (jove--reparse))
   ((buffer-live-p buffer)
    (with-current-buffer buffer
      (add-hook 'window-configuration-change-hook
                #'jove--idle-reparse-inner
                nil t)))))

(defun jove--idle-reparse-inner ()
  (remove-hook 'window-configuration-change-hook
               #'jove--idle-reparse-inner
               t)
  (jove--reparse))

(defun jove--reparse (&optional force)
  "Initiate reparse using `jove-parse'."
  (unwind-protect
      (when (or jove--buffer-dirty-p force)
        (setq jove--buffer-dirty-p nil
              jove--fontifications nil
              jove--warnings nil)
        (jove-parse))
    (setq jove--idle-timer nil)))

;;; Indentation Functions

;; Based on the code in `js--proper-indentation' from `js.el'
(defun jove--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  ;; Not complete yet. Will use lexer and parser data when available.
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)         ; Inside comment
           (message "inside comment, maybe %s" (nth 1 parse-status))
           (current-column))
          ((nth 3 parse-status)         ; Inside string
           (current-column))
          ((nth 1 parse-status)         ; Inside parenthetical grouping
           (let ((same-indent-p (looking-at-p "[]})]")))
             (goto-char (nth 1 parse-status)) ; Go to the opening char
             (if (looking-at-p "[[{(]\\s-*\\(/[/*]\\|$\\)")
                 (progn
                   (back-to-indentation)
                   (cond (same-indent-p
                          (current-column))
                         (t
                          (+ (current-column) jove-indent-level))))
               (unless same-indent-p
                 (forward-char)
                 (skip-chars-forward " \t"))
               (current-column))))
          (t
           0))))

(defun jove-indent-line ()
  "Indent the current line as JavaScript."
  (interactive)
  (let* ((parse-status
          (save-excursion (syntax-ppss (point-at-bol))))
         (offset (- (point) (save-excursion (back-to-indentation) (point)))))
    (unless (nth 3 parse-status)
      (indent-line-to (jove--proper-indentation parse-status))
      (when (> offset 0) (forward-char offset)))))

;;; Settings Toggle Function

(defun jove-disable-parser ()
  "Disable automatic syntax reparse on edit."
  (interactive)
  (when jove--idle-timer
    (cancel-timer jove--idle-timer))
  (setq jove-disable-parser-p t))

(defun jove-toggle-verbose ()
  "Toggle verbose parser messages."
  (interactive)
  (setq jove-verbose (not jove-verbose)))

(defun jove-toggle-debug ()
  "Toggle debug parser messages."
  (interactive)
  (setq jove-debug (not jove-debug))
  (jove--reparse t))

(defun jove-toggle-regex-extras ()
  "Toggle extra regular expression highlighting."
  (interactive)
  (setq jove-fontify-regexp-grouping-chars
        (not jove-fontify-regexp-grouping-chars))
  (jove--reparse t))

;;; Misc

(defun jove-fontify-comment ()
  (jove-set-face jove--start jove--end 'font-lock-comment-face))

;;; Before and After Change Hook Functions

(defun jove--flush-caches (&optional start _ignore)
  ;; Don't know what the IGNORE argument is for.
  (setq start (or start (save-restriction (widen) (point-min))))
  (jove-flush-ast (min start jove--cache-end)))

(defun jove--edit (_beg _end _len)
  "Schedule a new parse after buffer is edited.
Buffer edit spans from BEG to END and is of length LEN."
  (setq jove-ast-complete-p nil
        jove--buffer-dirty-p t)
  (unless jove-disable-parser-p
    (jove--reset-timer)))

;;; The Mode

(define-derived-mode jove-mode js-mode "\u26A1JS"
  "A JavaScript editing mode."
  :group 'jove-mode

  ;; Highlighting is handled by the parser.
  (setq-local font-lock-defaults '(nil t))

  (setq-local comment-start "// ")
  (setq-local comment-end "")

  (setq-local indent-line-function #'js-align-indent-line)

  ;; Change hooks
  (add-hook 'before-change-functions #'jove--flush-caches t t)
  (add-hook 'after-change-functions #'jove--edit nil t)

  (add-hook 'jove-after-comment-hook #'jove-fontify-comment)

  (setq-local syntax-propertize-function #'js-syntax-propertize)

  ;; TODO: replace `forward-sexp-function'

  (setq-local electric-indent-chars
               ;FIXME: js2-mode adds "[]*".
              (append "{}():;," electric-indent-chars))
  (setq-local electric-layout-rules
              '((?\; . after) (?\{ . after) (?\} . before)))

  (unless jove-disable-parser-p
    (jove--reparse t)))

(provide 'jove-mode)

;;; jove-mode.el ends here
