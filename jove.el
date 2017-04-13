;;;; jove.el --- A JavaScript Mode -*- lexical-binding: t; -*-

;;; Copyright (C) 2017 John Hooks

;; This file is part of Jove
;;
;; Jove is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Jove is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Jove.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'jove-lexer)

(defgroup jove-mode nil
  "A JavaScript mode."
  :group 'languages)

;;; Variables

(defcustom jove-indent-level 2
  "Number of spaces for each indentation step in `js-mode'."
  :type 'integer
  :safe 'integerp
  :group 'jove-mode)

(defcustom jove-idle-timer-delay 0.25
  "Delay in secs before re-parsing after user makes changes."
  :type 'number
  :group 'jove-mode)
(make-variable-buffer-local 'jove-idle-timer-delay)

(defvar jove-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    ;; Don't know why '+' was a word or symbol char before here.
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?` "\"" table)
    table)
  "Syntax table for `jove-mode'.")

;;; Buffer Local Variables

(defvar-local jove--parsing nil "Private variable.")
(defvar-local jove--cache-end 0 "Private variable.")
(defvar-local jove--idle-timer nil "Private variable.")
(defvar-local jove--buffer-dirty-p nil "Private variable.")

;;; Idle Reparse Timer Functions

(defun jove--reset-timer ()
  "Cancel any existing parse timer and schedule a new one."
  (when jove--idle-timer
    (cancel-timer jove--idle-timer))
  (setq jove--parsing nil)
  (let ((timer (timer-create)))
    (setq jove--idle-timer timer)
    (timer-set-function timer 'jove--idle-reparse (list (current-buffer)))
    (timer-set-idle-time timer jove-idle-timer-delay)
    (timer-activate-when-idle timer nil)))

(defun jove--idle-reparse (buffer)
  "Run `jove--reparse' if BUFFER is the current buffer, or schedule
it to be reparsed when the buffer is selected."
  (cond ((eq buffer (current-buffer))
         (jove--reparse))
        ((buffer-live-p buffer)
         ;; reparse when the buffer is selected again
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
  "Initiate reparse using `parse-js-lex'."
  (unwind-protect
      (when (or jove--buffer-dirty-p force)
        (setq jove--buffer-dirty-p nil
              jove--fontifications nil
              jove--warnings nil)
        (jove-lex))
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

;;; Before and After Change Hook Functions

(defun jove--flush-caches (&optional start ignore)
  (setq start (or start (save-restriction (widen) (point-min))))
  (setq jove--cache-end (min jove--cache-end start)))

(defun jove--edit (_beg _end _len)
  "Schedule a new parse after buffer is edited.
Buffer edit spans from BEG to END and is of length LEN."
  (setq jove--buffer-dirty-p t)
  (jove--reset-timer))

;;; The Mode

(define-derived-mode jove-mode prog-mode "JavaScript"
  "A JavaScript editing mode."
  :group 'jove-mode

  ;; Highlighting is handled by the parser.
  (setq-local font-lock-defaults '(nil t))
  (setq-local syntax-propertize-function nil)

  (setq-local comment-start "// ")
  (setq-local comment-end "")

  (setq-local indent-line-function #'jove-indent-line)

  ;; Change hooks
  (add-hook 'before-change-functions #'jove--flush-caches t t)
  (add-hook 'after-change-functions #'jove--edit nil t)

  ;; (add-hook 'jove-lexer-chunk-hook #'jove--apply-fontifications)

  (setq-local electric-indent-chars
               ;FIXME: js2-mode adds "[]*".
              (append "{}():;," electric-indent-chars))
  (setq-local electric-layout-rules
              '((?\; . after) (?\{ . after) (?\} . before)))

  (jove--reparse t))

(provide 'jove)

;;; jove.el ends here
