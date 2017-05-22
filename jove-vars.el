;;;; jove-vars.el --- Jove Mode Variables -*- lexical-binding: t; -*-

;;; Copyright (C) 2017 John Hooks

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

;;; Code:

(defcustom jove-indent-level 2
  "Number of spaces for each indentation step in `js-mode'."
  :type 'integer
  :safe 'integerp
  :group 'jove-mode)

;; TODO: Remove. Always provide for lastest features which for now are
;; upto ECMAScript 8.
(defcustom jove-ecma-version 6
  "ECMAScript version used to parse."
  :type 'number
  :group 'jove-mode)

;; TODO: Remove. Would be necessary if my purpose was to check the
;; parsed code for validity.
(defcustom jove-strict t
  "Parse all JavaScript as module code."
  :type 'boolean
  :group 'jove-mode)

(defcustom jove-verbose nil
  "Print information about parsing to *Messages* buffer."
  :type 'boolean
  :group 'jove-mode)

(defcustom jove-debug nil
  "Print parsing errors to *Messages* buffer."
  :type 'boolean
  :group 'jove-mode)

(defcustom jove-idle-timer-delay 0.20
  "Delay in secs before re-parsing after user makes changes."
  :type 'number
  :group 'jove-mode)
(make-variable-buffer-local 'jove-idle-timer-delay)

(defcustom jove-fontify-regexp-grouping-chars t
  "Fontify special grouping characters in regular expressions."
  :type 'boolean
  :group 'jove-mode)

;; Recoverable Error Messages

(defconst jove-messages
  (let ((hash (make-hash-table :test 'eq)))
    (mapc #'(lambda (pair)
              (puthash (car pair) (cadr pair) hash))
          '((:await-outside-async
             "found 'await' used outside of an async function.")
            (:cannot-insert-semi
             "unable to insert semicolon after statement.")))
    hash)
  "A hash table containing recoverable error keys and messages.")

;;; Errors

(define-error 'jove-error "A jove error")

(define-error 'jove-unexpected-character-error
  "Unexpected character" 'jove-error)

(define-error 'jove-parse-error
  "Jove parser errror" 'jove-error)

;;; Global Variables

(defvar jove-keywords (make-hash-table :test 'equal)
  "Hash table to map keyword names to token types.")

;;; Buffer Local Variables

(defvar-local jove-mode-reparse-on-edit nil
  "Boolean to flag automactic reparse on edit.
Typical use is while debugging Jove Mode itself.")

(defvar-local jove-ast nil
  "Parsed abstract syntax tree.")

(defvar-local jove--state nil
  "The current parser state.
Private variable.")

(defvar-local jove--buffer-dirty-p nil
  "Boolean to flag the buffer requires reparsing.
Private variable.")

(defvar-local jove--ast-complete-p nil
  "Boolean to flag the abstract syntax tree is complete.
Private variable.")

(defvar-local jove--cache-end 0
  "Number representing the position of the last buffer modification.
Private variable.")

(defvar-local jove--idle-timer nil
  "Timer used for idle reparseing.
Private variable.")

(defvar-local jove--fontifications nil
  "List of deferred fontifications.
Private variable.")

(defvar-local jove--warnings '()
  "A list to hold queued warnings.
Private variable.")

(defvar-local jove--errors '()
  "A list to hold queued errors.
Private variable.")

(defvar-local jove--timeline '()
  "A list to hold copies of the parser state before each token.")

;;; Lexer Hooks

(defvar jove-comment-hook nil
  "Abnormal hook for comments, args start and end.")

(provide 'jove-vars)

;;; jove-vars.el ends here
