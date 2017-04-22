;;;; jove-vars.el --- Jove Global and Buffer Local Variables -*- lexical-binding: t; -*-

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

(defcustom jove-indent-level 2
  "Number of spaces for each indentation step in `js-mode'."
  :type 'integer
  :safe 'integerp
  :group 'jove-mode)

(defcustom jove-ecma-version 6
  "ECMAScript version used to parse."
  :type 'number
  :group 'jove-mode)

(defcustom jove-strict t
  "Parse all JavaScript as module code."
  :type 'boolean
  :group 'jove-mode)

(defcustom jove-verbose nil
  "Print information about parsing to *Messages* buffer."
  :type 'boolean
  :group 'jove-mode)

(defcustom jove-idle-timer-delay 0.20
  "Delay in secs before re-parsing after user makes changes."
  :type 'number
  :group 'jove-mode)
(make-variable-buffer-local 'jove-idle-timer-delay)

;;; Errors

(define-error 'jove-error "A jove error")

(define-error 'jove-unexpected-character-error
  "Unexpected character" 'jove-error)

(define-error 'jove-parse-error
  "Jove parser errror" 'jove-error)

;;; Buffer Local Variables

(defvar-local jove-ast nil
  "Parsed abstract syntax tree.")

(defvar-local jove--state nil
  "The parser state.
Private variable.")

(defvar-local jove--parsing nil
  "Private variable.")

(defvar-local jove--fontifications nil
  "List of deferred fontifications.
Private variable.")

(defvar-local jove--buffer-dirty-p nil
  "Boolean representing whether the buffer requires reparsing.
Private variable.")

(defvar-local jove--cache-end 0
  "Number representing the position of the last buffer modification.
Private variable.")

(defvar-local jove--idle-timer nil
  "Timer used for idle reparseing.
Private variable.")

(defvar-local jove--string-buffer nil
  "List of chars built up while scanning various tokens.
Private variable.")

(defvar-local jove--warnings '()
  "A list to hold queued warnings.
Private variable.")

;;; Lexer Hooks

(defvar jove-comment-hook nil
  "Abnormal hook for comments, args start and end.")

(provide 'jove-vars)

;;; jove-vars.el ends here
