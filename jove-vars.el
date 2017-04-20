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

(defcustom jove-idle-timer-delay 0.25
  "Delay in secs before re-parsing after user makes changes."
  :type 'number
  :group 'jove-mode)
(make-variable-buffer-local 'jove-idle-timer-delay)

(defcustom jove-lexer-ceiling (/ 1.0 45)   ; Changed from 30
  "Maximum amount of time to chunk the lexer."
  :type 'number
  :group 'jove-mode)

(defcustom jove-lexer-timeout (/ 1.0 100)
  "Amount of time to pause the lexer."
  :type 'number
  :group 'jove-mode)

;;; Errors

(define-error 'jove-error "A jove error")

(define-error 'jove-unexpected-character-error
  "Unexpected character" 'jove-error)

(define-error 'jove-parse-error
  "Jove parser errror" 'jove-error)

;;; Buffer Local Variables

(defvar-local jove--parsing nil "Private variable.")

(defvar-local jove--fontifications nil
  "List of deferred fontifications.")

(defvar-local jove--buffer-dirty-p nil
  "Boolean representing whether the buffer requires reparsing.")

(defvar-local jove--cache-end 0
  "Number representing the position of the last buffer modification.")

(defvar-local jove--cache '()
  "Vector of cached lexer states.")

(defvar-local jove--idle-timer nil
  "Timer used for idle reparseing.")

(defvar-local jove--string-buffer nil
  "List of chars built up while scanning various tokens.")

(defvar-local jove--warnings '()
  "A list to hold queued warnings.")

;;; Lexer Hooks

(defvar-local jove-lexer-chunk-hook nil
  "Hooks called after finishing a chunk.")

(defvar-local jove-lexer-complete-hook nil
  "Hook called after lexical process is complete.")

(defvar jove-comment-hook nil
  "Abnormal hook for comments, args start and end.")

(provide 'jove-vars)

;;; jove-vars.el ends here
