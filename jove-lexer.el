;;;; jove-lexer.el --- A JavaScript Mode -*- lexical-binding: t; -*-

;;; Copyright (C) 2017 John Hooks

;; This file is part of jove
;;
;; jove is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; jove is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with jove.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'jove-token-types)

(defcustom jove-ecma-version 6
  "ECMAScript version used to parse."
  :type 'number
  :group 'jove)

(defcustom jove-always-strict t
  "Parse all JavaScript as module code."
  :type 'boolean
  :group 'jove)

(setq jove--strict t)

(defcustom jove-verbose nil
  "Print information about parse to *Messages* buffer."
  :type 'boolean
  :group 'jove)

(defcustom jove-lexer-ceiling (/ 1.0 45)   ; Changed from 30
  "Maximum amount of time to chunk the lexer."
  :type 'number
  :group 'jove)

(defcustom jove-lexer-timeout (/ 1.0 100)
  "Amount of time to pause the lexer."
  :type 'number
  :group 'jove)

;;; Local Variables

(defvar-local jove--lexer-state nil)

(defvar-local jove--string-buffer nil
  "List of chars built up while scanning various tokens.")

(defvar-local jove--warnings '()
  "A list to hold queued warnings.")

;;; Lexer Hooks

(defvar-local jove-lexer-chunk-hook nil
  "Hooks called after finishing a chunk.")

(defvar-local jove-lexer-complete-hook nil
  "Hook called after lexical process is complete.")

(defvar-local jove-comment-hook nil
  "Abnormal hook for comments, args start and end.")

(defvar-local jove-keyword-hook nil
  "Hook for keyword tokens.")

(defvar-local jove-regexp-hook nil
  "Hook for regular expression tokens.")

(defvar-local jove-string-hook nil
  "Hook for string tokens.")

(defvar-local jove-template-hook nil
  "Hook for template literal tokens.")

;;; Errors

(define-error 'jove-error "A joust error")

(define-error 'jove-unexpected-character-error
  "Unexpected character" 'jove-error)

;;; Context Types

;; 0 token
;; 1 is-expr
;; 2 preserve-space
;; 3 override

(cl-defun jove--ctx-create (token &key is-expr preserve-space override)
  "Return a vector representing a context type."
  (vector token is-expr preserve-space override))

(defsubst jove--ctx-token (ctx)
  (aref ctx 0))
(defsubst jove--ctx-is-expr (ctx)
  (aref ctx 1))
(defsubst jove--ctx-preserve-space (ctx)
  (aref ctx 2))
(defsubst jove--ctx-override (ctx)
  (aref ctx 3))

(defvar jove-B-STAT (jove--ctx-create "{"))
(defvar jove-B-EXPR (jove--ctx-create "{" :is-expr t))
(defvar jove-B-TMPL (jove--ctx-create "${" :is-expr t))
(defvar jove-P-STAT (jove--ctx-create "("))
(defvar jove-P-EXPR (jove--ctx-create "(" :is-expr t))
(defvar jove-F-EXPR (jove--ctx-create "function" :is-expr t))
(defvar jove-Q-TMPL (jove--ctx-create "`" :is-expr t :preserve-space t :override #'jove--read-tmpl-token))

;;; Regular Expressions

(defvar jove--binary-re "[0-1]+")
(defvar jove--octal-re "[0-7]+")
(defvar jove--decimal-re "[0-9]+")
(defvar jove--hexadecimal-re "[0-9A-Fa-f]+")

;;; Lexer Token State

;; Use `point' to track the current position of the lexer in the buffer.
;; The logic behind using `point', is the relative ease of use to apply
;; elisp regexp functions when reading tokens in the buffer.

;; Unlike Acorn, this parser is working inside an Emacs buffer, in
;; which positions are between characters and start at 1

;; 0 start
;; 1 end
;; 2 type
;; 3 value
;; 4 ctx-stack
;; 5 expr-allowed
;; 6 newline-before
;; 7 contains-escape

(defun jove--lex-create ()
  "Return a vector representing an initial lexer state."
  (vector 1 1 jove-EOF nil (jove--initial-ctx) t nil nil))

(defun jove--lex-p (state)
  "Predicate returns t if STATE is a lexer state."
  ;; value can be anything
  (and (vectorp state)
       (= (length state) 8)
       (numberp (aref state 0))
       (numberp (aref state 1))
       (vectorp (aref state 2))
       (listp (aref state 4))
       (booleanp (aref state 5))
       (booleanp (aref state 6))
       (booleanp (aref state 7))))

;; Use `vconcat' to copy previous state.

(defsubst jove--start (state)
  (aref state 0))
(defsubst jove--set-start (state value)
  (aset state 0 value))

(defsubst jove--end (state)
  (aref state 1))
(defsubst jove--set-end (state value)
  (aset state 1 value))

(defsubst jove--type (state)
  (aref state 2))
(defsubst jove--set-type (state value)
  (aset state 2 value))

(defsubst jove--value (state)
  (aref state 3))
(defsubst jove--set-value (state value)
  (aset state 3 value))

;; Quoted acorn/src/state.js
;;
;;   "The context stack is used to superficially track syntactic
;;    context to predict whether a regular expression is allowed in a
;;    given position."

(defsubst jove--ctx-stack (state)
  (aref state 4))
(defsubst jove--ctx-stack-push (state value)
  (aset state 4 (cons value (aref state 4))))
(defsubst jove--ctx-stack-pop (state)
  (prog1
      (car (aref state 4))
    (aset state 4 (cdr (aref state 4)))))

(defsubst jove--expr-allowed (state)
  (aref state 5))
(defsubst jove--set-expr-allowed (state value)
  (aset state 5 value))

(defsubst jove--newline-before (state)
  (aref state 6))
(defsubst jove--set-newline-before (state value)
  (aset state 6 value))

(defsubst jove--contains-esc (state)
  (aref state 7))
(defsubst jove--set-contains-esc (state value)
  (aset state 7 value))

;;; Token

;; I don't know if token will be necessary because all the relevent information
;; is contained in the lexer state.

;; 0 start
;; 1 end
;; 2 type
;; 3 value

(defun jove--token-create (start end type &optional value)
  "Return a vector representing a token."
  (vector start end type value))

(defun jove--token-p (object)
  "Return non-nil if OBJECT could represent a token."
  ;; value can be anything
  (and (vectorp object)
       (= 3 (length object))
       (numberp (aref object 0))
       (numberp (aref object 1))
       (vectorp (aref object 2))))

(defsubst jove--token-start (token)
  (aref token 0))
(defsubst jove--token-end (token)
  (aref token 1))
(defsubst jove--token-type (token)
  (aref token 2))
(defsubst jove--token-value (token)
  (aref token 3))

;;; Utility Functions

(defun jove--warn (start end message)
  "Queue a MESSAGE into `jove--warnings'."
  (push (list start end message) jove--warnings)
  nil)                                  ; Return nil

(defun jove--unexpected-char ()
  "Signal unexpected character error at current position."
  (jove--warn (point) (1+ (point)) "unexpected char")
  (forward-char))

(defun jove--raise (type start end &optional message)
  "Signal error of TYPE with an argument alist of START, END and MESSAGE."
  (signal type (if message
                   `((start ,start) (end ,end) (message, message))
                 `((start ,start) (end ,end)))))

;; http://nullprogram.com/blog/2017/01/30/
;; Builting a list and using `nreverse' is the correct way to build a
;; list in reverse. `nreverse' is a very fast C builtin function.

(defun jove--collect-string ()
  "Convert `jove--string-buffer', a list of chars, to a string.
Reverses the list before converting."
  (if jove--string-buffer
      (prog1 ;; Wouldn't `concat' work?
          (apply #'string (nreverse jove--string-buffer))
        (setq jove--string-buffer nil))
    ""))

(defsubst jove--add-to-string (char)
  "Add CHAR to `jove--string-buffer'."
  (push char jove--string-buffer))

;;; Helper Predicate Functions

;; Taken from `js2-mode-identifier-start-p'
(defun jove-identifier-start-p (char)
  "Is CHAR a valid start to an ES5 Identifier?
See http://es5.github.io/#x7.6"
  (or
   (memq char '(?$ ?_))
   (memq (get-char-code-property char 'general-category)
         ;; Letters
         '(Lu Ll Lt Lm Lo Nl))))

;; Taken from `js2-mode-identifier-part-p'
(defun jove-identifier-part-p (char)
  "Is CHAR a valid part of an ES5 Identifier?
See http://es5.github.io/#x7.6"
  (or
   (memq char '(?$ ?_ ?\u200c  ?\u200d))
   (memq (get-char-code-property char 'general-category)
         '(;; Letters
           Lu Ll Lt Lm Lo Nl
           ;; Combining Marks
           Mn Mc
           ;; Digits
           Nd
           ;; Connector Punctuation
           Pc))))

;;; Lexer Utility Functions - Movement

(defun jove--peek (&optional count)
  "Peek forward one or COUNT number of `char-after' point.
This function does not move point."
  (char-after (+ (point) (or count 1))))

(defun jove--match (regexp)
  "Return the `match-end' if text after point match REGEXP."
  ;; NOTE: `looking-at-p' does not update match data.
  (and (looking-at regexp)
       (match-end 0)))

(defun jove--eat-re (regexp)
  "Goto the `match-end' if text after point match REGEXP.
Set point to the end of match, and return point."
  (and (looking-at regexp)
       (goto-char (match-end 0))))

(defun jove--expect-char (character)
  "Return t and `forward-char' if `char-after' point is eq CHARACTER.
Otherwise signal `jove-unexpected-character-error'."
  (if (eq character (char-after))
      (progn (forward-char) t)
    (jove--unexpected-char)))

(defun jove--skip-line-comment (state)
  "Skip line comment and run abnormal comment hook."
  (let ((start (point)))
    (if (search-forward "\n" nil t)
        (jove--set-newline-before state t)
      (goto-char (point-max)))
    (run-hook-with-args 'jove-comment-hook start (point))))

(defun jove--skip-block-comment (state)
  "Skip block comment and run comment hook."
  (let ((start (point)))
    (or (and (re-search-forward "*/\\|\n" nil t)
             (if (eq ?\C-j (char-before))
                 (progn
                   (jove--set-newline-before state t)
                   (search-forward "*/" nil t))
               t))                    ; Found '*/' without '\n'
        (progn
          (goto-char (point-max))
          (jove--warn (jove--start state) (point-max) "Missing comment closing delimiter")))
    (run-hook-with-args 'jove-comment-hook start (point))))

(defun jove--skip-space (state)
  "Skip whitespace and comments."
  (let (first second (looking t))
    (while (and looking (not (eobp)))
      (skip-syntax-forward " ")
      (setq first (char-after))
      (cond
       ((and (eq ?\/ first) (eq ?\/ (setq second (jove--peek))))
        (jove--skip-line-comment state))
       ((and (eq ?\/ first) (eq ?\* second))
        (jove--skip-block-comment state))
       ((eq ?\C-j first)
        (forward-char)
        (jove--set-newline-before state t))
       (t
        (setq looking nil))))))

;;; Context Functions

(defun jove--current-ctx (state)
  "Return the the top of context stack of lexer STATE."
  (car (jove--ctx-stack state)))

(defun jove--initial-ctx ()
  "Return the initial context for use in `jove-ctx-stack'."
  (list jove-B-STAT))

(defun jove--brace-is-block-p (state prev-tt)
  "Use `jove--current-ctx' and PREV-TT to determine if brace is a block."
  (cond ((eq jove-COLON prev-tt)
         ;; If previous token type is a colon, determine its use as either
         ;; a statement label or a property key in an object literal.
         (let ((parent (jove--current-ctx state)))
           (if (or (eq jove-B-STAT parent)
                   (eq jove-B-EXPR parent))
               (not (jove--ctx-is-expr parent)))))
        ((eq jove-RETURN prev-tt)
         (jove--newline-before state))
        ((memq prev-tt (list jove-ELSE jove-SEMI jove-EOF jove-PAREN-R))
         t)
        ((eq jove-BRACE-L prev-tt)
         (eq jove-B-STAT (jove--current-ctx state)))
        (t
         (not (jove--expr-allowed state)))))

(defun jove--update-ctx (state prev-tt)
  "Modify ctx-stack and expr-allowed of STATE to reflect change of PREV-TT."
  (let ((type (jove--type state)))
    (cond ((and (jove--tt-keyword type)
                (eq jove-DOT prev-tt))
           ;; Don't know what situation this is trying to catch.
           (jove--set-expr-allowed state nil))
          ;; '{'  Enter brace statement, expression context.
          ((eq jove-BRACE-L type)
           (jove--ctx-stack-push state (if (jove--brace-is-block-p state prev-tt)
                                       jove-B-STAT
                                     jove-B-EXPR))
           (jove--set-expr-allowed state t))
          ;; '}' or ')'  Exit either brace or paren context.
          ((or (eq jove-BRACE-R type)
               (eq jove-PAREN-R type))
           (if (= 1 (length (jove--ctx-stack state)))
               (jove--set-expr-allowed state t)
             (let ((out (jove--ctx-stack-pop state)))
               (cond
                ((and (eq jove-B-STAT out)
                      (eq jove-F-EXPR (jove--current-ctx state)))
                 (jove--ctx-stack-pop state) ; Exit of function body.
                 (jove--set-expr-allowed state nil))
                ((eq jove-B-TMPL out)
                 (jove--set-expr-allowed state t))
                (t
                 (jove--set-expr-allowed state (not (jove--ctx-is-expr out))))))))
          ;; ?(  Enter parenthesis context.
          ((eq jove-PAREN-L type)
           (jove--ctx-stack-push state (if (or (eq jove-IF prev-tt)
                                           (eq jove-FOR prev-tt)
                                           (eq jove-WITH prev-tt)
                                           (eq jove-WHILE prev-tt))
                                       jove-P-STAT
                                     jove-P-EXPR))
           (jove--set-expr-allowed state t))
          ;; '${' Enter brace template context.
          ((eq jove-DOLLAR-BRACE-L type)
           (jove--ctx-stack-push state jove-B-TMPL)
           (jove--set-expr-allowed state t))
          ;; '`'  Enter or exit a template literal context.
          ((eq jove-BACKQUOTE type)
           (if (eq jove-Q-TMPL (jove--current-ctx state))
               (jove--ctx-stack-pop state)
             (jove--ctx-stack-push state jove-Q-TMPL))
           (jove--set-expr-allowed state nil))
          ;; '--' or '++'  Do not alter `jove--expr-allowed'.
          ((eq jove-INC-DEC type))
          ;; 'function'  Enter function expression context.
          ((eq jove-FUNCTION type)
           (if (and (jove--tt-before-expr prev-tt)
                    (not (or (eq jove-SEMI prev-tt)
                             (eq jove-ELSE prev-tt)
                             (eq jove-COLON prev-tt)
                             (eq jove-BRACE-L prev-tt)))
                    (eq jove-B-STAT (jove--current-ctx state)))
               (jove--ctx-stack-push state jove-F-EXPR))
           (jove--set-expr-allowed state nil))
          ;; If the token type does not have before-expr in its alist
          ;; `jove--expr-allowed' will be set to nil.
          (t
           (jove--set-expr-allowed state (jove--tt-before-expr type))))))

;;; Token Reading Functions

(defun jove--finish-token (state type &optional value)
  "Finish token of TYPE and value of VALUE then update the context."
  (jove--set-end state (point))
  (let ((prev-tt (jove--type state)))
    (jove--set-type state type)
    (jove--set-value state value)
    (jove--update-ctx state prev-tt)))

(defun jove--finish-punc (state type)
  "Finish punctuation token of TYPE."
  (forward-char)
  (jove--finish-token state type))

(defun jove--finish-op (state type size)
  "Finish operator token of TYPE and SIZE."
  ;; Acorn uses `pos' think `jove--start' should work... Seems to
  (let ((start (jove--start state)))
    (goto-char (+ (point) size))
    ;; Do I need to include a string value for operators?
    (jove--finish-token state type (buffer-substring-no-properties start (+ start size)))))

(defun jove--read-token-dot (state)
  "Read a token starting with a period."
  (let ((next (jove--peek)))
    (cond ((and (characterp next)       ; Protect from end of buffer
                (<= ?0 next ?9))
           (jove--read-number state t))
          ((and (eq ?. next) (eq ?. (jove--peek 2))) ; ...
           (forward-char 3)             ; Why not `jove--finish-op'?
           (jove--finish-token state jove-ELLIPSIS))
          (t
           (forward-char)
           (jove--finish-token state jove-DOT)))))

(defun jove--read-token-slash (state)
  "Read a token starting with a ?\/."
  (let ((next (jove--peek)))
    (cond
     ((or (jove--expr-allowed state))      ; Must be a regular expression.
      (jove--read-regexp state))
     ((eq ?= next)
      (jove--finish-op state jove-ASSIGN 2))
     (t
      (jove--finish-op state jove-SLASH 1)))))

(defun jove--read-token-mult-modulo-exp (state first)
  "Read a token starting with a ?* or ?%, FIRST indicates which."
  (let ((second (jove--peek)))
    (if (eq ?* first)
        ;; * *= ** **=
        (if (eq ?* second)
            (if (eq ?= (jove--peek 2))
                (jove--finish-op state jove-ASSIGN 3)  ; **=
              (jove--finish-op state jove-STARSTAR 2)) ; **
          (if (eq ?= second)
              (jove--finish-op state jove-ASSIGN 2) ; *=
            (jove--finish-op state jove-STAR 1)))   ; *
      ;; % %=
      (if (eq ?= second)
          (jove--finish-op state jove-ASSIGN 2)   ; %=
        (jove--finish-op state jove-MODULO 1))))) ; %

(defun jove--read-token-pipe-amp (state first)
  "Read a token starting with a ?| or ?&, FIRST indicates which."
  (let ((second (jove--peek)))
    (cond ((eq first second)            ; && ||
           (jove--finish-op state (if (eq ?& first) jove-LOGICAL-AND jove-LOGICAL-OR) 2))
          ((eq ?= second)               ; &= |=
           (jove--finish-op state jove-ASSIGN 2))
          (t                            ; & |
           (jove--finish-op state (if (eq ?& first) jove-BITWISE-AND jove-BITWISE-OR) 1)))))

(defun jove--read-token-caret (state)
  "Read a token starting with a ?^."
  (if (eq ?= (jove--peek))
      (jove--finish-op state jove-ASSIGN 2)
    (jove--finish-op state jove-BITWISE-XOR 1)))

(defun jove--read-token-plus-min (state first)
  "Read a token starting with a ?+ or ?-, FIRST indicates which."
  (let ((second (jove--peek)))
    (cond ((eq first second)
           (jove--finish-op state jove-INC-DEC 2))
          ((eq ?= second)
           (jove--finish-op state jove-ASSIGN 2))
          (t
           (jove--finish-op state jove-PLUS-MIN 1)))))

;; Not implementing XML-style comments for now <!--
(defun jove--read-token-lt-gt (state first)
  "Read a token starting with a ?< or ?>, FIRST indicates which."
  (let ((second (jove--peek))
        (third (jove--peek 2)))
    (if (eq first second)
        ;; << >> >>> <<= >>= >>>=
        (if (eq ?> third)
            (if (eq ?= (jove--peek 3))
                (jove--finish-op state jove-ASSIGN 4)  ; >>>=
              (jove--finish-op state jove-BITSHIFT 3)) ; >>>
          (if (eq ?= third)
              (jove--finish-op state jove-ASSIGN 3)    ; <<= >>=
            (jove--finish-op state jove-BITSHIFT 2)))  ; << >>
      ;; < > <= >=
      (jove--finish-op state jove-RELATIONAL (if (eq ?= second) 2 1)))))

(defun jove--read-token-eq-excl (state first)
  "Read a token starting with a ?= or ?!, FIRST indicates which."
  (let ((second (jove--peek)))
    (cond ((eq ?> second)               ; =>
           (forward-char 2)
           (jove--finish-token state jove-ARROW))
          ((eq ?= second)               ; == != === !==
           (jove--finish-op state jove-EQUALITY (if (eq ?= (jove--peek 2)) 3 2)))
          (t                            ; = !
           (jove--finish-op state (if (eq ?= first) jove-EQ jove-PREFIX) 1)))))

(defun jove--buffer-to-number (start end radix)
  "Attempt to read a number from buffer from START to END in RADIX."
  (string-to-number (buffer-substring-no-properties start end) radix))

(defun jove--read-number (state &optional starts-with-dot)
  "Read JavaScript number from the buffer.
STARTS-WITH-DOT indicates the previous character was a period."
  ;; Inputs: '.' or /[0-9]*/
  ;; Attempt to parse as either integer, float, or possible octal.
  (let ((start (point))
        (octal (eq ?0 (char-after)))) ; Would we ever get a zero
    (when (and (not starts-with-dot)  ; to this function?
               ;; Yes we can, `jove--read-zero' passes to this function
               ;; if the number starts with a zero and is not a
               ;; a literal using "0b, 0o, 0x" syntax.
               (not (jove--eat-re jove--decimal-re)))
      (jove--raise 'jove-number-invalid-error (jove--start state) (point)))
    (when (and octal (= (point) (1+ start))) ; A single ?0
      (setq octal nil))                 ; Could a number that starts
    (when (and (eq ?\. (char-after))    ; with a dot fall though?
               (not octal))
      (forward-char)
      (jove--eat-re jove--decimal-re)) ; (jove--eat-re "[0-9]+\\([eE][-+]?[0-9]+\\)?")
    (when (and (memq (char-after) '(?e ?E))
               (not octal))
      (forward-char)
      (when (memq (char-after) '(?+ ?-))
        (forward-char))
      (when (null (jove--eat-re jove--decimal-re))
        (jove--warn (jove--start state) (point) "Invalid float exponent")))
    (jove--finish-token state jove-NUM)
    (when (and (char-after)           ; Protect regex search from nil.
               (jove-identifier-part-p (char-after)))
      ;; If an identifier is found afer a number push a warning, though
      ;; still parse it as an identifier. Maybe change in the future.
      (let ((start (point)))
        (save-excursion
          (skip-syntax-forward "w_")
          (jove--warn start (point) "Unexpected identifier directly after number"))))))

(defun jove--read-zero (state)
  "Read a token starting with a ?0."
  (let ((next (jove--peek 1)))
    (if (memq next '(?x ?X ?o ?O ?b ?B)) ; 0b, 0o, 0x, etc.
        (progn
          (forward-char 2)
          (cond
           ((memq next '(?x ?X))
            (jove--eat-re jove--hexadecimal-re))
           ((memq next '(?o ?O))
            (jove--eat-re jove--octal-re))
           ((memq next '(?b ?B))
            (jove--eat-re jove--binary-re)))
          (when (and (char-after)           ; Protect regex search from nil.
                     (jove-identifier-part-p (char-after)))
            ;; If an identifier is found afer a number push a warning, though
            ;; still parse it as an identifier. Maybe change in the future.
            (let ((start (point)))
              (save-excursion
                (skip-syntax-forward "w_")
                (jove--warn start (point) "Unexpected identifier directly after number"))))
          (jove--finish-token state jove-NUM))
      (jove--read-number state nil))))

(defun jove--read-code-point ()
  "Read Unicode escape from buffer.
Return an integer representing the escape if valid, otherwise nil."
  (let ((code nil)
        (start (point)))
    (if (eq ?{ (char-after))
        ;; Unicode Code Point Escape \u{...}
        (progn
          (forward-char)                ; Move over '{'
          (when (jove--eat-re jove--hexadecimal-re)
            (setq code (jove--buffer-to-number (1+ start) (point) 16)))
          (cond
           ((not code)
            (jove--warn (- start 2) (point) "Invalid Unicode code point"))
           ((> code #x10ffff)
            (jove--warn (- start 2) (point) "Unicode code point out of bounds"))
           (t
            (jove--expect-char ?})
            code)))
      ;; Unicode Escape Sequence \uXXXX
      (if (jove--eat-re "[0-9A-Fa-f]\\{4\\}")
          (jove--buffer-to-number start (point) 16)
        (jove--eat-re "[0-9A-Fa-f]\\{1,3\\}")
        (jove--warn (- start 2) (point) "Invalid Unicode escape sequence")))))

(defun jove--read-escape-char (&optional in-template)
  "Read ECMAScript escape sequence from buffer.
The IN-TEMPLATE option invalidates the use of octal literals in the string."
  (forward-char)                        ; Move over '\'
  (let ((start (1- (point)))
        (char (char-after)))
    ;; If invalid escapes are found attempt to warn about them.
    (cond ((eq ?u char)
           (forward-char)
           (jove--read-code-point))
          ((eq ?x char)
           (forward-char)
           (unless (jove--eat-re "[0-9A-Fa-f]\\{2\\}")
             (jove--eat-re jove--hexadecimal-re)
             (jove--warn start (point) "Invalid hexadecimal escape")))
          ((<= ?0 char ?7)
           (jove--eat-re "[0-7]\\{1,3\\}")
           (when (or jove--strict
                     in-template)
             (jove--warn start (point) "Octal in template string")))
          (t                            ; Any other escape
           (forward-char)))))

(defun jove--read-regexp (state)
  "Read regular expression.
Criteria include ending delimiter and flags."
  (forward-char)                        ; Move over opening delimiter.
  (let (char
        in-class
        (looking t))
    (while looking
      ;; Advance to next critical character.
      (re-search-forward "[^\][/\\\\\C-j]*" nil t) ; ][/\ or \n
      (setq char (char-after))
      (cond
       ;; Found escape.
       ((eq ?\\ char)
        (jove--read-escape-char nil))
       ;; Found closing delimiter, exit the loop if not in a class.
       ((eq ?\/ char)
        (forward-char)
        (unless in-class (setq looking nil)))
       ((and (not in-class) (eq ?\[ char))
        (forward-char)
        (setq in-class t))              ; Enter character class
       ((and in-class (eq ?\] char))
        (forward-char)
        (setq in-class nil))            ; Exit character class
       ;; Hit eol or eof, signal error.
       (t
        (setq looking nil)
        (jove--warn (jove--start state) (point) "Missing regular expression closing delimiter")))))
  (skip-syntax-forward "w")       ; Advance over flags, valid or not
  (jove--finish-token state jove-REGEXP)
  (run-hooks 'jove-regexp-hook))            ; Will need to pass state and end to hooks???

(defun jove--read-string (state punc)
  "Search for ending delimiter matching PUNC.
Signal error if the eol or eof is reached before the matching
delimiter."
  (forward-char)                        ; Move over opening delimiter.
  (let (char
        (regexp (concat "[^" (string punc) "\\\\\C-j]*"))
        (looking t))
    (while looking
      ;; Advance to next critical character.
      (re-search-forward regexp nil t)
      (setq char (char-after))
      (cond
       ;; Found escape.
       ((eq ?\\ char)
        (jove--read-escape-char nil))
       ;; Found closing delimiter, exit the loop.
       ((eq punc char)
        (forward-char)
        (setq looking nil))
       ;; Hit eol or eof, signal error.
       (t
        (setq looking nil)
        (jove--warn (jove--start state) (point) "Missing string closing delimiter")))))
  (jove--finish-token state jove-STRING)
  (run-hooks 'jove-string-hook))            ; Will need to pass start and end to hooks???

(defun jove--string-builder (list)
  "Return a string built from a LIST of strings."
  (eval `(concat ,@list)))

(defun jove--read-tmpl-token (state)
  "Read template string tokens."
  (let (char)
    (catch 'token
      (while t
        (re-search-forward "[^`$\\\\]*" nil t)
        (setq char (char-after))
        (cond
         ((eq ?\` char)
          (if (and (= (jove--start state) (point))
                   (eq jove-TEMPLATE (jove--type state)))
              (progn
                (forward-char)
                (jove--finish-token state jove-BACKQUOTE)
                (run-hooks 'jove-template-hook) ; Will need to pass start and end to hooks???
                (throw 'token nil))
            (jove--finish-token state jove-TEMPLATE)
            (run-hooks 'jove-template-hook)
            (throw 'token nil)))
         ((eq ?\$ char)
          (if (eq ?\{ (jove--peek))
              (if (and (= (jove--start state) (point))
                       (eq jove-TEMPLATE (jove--type state)))
                  (progn
                    (forward-char 2)
                    (jove--finish-token state jove-DOLLAR-BRACE-L)
                    (throw 'token nil))
                (jove--finish-token state jove-TEMPLATE)
                (run-hooks 'jove-template-hook)
                (throw 'token nil))
            ;; Its possible to catch a single '$' which is part of the
            ;; literal template string. So it is necessary to always
            ;; advance at least one character.
            (forward-char)))
         ((eq ?\\ char)
          (jove--read-escape-char t))
         (t                             ; Hit eof.
          ;; Don't run the template hook because this isn't a real template literal.
          (jove--warn (jove--start state) (point-max) "Missing template string closing delimiter")
          (jove--finish-token state jove-TEMPLATE)
          (throw 'token nil)))))))

(defun jove--read-word-escape (state)
  "Read an escape in an identifier."
  ;; Already know there was a '\'
  (forward-char 2)
  (if (eq ?u (char-before))
      (progn
        (jove--set-contains-esc state t)
        ;; The function below will warn of any invalid code points.
        (jove--read-code-point))
    (jove--warn (- (point) 2) (point) "Invalid escape in identifier")))

(defun jove--read-word-internal (state)
  "Read ECMAScript Identifier."
  (jove--set-contains-esc state nil)
  (let (word
        chunk
        invalid
        (looking t)
        (start (point)))
    ;; `jove-identifier-start-p' was already varified for a regular
    ;; character. Need to check for code point escapes.
    (when (eq ?\\ (char-after))
      (setq chunk (jove--read-word-escape state))
      (setq invalid (or invalid
                        (not chunk)
                        (not (jove-identifier-start-p chunk))))
      (if invalid
          (jove--warn start (point) "Invalid identifier start character")
        ;; Should I use the string builder here?
        (push (string chunk) word))) ; Here chunk is a character
    (setq start (point))
    (while looking
      (cond
       ((eq ?\\ (char-after))
        (jove--set-contains-esc state t)
        (setq chunk (jove--read-word-escape state))
        (if (not chunk)
            (setq invalid t)
          ;; regexp func protected by above if condition
          (when (not (jove-identifier-part-p chunk))
            (setq invalid t)
            (jove--warn start (point) "Invalid identifier character")))
        (unless invalid
          (push (string chunk) word)))
       ((< 0 (skip-syntax-forward "w_"))
        (unless invalid
          (push (buffer-substring-no-properties start (point)) word)))
       (t
        (setq looking nil)))
      (setq start (point)))
    ;; If unable to properly parse an escape return nil,
    ;; the word is still moved over. Warning are added
    ;; and the word can not be considered a keyword.
    (unless invalid
      (apply #'concat (nreverse word)))))

(defun jove--read-word (state)
  "Read from buffer an ECMAScript Identifier Name or Keyword."
  (let ((word (jove--read-word-internal state))
        (type jove-NAME))
    (when (and word                     ; If word is nil just highlight it.
               (or (>= jove-ecma-version 6)
                   (not jove--contains-esc)))
      (setq type (or (gethash (intern word) jove-keywords)
                     jove-NAME)))
    (jove--finish-token state type word)
    (when (jove--tt-keyword type)
      (run-hooks 'jove-keyword-hook))))

(defun jove--read-token (state char)
  "Read token from CHAR."
  ;; Implements getTokenFromCode from acorn.
  (cond
   ((eq ?\. char) (jove--read-token-dot state))
   ((eq ?\( char) (jove--finish-punc state jove-PAREN-L))
   ((eq ?\) char) (jove--finish-punc state jove-PAREN-R))
   ((eq ?\; char) (jove--finish-punc state jove-SEMI))
   ((eq ?\, char) (jove--finish-punc state jove-COMMA))
   ((eq ?\[ char) (jove--finish-punc state jove-BRACKET-L))
   ((eq ?\] char) (jove--finish-punc state jove-BRACKET-R))
   ((eq ?{ char) (jove--finish-punc state jove-BRACE-L))
   ((eq ?} char) (jove--finish-punc state jove-BRACE-R))
   ((eq ?? char) (jove--finish-punc state jove-QUESTION))
   ((eq ?: char) (jove--finish-punc state jove-COLON))
   ((and (eq ?\` char) (<= 6 jove-ecma-version))
    (jove--finish-punc state jove-BACKQUOTE)
    (run-hooks 'jove-template-hook))
   ((eq ?0 char) (jove--read-zero state))
   ((<= ?1 char ?9)  (jove--read-number state nil))
   ((or (eq ?\' char) (eq ?\" char)) (jove--read-string state char))
   ((eq ?\/ char) (jove--read-token-slash state))
   ((memq char '(?* ?%)) (jove--read-token-mult-modulo-exp state char))
   ((memq char '(?\| ?\&)) (jove--read-token-pipe-amp state char))
   ((eq ?^ char) (jove--read-token-caret state))
   ((memq char '(?+ ?-)) (jove--read-token-plus-min state char))
   ((memq char '(?< ?>)) (jove--read-token-lt-gt state char))
   ((memq char '(?= ?!)) (jove--read-token-eq-excl state char))
   ((eq ?~ char) (jove--finish-op state jove-PREFIX 1))
   (t
    ;; Just skip and try again.
    (jove--warn (point) (1+ (point)) "Unexpected character")
    (forward-char)
    (jove--next state))))

(defun jove--next (prev-state)
  "Transition from PREV-STATE, read next token and return new state."
  ;; Load current token into parser state.
  (let ((char nil)
        (ctx (jove--current-ctx prev-state))
        (state (vconcat prev-state)))
    (when (or (not ctx)
              (not (jove--ctx-preserve-space ctx)))
      (jove--set-newline-before state nil)
      (jove--skip-space state))
    ;; At this point, if there is a newline, I want to save the state
    ;; in order to load it after a change.
    ;; Initialize current token state.
    (jove--set-start state (point))
    (jove--set-value state nil)
    (setq char (char-after))
    (cond
     ((eobp)
      (jove--finish-token state jove-EOF))
     ((jove--ctx-override ctx)
      (funcall (jove--ctx-override ctx) state))
     ((or (jove-identifier-start-p char)    ; Identifiers
          (eq ?\\ char))
      (jove--read-word state))
     (t
      (jove--read-token state char)))
    state))                             ; Return lexer state vector

;; Just for testing the lexer.
(defun jove-test ()
  (save-restriction
    (widen)
    (save-excursion
      (goto-char 1)
      (let ((count 0)
            (looping t)
            (state (jove--lex-create))
            (start-time (float-time))
            (list '()))
        (save-match-data
          (setq state (jove--next state))     ; Right now the loop wants to quite on first token.
          (push state list)
          (while looping
            (if (eq jove-EOF (jove--type state))
                (setq looping nil
                      count (1+ count))
              (setq state (jove--next state)
                    count (1+ count))
              (push state list))))
        (let ((time (/ (truncate (* (- (float-time) start-time)
                                       10000))
                          10000.0)))
          (message "Finished in %0.3fsec Count: %d" time count))
        (setq lexer-cache (vconcat (nreverse list)))
        state))))

(provide 'jove-lexer)

;;; jove-lexer.el ends here
