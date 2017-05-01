;;;; er.el --- The Jove Lexer -*- lexical-binding: t; -*-

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

(require 'jove-vars)

(defsubst jove-clear-face (start end)
  "Remove face properties from START to END."
  (remove-text-properties start end '(font-lock-face nil)))

(defun jove-apply-fontifications (start end &optional no-clear)
  "Apply fontifications from START to END.
Boolean NO-CLEAR flag prevents clearing faces before application."
  (with-silent-modifications
    (unless no-clear (jove-clear-face start end))
    (mapc #'(lambda (f)
              (put-text-property (aref f 0) (aref f 1) 'font-lock-face (aref f 2)))
          (nreverse jove--fontifications))  ; Allows applying over those previously pushed.
    (mapc #'(lambda (f)
              (put-text-property (aref f 0) (aref f 1) 'font-lock-face font-lock-warning-face))
          jove--warnings)
    (setq jove--fontifications nil
          jove--warnings nil)))     ; Probably shouldn't reset here.

(defun jove-set-face (start end face)
  "Queue region START to END for fontification using FACE."
  (setq start (min (point-max) start)
        start (max (point-min) start)
        end (min (point-max) end)
        end (max (point-min) end))
  (push (vector start end face) jove--fontifications))

(defsubst jove-token-set-face (token face)
  "Queue TOKEN region for fontification using FACE."
  ;;  `jove-start' is not defined yet here.
  (jove-set-face (jove-start token) (jove-end token) face))

(defsubst jove-node-set-face (node face)
  "Queue NODE region for fontification using FACE."
  ;;  `jove-start' is not defined yet here.
  (jove-set-face (aref (car (car node)) 0) (aref (car (car node)) 1) face))

(defsubst jove-set-face* (vec face)
  "Queue region for fontification using FACE.
Use location data in `jove-start' and `jove-end' of VEC."
  ;;  `jove-start' is not defined yet here.
  (jove-set-face (jove-start vec) (jove-end vec) face))

;;; Token Types

;; Quoted from acorn/src/tokentype.js

;;   "The assignment of fine-grained, information-carrying type objects
;;    allows the tokenizer to store the information it has about a
;;    token in a way that is very cheap for the parser to look up.

;;    All token type variables start with an underscore, to make them
;;    easy to recognize.

;;    The `beforeExpr` property is used to disambiguate between regular
;;    expressions and divisions. It is set on all token types that can
;;    be followed by an expression (thus, a slash after them would be a
;;    regular expression).
;;
;;    The `startsExpr` property is used to check if the token ends a
;;    `yield` expression. It is set on all token types that either can
;;    directly start an expression (like a quotation mark) or can
;;    continue an expression (like the body of a string).
;;
;;    `isLoop` marks a keyword as starting a loop, which is important
;;    to know when parsing a label, in order to allow or disallow
;;    continue jumps to that label."

(cl-defun jove-tt-make (label &key keyword before-expr starts-expr
                          is-loop is-assign prefix postfix binop)
  "Return a vector representing a token type."
  (vector label                         ; 0
          keyword                       ; 1
          before-expr                   ; 2
          starts-expr                   ; 3
          is-loop                       ; 4
          is-assign                     ; 5
          prefix                        ; 6
          postfix                       ; 7
          binop))                       ; 8

(defsubst jove-tt-label (tt)
  "Return the 'label' slot of the token type TT."
  (aref tt 0))
(defsubst jove-tt-keyword (tt)
  "Return the 'keyword' slot of the token type TT."
  (aref tt 1))
(defsubst jove-tt-before-expr (tt)
  "Return the 'before-expr' slot of the token type TT."
  (aref tt 2))
(defsubst jove-tt-starts-expr (tt)
  "Return the 'starts-expr' slot of the token type TT."
  (aref tt 3))
(defsubst jove-tt-is-loop (tt)
  "Return the 'is-loop' slot of the token type TT."
  (aref tt 4))
(defsubst jove-tt-is-assign (tt)
  "Return the 'is-assign' slot of the token type TT."
  (aref tt 5))
(defsubst jove-tt-prefix (tt)
  "Return the 'prefix' slot of the token type TT."
  (aref tt 6))
(defsubst jove-tt-postfix (tt)
  "Return the 'postfix' slot of the token type TT."
  (aref tt 7))
(defsubst jove-tt-binop (tt)
  "Return the 'binop' slot of the token type TT."
  (aref tt 8))

(defun jove-binop-make (label prec)
  "Return a vector representing a binary operator token type.
LABEL should be a string and PREC a number for operator precedence."
  (jove-tt-make label :before-expr t :binop prec))

(defmacro jove-keyword-make (label &rest options)
  "Return a vector representing a keyword token type.
LABEL should be a string and the remaining arguments are key value
pairs collected in OPTIONS."
  `(puthash (intern ,label)
            (jove-tt-make ,label :keyword t ,@options)
            jove-keywords))

(defvar jove-keywords (make-hash-table :test 'equal)
  "Hash table to map keyword names to token types.")

(defconst jove-NUM (jove-tt-make "num" :starts-expr t))
(defconst jove-REGEXP (jove-tt-make "regexp" :starts-expr t))
(defconst jove-STRING (jove-tt-make "string" :starts-expr t))
(defconst jove-NAME (jove-tt-make "name" :starts-expr t))
(defconst jove-EOF (jove-tt-make "eof"))

;;; Punctuation Token Types

(defconst jove-BRACE-L (jove-tt-make "{" :before-expr t :starts-expr t))
(defconst jove-BRACE-R (jove-tt-make "}"))
(defconst jove-PAREN-L (jove-tt-make "(" :before-expr t :starts-expr t))
(defconst jove-PAREN-R (jove-tt-make ")"))
(defconst jove-BRACKET-L (jove-tt-make "[" :before-expr t :starts-expr t))
(defconst jove-BRACKET-R (jove-tt-make "]"))
(defconst jove-COMMA (jove-tt-make "," :before-expr t))
(defconst jove-SEMI (jove-tt-make ";" :before-expr t))
(defconst jove-COLON (jove-tt-make ":" :before-expr t))
(defconst jove-DOT (jove-tt-make "."))
(defconst jove-QUESTION (jove-tt-make "?" :before-expr t))
(defconst jove-ARROW (jove-tt-make "=>" :before-expr t))
(defconst jove-TEMPLATE (jove-tt-make "template"))
(defconst jove-ELLIPSIS (jove-tt-make "..." :before-expr t))
(defconst jove-DOLLAR-BRACE-L (jove-tt-make "${" :before-expr t :starts-expr t))
(defconst jove-BACKQUOTE (jove-tt-make "`" :before-expr t))

;;; Operator Token Types

;; Quoted from acorn/src/tokentypes.js

;;  "Operators. These carry several kinds of properties to help the
;;   parser use them properly (the presence of these properties is
;;   what categorizes them as operators).
;;
;;   `binop`, when present, specifies that this operator is a binary
;;   operator, and will refer to its precedence.
;;
;;   `prefix` and `postfix` mark the operator as a prefix or postfix
;;   unary operator.
;;
;;   `isAssign` marks all of `=`, `+=`, `-=` etcetera, which act as
;;   binary operators with a very low precedence, that should result
;;   in AssignmentExpression nodes."

(defconst jove-EQ (jove-tt-make "=" :before-expr t :is-assign t))
(defconst jove-ASSIGN (jove-tt-make "_=" :before-expr t :is-assign t))
(defconst jove-INC-DEC (jove-tt-make "++/--" :starts-expr t :prefix t :postfix t))
(defconst jove-PREFIX (jove-tt-make "prefix" :before-expr t :starts-expr t :prefix t))
(defconst jove-LOGICAL-OR (jove-binop-make "||" 1))
(defconst jove-LOGICAL-AND (jove-binop-make "&&" 2))
(defconst jove-BITWISE-OR (jove-binop-make "|" 3))
(defconst jove-BITWISE-XOR (jove-binop-make "^" 4))
(defconst jove-BITWISE-AND (jove-binop-make "&" 5))
(defconst jove-EQUALITY (jove-binop-make "==/!=" 6)) ; == != === !==
(defconst jove-RELATIONAL (jove-binop-make "</>" 7)) ; < > <= >=
(defconst jove-BITSHIFT (jove-binop-make "<</>>" 8)) ; << >> >>>
(defconst jove-PLUS-MIN (jove-tt-make "+/-"
                              :binop 9
                              :before-expr t
                              :starts-expr t
                              :prefix t))
(defconst jove-MODULO (jove-binop-make "%" 10))
(defconst jove-STAR (jove-binop-make "*" 10))
(defconst jove-SLASH (jove-binop-make "/" 10))
(defconst jove-STARSTAR (jove-tt-make "**" :before-expr t))

;;; Keywords Token Types

(defconst jove-BREAK (jove-keyword-make "break"))
(defconst jove-CASE (jove-keyword-make "case" :before-expr t))
(defconst jove-CATCH (jove-keyword-make "catch"))
(defconst jove-CONTINUE (jove-keyword-make "continue"))
(defconst jove-DEBUGGER (jove-keyword-make "debugger"))
(defconst jove-DEFAULT (jove-keyword-make "default"))
(defconst jove-DO (jove-keyword-make "do" :is-loop t :before-expr t))
(defconst jove-ELSE (jove-keyword-make "else" :before-expr t))
(defconst jove-FINALLY (jove-keyword-make "finally"))
(defconst jove-FOR (jove-keyword-make "for" :is-loop t))
(defconst jove-FUNCTION (jove-keyword-make "function" :starts-expr t))
(defconst jove-IF (jove-keyword-make "if"))
(defconst jove-RETURN (jove-keyword-make "return" :before-expr t))
(defconst jove-SWITCH (jove-keyword-make "switch"))
(defconst jove-THROW (jove-keyword-make "throw" :before-expr t))
(defconst jove-TRY (jove-keyword-make "try"))
(defconst jove-VAR (jove-keyword-make "var"))
(defconst jove-CONST (jove-keyword-make "const"))
(defconst jove-WHILE (jove-keyword-make "while" :is-loop t))
(defconst jove-WITH (jove-keyword-make "with"))
(defconst jove-NEW (jove-keyword-make "new" :before-expr t :starts-expr t))
(defconst jove-THIS (jove-keyword-make "this" :starts-expr t))
(defconst jove-SUPER (jove-keyword-make "super" :starts-expr t))
(defconst jove-CLASS (jove-keyword-make "class"))
(defconst jove-EXTENDS (jove-keyword-make "extends" :before-expr t))
(defconst jove-EXPORT (jove-keyword-make "export"))
(defconst jove-IMPORT (jove-keyword-make "import"))
(defconst jove-NULL (jove-keyword-make "null" :starts-expr t))
(defconst jove-UNDEFINED (jove-keyword-make "undefined" :starts-expr t))
(defconst jove-TRUE (jove-keyword-make "true" :starts-expr t))
(defconst jove-FALSE (jove-keyword-make "false" :starts-expr t))
(defconst jove-IN (jove-keyword-make "in" :binop 7 :before-expr t))
(defconst jove-INSTANCEOF (jove-keyword-make "instanceof" :binop 7 :before-expr t))
(defconst jove-TYPEOF (jove-keyword-make "typeof" :prefix t :before-expr t :starts-expr t))
(defconst jove-VOID (jove-keyword-make "void" :prefix t :before-expr t :starts-expr t))
(defconst jove-DELETE (jove-keyword-make "delete" :prefix t :before-expr t :starts-expr t))

;;; Context Types

(cl-defun jove-ctx-make (token &key is-expr preserve-space override)
  "Return a vector representing a context type."
  (vector token                         ; 0
          is-expr                       ; 1
          preserve-space                ; 2
          override))                    ; 3

(defsubst jove-ctx-token (ctx)
  "Return the 'token' slot of the context type CTX."
  (aref ctx 0))
(defsubst jove-ctx-is-expr (ctx)
  "Return the 'is-expr' slot of the context type CTX."
  (aref ctx 1))
(defsubst jove-ctx-preserve-space (ctx)
  "Return the 'preserve-space' slot of the context type CTX."
  (aref ctx 2))
(defsubst jove-ctx-override (ctx)
  "Return the 'override' slot of the context type CTX."
  (aref ctx 3))

(defvar jove-B-STAT (jove-ctx-make "{"))
(defvar jove-B-EXPR (jove-ctx-make "{" :is-expr t))
(defvar jove-B-TMPL (jove-ctx-make "${" :is-expr t))
(defvar jove-P-STAT (jove-ctx-make "("))
(defvar jove-P-EXPR (jove-ctx-make "(" :is-expr t))
(defvar jove-F-EXPR (jove-ctx-make "function" :is-expr t))
(defvar jove-Q-TMPL (jove-ctx-make "`" :is-expr t :preserve-space t :override #'jove-read-tmpl-token))

;;; Token

;; Use `point' to track the current position of the lexer in the buffer.
;; The logic behind using `point', is the relative ease of use to apply
;; elisp regexp functions when reading tokens in the buffer.

;; Unlike Acorn, this parser is working inside an Emacs buffer, in
;; which positions are between characters and start at 1

(defun jove-token-make ()
  "Return a vector representing a token.
Includes full lexer state data, which can be used as an argument
to `jove-next-token' to resume tokenization."
  (vector 'jove-token                       ; 0
          1                             ; 1 start
          1                             ; 2 end
          jove-EOF                          ; 3 token type (tt)
          nil                           ; 4 value
          nil                           ; 5 newline-before
          (jove-initial-ctx)                ; 6 ctx-stack
          t                             ; 7 expr-allowed
          nil))                         ; 8 contains-esc

;; NOTE: Use `vconcat' to copy previous state.

(defun jove-token-p (object)
  "Return t if OBJECT is a token."
  (eq 'jove-token (aref object 0)))

;;; NOTE: `start', `end', and `type' work on tokens and nodes.

(defun jove-start (vec)
  "Return the 'start' slot of the VEC."
  (aref vec 1))
(defun jove-set-start (vec value)
  "Set the 'start' slot of the VEC to VALUE."
  (aset vec 1 value))

(defun jove-end (vec)
  "Return the 'end' slot of the VEC."
  (aref vec 2))
(defun jove-set-end (vec value)
  "Set the 'end' slot of the VEC to VALUE."
  (aset vec 2 value))

(defun jove-type (vec)
  "Return the 'type' slot of the VEC."
  (aref vec 3))
(defun jove-set-type (vec value)
  "Set the 'type' slot of the VEC to VALUE."
  (aset vec 3 value))

(defalias 'jove-tt 'jove-type)
(defalias 'jove-set-tt 'jove-set-type)

;;; Token Specific Getter and Setter Functions:

(defsubst jove-value (token)
  "Return the 'value' slot of the lexer TOKEN."
  (aref token 4))
(defsubst jove-set-value (token value)
  "Set the 'value' slot of the lexer TOKEN to VALUE."
  (aset token 4 value))

(defsubst jove-newline-before (token)
  "Return the 'newline-before' slot of the lexer TOKEN."
  (aref token 5))
(defsubst jove-set-newline-before (token value)
  "Set the 'newline-before' slot of the lexer TOKEN to VALUE."
  (aset token 5 value))

;; Quoted acorn/src/state.js
;;
;;   "The context stack is used to superficially track syntactic
;;    context to predict whether a regular expression is allowed in a
;;    given position."

(defsubst jove-ctx-stack (token)
  "Return the 'ctx-stack' slot of the lexer TOKEN."
  (aref token 6))
(defsubst jove-ctx-stack-push (token value)
  "Push on to the 'ctx-stack' of the lexer TOKEN a VALUE."
  (aset token 6 (cons value (aref token 6))))
(defsubst jove-ctx-stack-pop (token)
  "Pop the 'ctx-stack' of the lexer TOKEN."
  (prog1 (car (aref token 6))
    (aset token 6 (cdr (aref token 6)))))

(defsubst jove-expr-allowed (token)
  "Return the 'expr-allowed' slot of the lexer TOKEN."
  (aref token 7))
(defsubst jove-set-expr-allowed (token value)
  "Set the 'expr-allowed' slot of the lexer TOKEN to VALUE."
  (aset token 7 value))

(defsubst jove-contains-esc (token)
  "Return the 'contains-esc' slot of the lexer TOKEN."
  (aref token 8))
(defsubst jove-set-contains-esc (token value)
  "Set the 'contains-esc' slot of the lexer TOKEN to VALUE."
  (aset token 8 value))

;;; Utility Functions

(defun jove-warn (start end message)
  "Queue a warning from START to END with MESSAGE.
Push the warning into the list `jove--warnings'."
  (push (vector start end message) jove--warnings)
  nil)                                  ; Return nil

(defun jove-unexpected-char ()
  "Signal unexpected character error at current position."
  (jove-warn (point) (1+ (point)) "unexpected char")
  (forward-char))

;; http://nullprogram.com/blog/2017/01/30/
;; Builting a list and using `nreverse' is the correct way to build a
;; list in reverse. `nreverse' is a very fast C builtin function.

(defun jove-collect-string ()
  "Convert `jove--string-buffer', a list of strings, to a string.
Reverse the list before converting."
  (if jove--string-buffer
      (prog1 ;; Wouldn't `concat' work?
          (apply #'concat (nreverse jove--string-buffer))
        (setq jove--string-buffer nil))
    ""))

(defsubst jove-add-to-string (chunk)
  "Add CHUCK to `jove--string-buffer'."
  (push chunk jove--string-buffer))

;;; Lexer Utility Functions - Movement

(defun jove-peek-char (&optional count)
  "Peek forward one or COUNT number of `char-after' point.
This function does not move point."
  (char-after (+ (point) (or count 1))))

(defun jove-match (regexp)
  "Return the `match-end' if text after point match REGEXP."
  ;; NOTE: `looking-at-p' does not update match data.
  (and (looking-at regexp)
       (match-end 0)))

(defun jove-eat-re (regexp)
  "Goto the `match-end' if text after point match REGEXP.
Set point to the end of match, and return point."
  (and (looking-at regexp)
       (goto-char (match-end 0))))

(defun jove-expect-char (character)
  "Return t and `forward-char' if `char-after' point is eq CHARACTER.
Otherwise signal `jove-unexpected-character-error'."
  (if (eq character (char-after))
      (progn (forward-char) t)
    (jove-unexpected-char)))

(defun jove-skip-line-comment (state)
  "Skip line comment and run comment hook."
  (let ((start (point)))
    (if (search-forward "\n" nil t)
        (jove-set-newline-before state t)
      (goto-char (point-max)))
    (jove-set-face start (point) font-lock-comment-face)
    (run-hook-with-args 'jove-comment-hook start (point))))

(defun jove-skip-block-comment (state)
  "Skip block comment and run comment hook."
  (let ((start (point)))
    (or (and (re-search-forward "*/\\|\n" nil t)
             (if (eq ?\C-j (char-before))
                 (progn
                   (jove-set-newline-before state t)
                   (search-forward "*/" nil t))
               t))                    ; Found '*/' without '\n'
        (progn
          (goto-char (point-max))
          (jove-warn start (point-max) "Missing comment closing delimiter")))
    (jove-set-face start (point) font-lock-comment-face)
    (run-hook-with-args 'jove-comment-hook start (point))))

(defun jove-skip-space (state)
  "Skip whitespace and comments."
  (let (first second (looking t))
    (while (and looking (not (eobp)))
      (skip-syntax-forward " ")
      (setq first (char-after))
      (cond
       ((and (eq ?\/ first) (eq ?\/ (setq second (jove-peek-char))))
        (jove-skip-line-comment state))
       ((and (eq ?\/ first) (eq ?\* second))
        (jove-skip-block-comment state))
       ((eq ?\C-j first)
        (forward-char)
        (jove-set-newline-before state t))
       (t
        (setq looking nil))))))

;;; Context Functions

(defun jove-current-ctx (state)
  "Return the the top of context stack of lexer STATE."
  (car (jove-ctx-stack state)))

(defun jove-initial-ctx ()
  "Return the initial context for use in `jove-ctx-stack'."
  (list jove-B-STAT))

(defun jove-brace-is-block-p (state prev-tt)
  "Use `jove-current-ctx' and PREV-TT to determine if brace is a block."
  (cond
   ((eq jove-COLON prev-tt)
    ;; If previous token type is a colon, determine its use as either
    ;; a statement label or a property key in an object literal.
    (let ((parent (jove-current-ctx state)))
      (if (or (eq jove-B-STAT parent)
              (eq jove-B-EXPR parent))
          (not (jove-ctx-is-expr parent)))))
   ((eq jove-RETURN prev-tt)
    (jove-newline-before state))
   ((memq prev-tt (list jove-ELSE jove-SEMI jove-EOF jove-PAREN-R))
    t)
   ((eq jove-BRACE-L prev-tt)
    (eq jove-B-STAT (jove-current-ctx state)))
   (t
    (not (jove-expr-allowed state)))))

(defun jove-update-ctx (state prev-tt)
  "Modify ctx-stack and expr-allowed of STATE to reflect change of PREV-TT."
  (let ((type (jove-tt state)))
    (cond
     ((and (jove-tt-keyword type)
           (eq jove-DOT prev-tt))
      ;; Don't know what situation this is trying to catch.
      (jove-set-expr-allowed state nil))
     ;; '{'  Enter brace statement, expression context.
     ((eq jove-BRACE-L type)
      (jove-ctx-stack-push state (if (jove-brace-is-block-p state prev-tt)
                                 jove-B-STAT
                               jove-B-EXPR))
      (jove-set-expr-allowed state t))
     ;; '}' or ')'  Exit either brace or paren context.
     ((or (eq jove-BRACE-R type)
          (eq jove-PAREN-R type))
      (if (= 1 (length (jove-ctx-stack state)))
          (jove-set-expr-allowed state t)
        (let ((out (jove-ctx-stack-pop state)))
          (cond
           ((and (eq jove-B-STAT out)
                 (eq jove-F-EXPR (jove-current-ctx state)))
            (jove-ctx-stack-pop state) ; Exit of function body.
            (jove-set-expr-allowed state nil))
           ((eq jove-B-TMPL out)
            (jove-set-expr-allowed state t))
           (t
            (jove-set-expr-allowed state (not (jove-ctx-is-expr out))))))))
     ;; ?(  Enter parenthesis context.
     ((eq jove-PAREN-L type)
      (jove-ctx-stack-push state (if (or (eq jove-IF prev-tt)
                                     (eq jove-FOR prev-tt)
                                     (eq jove-WITH prev-tt)
                                     (eq jove-WHILE prev-tt))
                                 jove-P-STAT
                               jove-P-EXPR))
      (jove-set-expr-allowed state t))
     ;; '${' Enter brace template context.
     ((eq jove-DOLLAR-BRACE-L type)
      (jove-ctx-stack-push state jove-B-TMPL)
      (jove-set-expr-allowed state t))
     ;; '`'  Enter or exit a template literal context.
     ((eq jove-BACKQUOTE type)
      (if (eq jove-Q-TMPL (jove-current-ctx state))
          (jove-ctx-stack-pop state)
        (jove-ctx-stack-push state jove-Q-TMPL))
      (jove-set-expr-allowed state nil))
     ;; '--' or '++'  Do not alter `jove-expr-allowed'.
     ((eq jove-INC-DEC type))
     ;; 'function'  Enter function expression context.
     ((eq jove-FUNCTION type)
      (if (and (jove-tt-before-expr prev-tt)
               (not (or (eq jove-SEMI prev-tt)
                        (eq jove-ELSE prev-tt)
                        (eq jove-COLON prev-tt)
                        (eq jove-BRACE-L prev-tt)))
               (eq jove-B-STAT (jove-current-ctx state)))
          (jove-ctx-stack-push state jove-F-EXPR))
      (jove-set-expr-allowed state nil))
     ;; If the token type does not have before-expr in its alist
     ;; `jove-expr-allowed' will be set to nil.
     (t
      (jove-set-expr-allowed state (jove-tt-before-expr type))))))

;;; Token Reading Functions

(defun jove-finish-token (state type &optional value)
  "Finish token of TYPE and value of VALUE then update the context."
  (jove-set-end state (point))
  (let ((prev-tt (jove-tt state)))
    (jove-set-tt state type)
    (jove-set-value state value)
    (jove-update-ctx state prev-tt)))

(defun jove-finish-punc (state type)
  "Finish punctuation token of TYPE."
  (forward-char)
  (jove-finish-token state type))

(defun jove-finish-op (state type size)
  "Finish operator token of TYPE and SIZE."
  (let ((end (+ (point) size)))
    (goto-char end)
    (jove-finish-token state
                   type
                   (buffer-substring-no-properties (jove-start state)
                                                   end))))

(defun jove-read-token-dot (state)
  "Read a token starting with a period."
  (cond ((jove-eat-re ".[0-9]+\\(?:[eE][-+]?[0-9]+\\)?")
         (jove-finish-token state jove-NUM))
        ((and (eq ?. (jove-peek-char)) (eq ?. (jove-peek-char 2))) ; ...
         (forward-char 3)             ; Why not `jove-finish-op'?
         (jove-finish-token state jove-ELLIPSIS))
        (t
         (forward-char)
         (jove-finish-token state jove-DOT))))

(defun jove-read-token-slash (state)
  "Read a token starting with a ?\/."
  (let ((next (jove-peek-char)))
    (cond
     ((or (jove-expr-allowed state))      ; Must be a regular expression.
      (jove-read-regexp state))
     ((eq ?= next)
      (jove-finish-op state jove-ASSIGN 2))
     (t
      (jove-finish-op state jove-SLASH 1)))))

(defun jove-read-token-mult-modulo-exp (state first)
  "Read a token starting with a ?* or ?%, FIRST indicates which."
  (let ((second (jove-peek-char)))
    (if (eq ?* first)
        ;; * *= ** **=
        (if (eq ?* second)
            (if (eq ?= (jove-peek-char 2))
                (jove-finish-op state jove-ASSIGN 3)  ; **=
              (jove-finish-op state jove-STARSTAR 2)) ; **
          (if (eq ?= second)
              (jove-finish-op state jove-ASSIGN 2) ; *=
            (jove-finish-op state jove-STAR 1)))   ; *
      ;; % %=
      (if (eq ?= second)
          (jove-finish-op state jove-ASSIGN 2)   ; %=
        (jove-finish-op state jove-MODULO 1))))) ; %

(defun jove-read-token-pipe-amp (state first)
  "Read a token starting with a ?| or ?&, FIRST indicates which."
  (let ((second (jove-peek-char)))
    (cond ((eq first second)            ; && ||
           (jove-finish-op state (if (eq ?& first) jove-LOGICAL-AND jove-LOGICAL-OR) 2))
          ((eq ?= second)               ; &= |=
           (jove-finish-op state jove-ASSIGN 2))
          (t                            ; & |
           (jove-finish-op state (if (eq ?& first) jove-BITWISE-AND jove-BITWISE-OR) 1)))))

(defun jove-read-token-caret (state)
  "Read a token starting with a ?^."
  (if (eq ?= (jove-peek-char))
      (jove-finish-op state jove-ASSIGN 2)
    (jove-finish-op state jove-BITWISE-XOR 1)))

(defun jove-read-token-plus-min (state first)
  "Read a token starting with a ?+ or ?-, FIRST indicates which."
  (let ((second (jove-peek-char)))
    (cond ((eq first second)
           (jove-finish-op state jove-INC-DEC 2))
          ((eq ?= second)
           (jove-finish-op state jove-ASSIGN 2))
          (t
           (jove-finish-op state jove-PLUS-MIN 1)))))

;; Not implementing XML-style comments for now <!--
(defun jove-read-token-lt-gt (state first)
  "Read a token starting with a ?< or ?>, FIRST indicates which."
  (let ((second (jove-peek-char))
        (third (jove-peek-char 2)))
    (if (eq first second)
        ;; << >> >>> <<= >>= >>>=
        (if (eq ?> third)
            (if (eq ?= (jove-peek-char 3))
                (jove-finish-op state jove-ASSIGN 4)  ; >>>=
              (jove-finish-op state jove-BITSHIFT 3)) ; >>>
          (if (eq ?= third)
              (jove-finish-op state jove-ASSIGN 3)    ; <<= >>=
            (jove-finish-op state jove-BITSHIFT 2)))  ; << >>
      ;; < > <= >=
      (jove-finish-op state jove-RELATIONAL (if (eq ?= second) 2 1)))))

(defun jove-read-token-eq-excl (state first)
  "Read a token starting with a ?= or ?!, FIRST indicates which."
  (let ((second (jove-peek-char)))
    (cond ((eq ?> second)               ; =>
           (forward-char 2)
           (jove-finish-token state jove-ARROW))
          ((eq ?= second)               ; == != === !==
           (jove-finish-op state jove-EQUALITY (if (eq ?= (jove-peek-char 2)) 3 2)))
          (t                            ; = !
           (jove-finish-op state (if (eq ?= first) jove-EQ jove-PREFIX) 1)))))

(defsubst jove-buffer-to-number (start end radix)
  "Attempt to read a number from buffer from START to END in RADIX."
  (string-to-number (buffer-substring-no-properties start end) radix))

(defun jove-read-code-point ()
  "Read Unicode escape from buffer.
Return an integer representing the escape if valid, otherwise nil."
  (let ((code nil)
        (start (point)))
    (if (eq ?{ (char-after))
        ;; Unicode Code Point Escape \u{...}
        (progn
          (forward-char)                ; Move over '{'
          (when (jove-eat-re jove-hexadecimal-re)
            (setq code (jove-buffer-to-number (1+ start) (point) 16)))
          (cond
           ((not code)
            (jove-warn (- start 2) (point) "Invalid Unicode code point"))
           ((> code #x10ffff)
            (jove-warn (- start 2) (point) "Unicode code point out of bounds"))
           (t
            (jove-expect-char ?})
            code)))
      ;; Unicode Escape Sequence \uXXXX
      (if (jove-eat-re "[0-9A-Fa-f]\\{4\\}")
          (jove-buffer-to-number start (point) 16)
        (jove-eat-re "[0-9A-Fa-f]\\{1,3\\}")
        (jove-warn (- start 2) (point) "Invalid Unicode escape sequence")))))

(defun jove-read-escape-char (&optional in-template)
  "Read ECMAScript escape sequence from buffer.
The IN-TEMPLATE option invalidates the use of octal literals in the string."
  (forward-char)                        ; Move over '\'
  (let ((start (1- (point)))
        (char (char-after)))
    ;; If invalid escapes are found attempt to warn about them.
    (cond ((eq ?u char)
           (forward-char)
           (jove-read-code-point))
          ((eq ?x char)
           (forward-char)
           (unless (jove-eat-re "[0-9A-Fa-f]\\{2\\}")
             (jove-eat-re jove-hexadecimal-re)
             (jove-warn start (point) "Invalid hexadecimal escape")))
          ((<= ?0 char ?7)
           (jove-eat-re "[0-7]\\{1,3\\}")
           (when (or jove-strict
                     in-template)
             (jove-warn start (point) "Octal in template string")))
          (t                            ; Any other escape
           (forward-char)))))

(defun jove-read-regexp-class ()
  "Read regular expression class."
  (catch 'char-class
    (let (char)
      (while t
        ;; Advance to next critical character.
        (re-search-forward "[^\]\\\\\C-j]*" nil t) ; ?] ?\ or ?\C-j
        (setq char (char-after))
        (cond
         ((eq ?\\ char)
          (forward-char))
         ((eq ?\] char)
          (forward-char)
          (throw 'char-class t))
         (t
          ;; Hit eol or eof.
          (throw 'char-class nil)))))))

(defun jove-read-regexp-simple (state)
  "Read regular expression without extra fontifications."
  (forward-char)                        ; Move over opening delimiter.
  (let (char)
    (catch 'regexp
      (while t
        ;; Advance to next critical character.
        (re-search-forward "[^[/\\\\\C-j]*" nil t) ; ][/\ or \n
        (setq char (char-after))
        (cond
         ;; Found escape.
         ((eq ?\\ char)
          (jove-read-escape-char nil))
         ;; Found closing delimiter, exit the loop if not in a class.
         ((eq ?\/ char)
          (forward-char)
          ;; Advance over flags, valid or not
          (skip-syntax-forward "w")
          (throw 'regexp t))
         ;; Enter character class
         ((eq ?\[ char)
          (forward-char)
          (jove-read-regexp-class))
         ;; Hit eol or eof, signal error.
         (t
          (jove-warn (jove-start state) (point) "Missing regex delimiter")
          (throw 'regexp nil))))))
  (jove-finish-token state jove-REGEXP)
  (jove-set-face* state font-lock-string-face))

(defun jove-read-regexp-extras (state)
  "Read regular expression with extra fontifications."
  (forward-char)                        ; Move over opening delimiter.
  (let ((char nil)
        (looking t)
        (in-paren '())
        (in-curly '())
        (fontifies '())
        (face 'font-lock-regexp-grouping-construct))
    (catch 'regexp
      (while looking
        ;; Advance to next critical character.
        (re-search-forward "[^[(){}^$*+?.|/\\\\\C-j]*" nil t) ; ]()[/\ or \n
        (setq char (char-after))
        (cond
         ;; Found closing delimiter, exit the loop if not in a class.
         ((eq ?\/ char)
          (forward-char)
          ;; Advance over flags, valid or not.
          (skip-syntax-forward "w")
          (throw 'regexp t))
         ;; Found escape.
         ((eq ?\\ char)
          (forward-char)
          ;; Need to protect from unicode escape sequences with curlies
          ;; if fontifing regular expression grouping characters. Otherwise
          ;; just skip one character.
          (if jove-fontify-regexp-grouping-chars
              (or (jove-eat-re "u{[0-9a-fA-F]\\{1,\\}}")
                  (forward-char))
            (forward-char)))
         ;; Found character class.
         ((eq ?\[ char)
          (forward-char)
          ;; Maybe fontify the character class negation.
          (if (looking-at-p "\\^")
              (push (list  (1- (point)) (1+ (point)) face) fontifies)
            (push (list (1- (point)) (point) face) fontifies))
          (if (jove-read-regexp-class)
              (push (list  (1- (point)) (point) face) fontifies)
            ;; Hit eol or eof, finish and warn.
            (jove-warn (jove-start state) (point) "missing regex delimiter")
            (throw 'regexp 'nil)))
         ;; Found paren grouping construct.
         ((memq char '(?\( ?\) ?^ ?$ ?* ?+ ?? ?. ?|))
          (when (or (and (eq ?\( char)
                         (prog1 (push t in-paren)
                           (save-excursion
                             (forward-char)
                             (when (looking-at "\\?[:=!]")
                               (push (list (point) (+ (point) 2) face) fontifies)))))
                    (and (eq ?\) char)
                         (pop in-paren))
                    (memq char '(?^ ?$ ?* ?+ ?? ?. ?|)))
            (push (list (point) (1+ (point)) face) fontifies))
          (forward-char))
         ;; Found opening curley grouping construct.
         ((eq ?\{ char)
          (when (not in-curly)
            (setq in-curly (point)))
          (forward-char))
         ;; Found closing curley grouping construct.
         ((eq ?\} char)
          (when in-curly
            (push (list in-curly (1+ (point)) face) fontifies)
            (setq in-curly nil))
          (forward-char))
         ;; Hit eol or eof, signal error.
         (t
          (setq looking nil)
          (jove-warn (jove-start state) (point) "missing regex delimiter")))))
    (jove-finish-token state jove-REGEXP)
    (jove-set-face* state font-lock-string-face)
    (dolist (f fontifies)
      (apply #'jove-set-face f))))

(defun jove-read-regexp (state)
  "Read a regular expression."
  (if jove-fontify-regexp-grouping-chars
      (jove-read-regexp-extras state)
    (jove-read-regexp-simple state)))

(defun jove-read-string (state punc)
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
        (goto-char (+ (point) 2)))
       ;; Found closing delimiter, exit the loop.
       ((eq punc char)
        (forward-char)
        (setq looking nil))
       ;; Hit eol or eof, signal error.
       (t
        (setq looking nil)
        (jove-warn (jove-start state) (point) "Missing string closing delimiter")))))
  (jove-finish-token state jove-STRING)
  (jove-set-face* state font-lock-string-face))

(defun jove-string-builder (list)
  "Return a string built from a LIST of strings."
  (eval `(concat ,@list)))

(defun jove-read-tmpl-token (state)
  "Read template string tokens."
  (let (char)
    (catch 'token
      (while t
        (re-search-forward "[^`$\\\\]*" nil t)
        (setq char (char-after))
        (cond
         ((eq ?\` char)
          (if (and (= (jove-start state) (point))
                   (eq jove-TEMPLATE (jove-tt state)))
              (progn
                (forward-char)
                (jove-finish-token state jove-BACKQUOTE)
                (jove-set-face* state font-lock-string-face)
                (throw 'token nil))
            (jove-finish-token state jove-TEMPLATE)
            (jove-set-face* state font-lock-string-face)
            (throw 'token nil)))
         ((eq ?\$ char)
          (if (eq ?\{ (jove-peek-char))
              (if (and (= (jove-start state) (jove-end state))
                       (eq jove-TEMPLATE (jove-tt state)))
                  (progn
                    (forward-char 2)
                    (jove-finish-token state jove-DOLLAR-BRACE-L)
                    (throw 'token nil))
                (jove-finish-token state jove-TEMPLATE)
                (jove-set-face* state font-lock-string-face)
                (throw 'token nil))
            ;; Its possible to catch a single '$' which is part of the
            ;; literal template string. So it is necessary to always
            ;; advance at least one character.
            (forward-char)))
         ((eq ?\\ char)
          (jove-read-escape-char t))
         (t                             ; Hit eof.
          ;; Don't run the template hook because this isn't a real template literal.
          (jove-warn (jove-start state) (point-max) "Missing template string closing delimiter")
          (jove-finish-token state jove-TEMPLATE)
          (throw 'token nil)))))))

(defun jove-read-word-escape (state)
  "Read an escape in an identifier."
  ;; Already know there was a '\'
  (forward-char 2)
  (if (eq ?u (char-before))
      (progn
        (jove-set-contains-esc state t)
        ;; The function below will warn of any invalid code points.
        (jove-read-code-point))
    (jove-warn (- (point) 2) (point) "Invalid escape in identifier")))

(defun jove-read-word-internal (state)
  "Read ECMAScript Identifier."
  (jove-set-contains-esc state nil)
  (let (chunk
        invalid
        (looking t)
        (start (point)))
    (when (eq ?\\ (char-after))
      (setq chunk (jove-read-word-escape state))
      (setq invalid (or invalid
                        (not chunk)
                        (not (eq ?w (char-syntax chunk)))))
      (if invalid
          (jove-warn start (point) "Invalid identifier start character")
        (jove-add-to-string (string chunk)))) ; Here chunk is a character.
    (setq start (point))
    (while looking
      (cond
       ((eq ?\\ (char-after))
        (jove-set-contains-esc state t)
        (setq chunk (jove-read-word-escape state))
        (if (not chunk)
            (setq invalid t)
          ;; `char-syntax' protected by above if condition.
          (when (not (memq (char-syntax chunk) '(?w ?_)))
            (setq invalid t)
            (jove-warn start (point) "Invalid identifier character")))
        (unless invalid
          (jove-add-to-string (string chunk))))
       ((< 0 (skip-syntax-forward "w_"))
        (unless invalid
          (jove-add-to-string (buffer-substring-no-properties start (point)))))
       (t
        (setq looking nil)))
      (setq start (point)))
    ;; If unable to properly parse an escape return nil, the word is
    ;; still moved over. A warning is added and the word cannot be
    ;; considered a keyword.
    (unless invalid
      (jove-collect-string))))

(defun jove-read-word (state)
  "Read from buffer an ECMAScript Identifier Name or Keyword."
  (let ((word (jove-read-word-internal state))
        (type jove-NAME))
    (when (and word                     ; If word is nil just highlight it.
               (or (>= jove-ecma-version 6)
                   (not (jove-contains-esc state))))
      (setq type (or (gethash (intern word) jove-keywords)
                     jove-NAME)))
    (jove-finish-token state type word)
    (when (jove-tt-keyword type)
      (jove-set-face* state font-lock-keyword-face))))

(defun jove-read-token (state char)
  "Read token using STATE with supplied CHAR."
  ;; Implements getTokenFromCode from acorn.
  (cond
   ((eq ?\. char) (jove-read-token-dot state))
   ((eq ?\( char) (jove-finish-punc state jove-PAREN-L))
   ((eq ?\) char) (jove-finish-punc state jove-PAREN-R))
   ((eq ?\; char) (jove-finish-punc state jove-SEMI))
   ((eq ?\, char) (jove-finish-punc state jove-COMMA))
   ((eq ?\[ char) (jove-finish-punc state jove-BRACKET-L))
   ((eq ?\] char) (jove-finish-punc state jove-BRACKET-R))
   ((eq ?{ char) (jove-finish-punc state jove-BRACE-L))
   ((eq ?} char) (jove-finish-punc state jove-BRACE-R))
   ((eq ?? char) (jove-finish-punc state jove-QUESTION))
   ((eq ?: char) (jove-finish-punc state jove-COLON))
   ((and (eq ?\` char) (<= 6 jove-ecma-version))
    (jove-finish-punc state jove-BACKQUOTE)
    (jove-set-face* state font-lock-string-face))
   ((eq ?0 char)
    (let ((next (jove-peek-char 1)))
      (cond
       ((memq next '(?x ?X))
        (forward-char 2) (jove-eat-re jove-hexadecimal-re))
       ((memq next '(?o ?O))
        (forward-char 2) (jove-eat-re jove-octal-re))
       ((memq next '(?b ?B))
        (forward-char 2) (jove-eat-re jove-binary-re))
       (t
        (jove-eat-re jove-number-re)))
      (jove-finish-token state jove-NUM)))
   ((<= ?1 char ?9)
    (jove-eat-re jove-number-re)
    (jove-finish-token state jove-NUM))
   ((or (eq ?\' char) (eq ?\" char)) (jove-read-string state char))
   ((eq ?\/ char) (jove-read-token-slash state))
   ((memq char '(?* ?%)) (jove-read-token-mult-modulo-exp state char))
   ((memq char '(?\| ?\&)) (jove-read-token-pipe-amp state char))
   ((eq ?^ char) (jove-read-token-caret state))
   ((memq char '(?+ ?-)) (jove-read-token-plus-min state char))
   ((memq char '(?< ?>)) (jove-read-token-lt-gt state char))
   ((memq char '(?= ?!)) (jove-read-token-eq-excl state char))
   ((eq ?~ char) (jove-finish-op state jove-PREFIX 1))
   (t
    ;; Skip and try again.
    (jove-warn (point) (1+ (point)) "unexpected character")
    (forward-char)
    (jove-next-token state))))

(defun jove-next-token (state)
  "Use previous STATE to read next token and return new state."
  (let ((char nil)
        (ctx (jove-current-ctx state)))
    (when (or (not ctx)
              (not (jove-ctx-preserve-space ctx)))
      (jove-set-newline-before state nil)
      (jove-skip-space state))
    ;; Initialize current token state.
    (jove-set-start state (point))
    (jove-set-value state nil)
    (setq char (char-after))
    (cond
     ((eobp)
      (jove-finish-token state jove-EOF))
     ((jove-ctx-override ctx)
      (funcall (jove-ctx-override ctx) state))
     ((or (memq (char-syntax char) '(?w ?\\)))
      (jove-read-word state))
     (t
      (jove-read-token state char)))
    state))                             ; Return a token vector.

(provide 'jove-lexer)

;;; jove-lexer.el ends here
