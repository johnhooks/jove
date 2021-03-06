;;;; jove-lexer.el --- The Jove Mode Lexer -*- lexical-binding: t; -*-

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

;; TODO: Remove all fontification.

;;; Code:

(require 'jove-vars)

;;; Global Variables

(defvar jove-keywords (make-hash-table :test 'equal)
  "Hash table to map keyword names to token types.")

(defvar jove-after-comment-hook nil
  "Hook called after the lexer has encountered a comment.")

;; Buffer Local Global Scratch Variables

(defvar-local jove--string-buffer nil
  "List of string chunks built up while scanning various tokens.
A scratch variable.")

(defvar-local jove--contains-esc nil
  "Boolean flag to indicate to `jove-read-word' that it contains an escape.
A scratch variable.")

(defvar-local jove--cache '()
  "List of previously lexed tokens.")

;;; Regular Expressions

(defconst jove-binary-re "[0-1]+"
  "A regular expression to match a binary number.")

(defconst jove-octal-re "[0-7]+"
  "A regular expression to match an octal number.")

(defconst jove-decimal-re "[0-9]+"
  "A regular expression to match a decimal number.")

(defconst jove-hexadecimal-re "[0-9A-Fa-f]+"
  "A regular expression to match a hexadecimal number.")

(defconst jove-number-re "[0-9]+\\(?:\\.[0-9]*\\)?\\(?:[eE][-+]?[0-9]+\\)?"
  "A regular expression to match a integer or float number.")

(defconst jove-escape-re (concat "\\([fnrtv]\\|x[0-9a-fA-F]\\{2\\}\\|c[A-Z]\\|"
                             "u\\([0-9a-fA-F]\\{4\\}\\|{[0-9a-fA-F]\\{1,\\}}\\)\\)")
  "A regular expression to match escape sequences.
Does not include the backslash character.")

(defconst jove-regexp-escape-re "[bBdDsSwW1-9]"
  "A regular expression to match JavaScript regex escape sequences.
Does not include the backslash character.")

(defvar jove-string-single-quote-re
  (eval-when-compile
    (rx
     ;; Opening delimiter.
     "'"
     (0+ (or
          ;; Match any characters outside an escape
          (not (any ?\\ ?'))
          ;; Match escaped characters.
          (and "\\" not-newline)))
     ;; Closing delimiter.
     "'"))
  "A regular expression for reading single quote strings.")

(defvar jove-string-double-quote-re
  (eval-when-compile
    (rx
     ;; Opening delimiter.
     "\""
     (0+ (or
          ;; Match any characters outside an escape
          (not (any ?\\ ?\"))
          ;; Match escaped characters.
          (and "\\" not-newline)))
     ;; Closing delimiter.
     "\""))
  "A regular expression for reading double quote strings.")

(defvar jove-regular-expression-re
  (eval-when-compile
    (rx
     ;; Start of regex.
     "/"
     (0+ (or
          ;; Match characters outside of a character class.
          (not (any ?\[ ?/ ?\\))
          ;; Match backslash quoted characters.
          (and "\\" not-newline)
          ;; Match character class.
          ;; So first condition to or makes this where it matchs!
          (and
           "["
           (0+ (or
                (not (any ?\] ?\\))
                (and "\\" not-newline)))
           "]")))
     (group "/")
     (0+ (any ?g ?i ?m ?u ?y))))
  "Regular expression matching a JavaScript Regex literal.
First group match is the closing delimiter.")

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

(cl-defun jove-make-tt (label &key before-expr starts-expr prefix postfix binop
                          is-keyword is-assign is-word is-atom)
  "Return a vector representing a token type.
LABEL should be a string.  The remaining arguments are key value
pairs representing different token type characteristic to set."
  (vector label                         ; 0
          before-expr                   ; 1
          starts-expr                   ; 2
          prefix                        ; 3
          postfix                       ; 4
          binop                         ; 5
          is-keyword                    ; 6
          is-assign                     ; 7
          is-word                       ; 8
          is-atom))                     ; 9

(defsubst jove-tt-label (tt)
  "Return the 'label' slot of the token type TT."
  (aref tt 0))
(defsubst jove-tt-before-expr (tt)
  "Return the 'before-expr' slot of the token type TT."
  (aref tt 1))
(defsubst jove-tt-starts-expr (tt)
  "Return the 'starts-expr' slot of the token type TT."
  (aref tt 2))
(defsubst jove-tt-prefix (tt)
  "Return the 'prefix' slot of the token type TT."
  (aref tt 3))
(defsubst jove-tt-postfix (tt)
  "Return the 'postfix' slot of the token type TT."
  (aref tt 4))
(defsubst jove-tt-binop (tt)
  "Return the 'binop' slot of the token type TT."
  (aref tt 5))
(defsubst jove-tt-is-keyword (tt)
  "Return the 'is-keyword' slot of the token type TT."
  (aref tt 6))
(defsubst jove-tt-is-assign (tt)
  "Return the 'is-assign' slot of the token type TT."
  (aref tt 7))
(defsubst jove-tt-is-word (tt)
  "Return the 'is-word' slot of the token type TT."
  (aref tt 8))
(defsubst jove-tt-is-atom (tt)
  "Return the 'is-atom' slot of the token type TT."
  (aref tt 9))

(defun jove-make-binop (label prec)
  "Return a vector representing a binary operator token type.
LABEL should be a string and PREC an integer of operator precedence."
  (jove-make-tt label :before-expr t :binop prec))

(defun jove-make-keyword (label &rest options)
  "Return a vector representing a keyword token type.
LABEL should be a string.  The remaining arguments are key value
pairs collected in OPTIONS."
  (let ((tt (apply #'jove-make-tt
                   (append `(,label :is-keyword t)
                           options))))
    (puthash label tt jove-keywords)))

(defun jove-make-atom (label &rest options)
  "Return a vector representing an atom token type.
LABEL should be a string.  The remaining arguments are key value
pairs collected in OPTIONS."
  (let ((tt (apply #'jove-make-tt
                   (append `(,label :is-atom t :is-keyword t :starts-expr t)
                           options))))
    (puthash label tt jove-keywords)))

(defun jove-make-contextual (label &rest options)
  "Return a vector representing a keyword token type.
LABEL should be a string.  The remaining arguments are key value
pairs collected in OPTIONS."
  (let ((tt (apply #'jove-make-tt
                   (append `(,label :is-word t)
                           options))))
    (puthash label tt jove-keywords)))

(defconst jove-NUM (jove-make-tt "num" :starts-expr t :is-atom t))
(defconst jove-REGEX (jove-make-tt "regex" :starts-expr t :is-atom t))
(defconst jove-STRING (jove-make-tt "string" :starts-expr t :is-atom t))
(defconst jove-NAME (jove-make-tt "name" :starts-expr t :is-word t))
(defconst jove-BOB (jove-make-tt "bob"))        ; Beginning of the buffer.
(defconst jove-EOB (jove-make-tt "eob"))        ; End of the buffer.

;;; Punctuation Token Types

(defconst jove-BRACE-L (jove-make-tt "{" :before-expr t :starts-expr t))
(defconst jove-BRACE-R (jove-make-tt "}"))
(defconst jove-PAREN-L (jove-make-tt "(" :before-expr t :starts-expr t))
(defconst jove-PAREN-R (jove-make-tt ")"))
(defconst jove-BRACKET-L (jove-make-tt "[" :before-expr t :starts-expr t))
(defconst jove-BRACKET-R (jove-make-tt "]"))
(defconst jove-COMMA (jove-make-tt "," :before-expr t))
(defconst jove-SEMI (jove-make-tt ";" :before-expr t))
(defconst jove-COLON (jove-make-tt ":" :before-expr t))
(defconst jove-DOT (jove-make-tt "."))
(defconst jove-QUESTION (jove-make-tt "?" :before-expr t))
(defconst jove-ARROW (jove-make-tt "=>" :before-expr t))
(defconst jove-TEMPLATE (jove-make-tt "template"))
(defconst jove-ELLIPSIS (jove-make-tt "..." :before-expr t))
(defconst jove-DOLLAR-BRACE-L (jove-make-tt "${" :before-expr t :starts-expr t))
(defconst jove-BACKQUOTE (jove-make-tt "`" :before-expr t))
(defconst jove-KLAMMERRAFFE (jove-make-tt "@" :before-expr t :prefix)) ; Clinging Monkey

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

(defconst jove-EQ (jove-make-tt "=" :before-expr t :is-assign t))
(defconst jove-ASSIGN (jove-make-tt "_=" :before-expr t :is-assign t))
(defconst jove-INC-DEC (jove-make-tt "++/--" :starts-expr t :prefix t :postfix t))
(defconst jove-PREFIX (jove-make-tt "prefix" :before-expr t :starts-expr t :prefix t))
(defconst jove-LOGICAL-OR (jove-make-binop "||" 1))
(defconst jove-LOGICAL-AND (jove-make-binop "&&" 2))
(defconst jove-BITWISE-OR (jove-make-binop "|" 3))
(defconst jove-BITWISE-XOR (jove-make-binop "^" 4))
(defconst jove-BITWISE-AND (jove-make-binop "&" 5))
(defconst jove-EQUALITY (jove-make-binop "==/!=" 6)) ; == != === !==
(defconst jove-RELATIONAL (jove-make-binop "</>" 7)) ; < > <= >=
(defconst jove-BITSHIFT (jove-make-binop "<</>>" 8)) ; << >> >>>
(defconst jove-PLUS-MIN (jove-make-tt "+/-" :binop 9 :before-expr t :starts-expr t :prefix t))
(defconst jove-MODULO (jove-make-binop "%" 10))
(defconst jove-STAR (jove-make-binop "*" 10))
(defconst jove-SLASH (jove-make-binop "/" 10))
(defconst jove-STARSTAR (jove-make-tt "**" :before-expr t))

;;; Keywords Token Types

(defconst jove-BREAK (jove-make-keyword "break"))
(defconst jove-CASE (jove-make-keyword "case" :before-expr t))
(defconst jove-CATCH (jove-make-keyword "catch"))
(defconst jove-CONTINUE (jove-make-keyword "continue"))
(defconst jove-DEBUGGER (jove-make-keyword "debugger"))
(defconst jove-DEFAULT (jove-make-keyword "default"))
(defconst jove-DO (jove-make-keyword "do" :before-expr t))
(defconst jove-ELSE (jove-make-keyword "else" :before-expr t))
(defconst jove-FINALLY (jove-make-keyword "finally"))
(defconst jove-FOR (jove-make-keyword "for"))
(defconst jove-FUNCTION (jove-make-keyword "function" :starts-expr t))
(defconst jove-IF (jove-make-keyword "if"))
(defconst jove-RETURN (jove-make-keyword "return" :before-expr t))
(defconst jove-SWITCH (jove-make-keyword "switch"))
(defconst jove-THROW (jove-make-keyword "throw" :before-expr t))
(defconst jove-TRY (jove-make-keyword "try"))
(defconst jove-VAR (jove-make-keyword "var"))
(defconst jove-CONST (jove-make-keyword "const"))
(defconst jove-WHILE (jove-make-keyword "while"))
(defconst jove-WITH (jove-make-keyword "with"))
(defconst jove-NEW (jove-make-keyword "new" :before-expr t :starts-expr t))
(defconst jove-THIS (jove-make-keyword "this" :starts-expr t))
(defconst jove-SUPER (jove-make-keyword "super" :starts-expr t))
(defconst jove-CLASS (jove-make-keyword "class"))
(defconst jove-EXTENDS (jove-make-keyword "extends" :before-expr t))
(defconst jove-EXPORT (jove-make-keyword "export"))
(defconst jove-IMPORT (jove-make-keyword "import"))
(defconst jove-IN (jove-make-keyword "in" :binop 7 :before-expr t))
(defconst jove-INSTANCEOF (jove-make-keyword "instanceof" :binop 7 :before-expr t))
(defconst jove-TYPEOF (jove-make-keyword "typeof" :prefix t :before-expr t :starts-expr t))
(defconst jove-VOID (jove-make-keyword "void" :prefix t :before-expr t :starts-expr t))
(defconst jove-DELETE (jove-make-keyword "delete" :prefix t :before-expr t :starts-expr t))

;;; Atom Token Types

(defvar jove-NULL (jove-make-atom "null"))
(defvar jove-UNDEFINED (jove-make-atom "undefined"))
(defvar jove-TRUE (jove-make-atom "true"))
(defvar jove-FALSE (jove-make-atom "false"))
(defvar jove-NAN (jove-make-atom "NaN"))
(defvar jove-INFINITY (jove-make-atom "Infinity"))

;;; Contextual Keyword Token Types

(defvar jove-AS (jove-make-contextual "as"))
(defvar jove-OF (jove-make-contextual "of"))
(defvar jove-GET (jove-make-contextual "get"))
(defvar jove-SET (jove-make-contextual "set"))
(defvar jove-LET (jove-make-contextual "let"))
(defvar jove-FROM (jove-make-contextual "from"))
(defvar jove-ASYNC (jove-make-contextual "async"))
(defvar jove-YIELD (jove-make-contextual "yield"))
(defvar jove-AWAIT (jove-make-contextual "await"))
(defvar jove-STATIC (jove-make-contextual "static"))

;; JSX Token Types

(defvar jove-JSX-NAME (jove-make-tt "jsxName"))
(defvar jove-JSX-TEXT (jove-make-tt "jsxText" :before-expr t))
(defvar jove-JSX-TAG-START (jove-make-tt "jsxTagStart"))
(defvar jove-JSX-TAG-END (jove-make-tt "jsxTagEnd"))

;;; Context Types

(cl-defun jove-make-ctx (token &key is-expr preserve-space override is-generator)
  "Return a vector representing a context type."
  (vector token                         ; 0
          is-expr                       ; 1
          is-generator                  ; 2
          preserve-space                ; 3
          override))                    ; 4

(defsubst jove-ctx-token (ctx)
  "Return the 'token' slot of the context type CTX."
  (aref ctx 0))
(defsubst jove-ctx-is-expr (ctx)
  "Return the 'is-expr' slot of the context type CTX."
  (aref ctx 1))
(defsubst jove-ctx-is-generator (ctx)
  "Return the 'is-generator' slot of the context type CTX."
  (aref ctx 2))
(defsubst jove-ctx-preserve-space (ctx)
  "Return the 'preserve-space' slot of the context type CTX."
  (aref ctx 3))
(defsubst jove-ctx-override (ctx)
  "Return the 'override' slot of the context type CTX."
  (aref ctx 4))

(defvar jove-B-STAT (jove-make-ctx "{"))
(defvar jove-B-EXPR (jove-make-ctx "{" :is-expr t))
(defvar jove-B-TMPL (jove-make-ctx "${"))
(defvar jove-P-STAT (jove-make-ctx "("))
(defvar jove-P-EXPR (jove-make-ctx "(" :is-expr t))
(defvar jove-Q-TMPL (jove-make-ctx "`" :is-expr t :preserve-space t :override #'jove-read-tmpl-token))
(defvar jove-F-STAT (jove-make-ctx "function"))
(defvar jove-F-EXPR (jove-make-ctx "function" :is-expr t))
(defvar jove-F-STAT-GEN (jove-make-ctx "function" :is-generator t))
(defvar jove-F-EXPR-GEN (jove-make-ctx "function" :is-expr t :is-generator t))

;; JSX Context Types

(defvar jove-J-OTAG (jove-make-ctx "<tag>" :override #'jove-jsx-read-tag-token))
(defvar jove-J-CTAG (jove-make-ctx "</tag>" :override #'jove-jsx-read-tag-token))
(defvar jove-J-EXPR (jove-make-ctx "<tag>...</tag>" :is-expr t :preserve-space t
                           :override #'jove-jsx-read-expr-token))

;;; Token

;; Use `point' to track the current position of the lexer in the buffer.
;; The logic behind using `point', is the relative ease of use to apply
;; elisp regexp functions when reading tokens in the buffer.

;; Unlike Acorn, this parser is working inside an Emacs buffer, in
;; which positions are between characters and start at 1

;; State properties of current token:

(defvar-local jove--start 1
  "Current token start position.")

(defvar-local jove--end 1
  "Current token end position.")

(defvar-local jove--tt jove-BOB
  "Current token type.")

(defvar-local jove--value nil
  "Current token value.
Used by `jove-read-word' to store the parsed string value.")

(defvar-local jove--linum nil
  "Current line number.")

(defvar-local jove--face nil
  "Current token's suggested face.")

(defvar-local jove--expr-allowed t
  "Boolean to indicate whether an expression is allowed.")

(defvar-local jove--newline-before t
  "Boolean to indicate a newline between the previous and current token.")

(defvar-local jove--ctx-stack '()
  "Lexer state context stack.
Used to superficially track syntactic context in order to interpret a
slash character as either an operator or regular expression delimiter.")

;; Thinking about removing this and making previous token info
;; avaiable through the token cache.  If the list is in reverse order
;; I would need to look back through all the tokens.  once I find one
;; that is before an edit then switch the search for one that is
;; before a newline.

(defvar-local jove--prev-start 1
  "Previous token start position.")

(defvar-local jove--prev-end 1
  "Previous token end position.")

(defvar-local jove--prev-tt jove-BOB
  "Previoustoken type.")

(defvar-local jove--prev-value nil
  "Previous token value.
Additional information beyond type.")

(defvar-local jove--prev-linum nil
  "Previous line number.")

(defun jove-make-token ()
  "Make a token from the lexer global variables."
  (list jove--start                         ; 0
        jove--end                           ; 1
        jove--tt                            ; 2
        jove--value                         ; 3
        jove--newline-before))              ; 4

(defun jove-copy-lexer-state ()
  "Return a list representing a copy of the lexer state."
  (list jove--start                         ; 0
        jove--end                           ; 1
        jove--tt                            ; 2
        jove--value                         ; 3
        jove--linum                         ; 5
        jove--expr-allowed                  ; 6
        jove--newline-before                ; 7
        jove--ctx-stack))                   ; 8

;;; Utility Functions

(defun jove-warn (start end message)
  "Queue a warning from START to END with MESSAGE.
Push the warning into the list `jove--warnings'."
  (push (list start end message) jove--warnings)
  nil)                                  ; Return nil

(defun jove-unexpected-char ()
  "Signal unexpected character error at current position."
  (jove-warn (point) (1+ (point)) "unexpected char")
  (forward-char))

(defun jove-token-raw ()
  "Return the raw value of the current token from the buffer."
  (buffer-substring-no-properties jove--start jove--end))

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
  "Add CHUNK to `jove--string-buffer'."
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

(defsubst jove-inc-linum ()
  (setq jove--linum (1+ jove--linum)))

(defun jove-skip-line-comment ()
  "Skip line comment.
Run comment hook after the end is found."
  (if (setq jove--end (search-forward "\n" nil t))
      (jove-inc-linum)
    (setq jove--end (goto-char (point-max))))       ; Reached the end of the buffer.
  (run-hooks 'jove-after-comment-hook))

(defun jove-skip-block-comment ()
  "Skip block comment.
Run comment hook after the end is found."
  (let ((looping t))
    (condition-case nil
        (while looping
          (setq jove--end (re-search-forward "\*/\\|\C-j" nil))
          (if (eq ?\C-j (char-before))
              (jove-inc-linum)
            (if (and (eq ?\/ (char-before))
                     (eq ?* (char-before (1- (point)))))
                (setq looping nil))))
      (search-failed
       (jove-warn jove--start (point-max) "Missing comment closing delimiter")))
    (run-hooks 'jove-after-comment-hook)))

(defun jove-skip-space ()
  "Skip whitespace and comments.
When a newline is encountered, `jove--newline-before' is set to t and
`jove--linum' is incremented."
  (let ((first nil)
        (second nil)
        (prev-linum jove--linum)
        (jove--start jove--start)               ; Lexically scoped copy.
        (jove--end jove--end))                  ; Lexically scoped copy.
    (catch 'whitespace
      (while t
        (skip-syntax-forward " ")
        (setq first (char-after))
        (cond
         ((eq ?\/ first)
          (if (eq ?\/ (setq jove--start (point)
                            second (jove-peek-char)))
              (jove-skip-line-comment)
            (if (eq ?\* second)
                (jove-skip-block-comment)
              (throw 'whitespace nil))))
         ((eq ?\C-j first)
          (forward-char)
          (jove-inc-linum))
         (t                             ; Hit eob.
          (throw 'whitespace nil)))))
    (setq jove--newline-before (< prev-linum jove--linum))))

;;; Context Functions

(defsubst jove-current-ctx ()
  "Return the the top of the lexer context stack."
  (car jove--ctx-stack))

(defun jove-initial-ctx ()
  "Return the initial context for use for `jove--ctx-stack'."
  (list jove-B-STAT))

;; TODO: Figure out how to handle contextual 'yield' and 'of'
(defun jove-brace-is-block-p (prev-tt)
  "Return non-nil of if brace is a block.
The `jove--current-ctx' and PREV-TT are used to determine
the context of a left brace character."
  (cond
   ((eq jove-COLON prev-tt)
    ;; If previous token type is a colon, determine it is used in a
    ;; property key in an object literal.
    (let ((parent (jove-current-ctx)))
      (if (memq parent (list jove-B-STAT jove-B-EXPR))
          (not (jove-ctx-is-expr parent)))))
   ;; The check for `jove-NAME' and `jove--expr-allowed' detects whether we are
   ;; after a `yield` or `of` construct. See the `jove-ctx-update' for
   ;; `jove-NAME'.
   ((or (eq jove-RETURN prev-tt)
        (and (memq prev-tt (list jove-YIELD jove-OF))
             jove--expr-allowed))
    jove--newline-before)
   ((memq prev-tt (list jove-ELSE jove-SEMI jove-BOB jove-PAREN-R jove-ARROW))
    t)
   ((eq jove-BRACE-L prev-tt)
    (eq jove-B-STAT (jove-current-ctx)))
   ((memq prev-tt (list jove-VAR jove-LET jove-CONST))
    nil)
   (t
    (not jove--expr-allowed))))

(defun jove-update-ctx (tt prev-tt)
  "Update the syntactic context stack.
The `jove--ctx-stack' and `jove--expr-allowed' variables are
modified to reflect the change of `jove--prev-tt'."
  ;; TODO: Signal an error if an incorrect context type is encountered when
  ;; popping the context stack.
  (cond
   ((and (jove-tt-is-keyword tt)
         (eq jove-DOT prev-tt))
    ;; NOTE: What is this condition trying to catch? I think it is to
    ;; catch instances where a keyword has mistakenly been used as a
    ;; member of an object. This prevents the token type from
    ;; mistakenly setting `jove--expr-allowed' to t.
    (setq jove--expr-allowed nil))

   ;; '{'  Enter brace statement, expression context.
   ((eq jove-BRACE-L tt)
    (let ((top (jove-current-ctx)))
      (push (cond ((eq jove-J-OTAG top) jove-B-EXPR)
                  ((eq jove-J-EXPR top) jove-B-TMPL)
                  ((jove-brace-is-block-p prev-tt) jove-B-STAT)
                  (t jove-B-EXPR))
            jove--ctx-stack))
    (setq jove--expr-allowed t))

   ;; '}' Or ')'  Exit either brace or paren context.
   ((memq tt (list jove-BRACE-R jove-PAREN-R))
    (if (= 1 (length jove--ctx-stack))
        (setq jove--expr-allowed t)
      (let ((out (pop jove--ctx-stack)))
        (when (and (eq jove-B-STAT out)
                   ;; FIXME: Use a symbol rather than a string
                   (string= "function" (jove-ctx-token (jove-current-ctx))))
          (setq out (pop jove--ctx-stack)))
        (setq jove--expr-allowed (not (jove-ctx-is-expr out))))))

   ;; '('  Enter parenthesis context.
   ((eq jove-PAREN-L tt)
    (push (if (memq prev-tt (list jove-IF jove-FOR jove-WITH jove-WHILE))
              jove-P-STAT
            jove-P-EXPR)
          jove--ctx-stack)
    (setq jove--expr-allowed t))

   ;; '${' Enter brace template context.
   ((eq jove-DOLLAR-BRACE-L tt)
    (push jove-B-TMPL jove--ctx-stack)
    (setq jove--expr-allowed t))

   ;; '`'  Enter or exit a template literal context.
   ((eq jove-BACKQUOTE tt)
    (if (eq jove-Q-TMPL (jove-current-ctx))
        (pop jove--ctx-stack)
      (push jove-Q-TMPL jove--ctx-stack))
    (setq jove--expr-allowed nil))

   ;; '--' or '++'  Do not alter `jove-expr-allowed'.
   ((eq jove-INC-DEC tt))

   ;; 'function'  Enter function expression context.
   ((eq jove-FUNCTION tt)
    (push (if (and (jove-tt-before-expr prev-tt)
                   (not (memq prev-tt (list jove-SEMI jove-ELSE)))
                   (not (and (memq prev-tt (list jove-COLON jove-BRACE-L))
                             (eq jove-B-STAT (jove-current-ctx)))))
              jove-F-EXPR
            jove-F-STAT)
          jove--ctx-stack)
    (setq jove--expr-allowed nil))

   ;; '**'
   ((eq jove-STAR tt)
    (when (eq jove-FUNCTION prev-tt)
      (push (if (eq jove-F-EXPR (pop jove--ctx-stack))
                jove-F-EXPR-GEN
              jove-F-STAT-GEN)
            jove--ctx-stack))
    (setq jove--expr-allowed t))

   ;; 'of' or 'yeild'
   ((or (and (eq jove-OF tt) (not jove--expr-allowed))
        (and (eq jove-YIELD tt) (jove-in-generator-ctx)))
    (setq jove--expr-allowed t))

   ;; '<tag>'
   ((eq jove-JSX-TAG-START tt)
    (push jove-J-EXPR jove--ctx-stack)          ; Treat as beginning of JSX expression.
    (push jove-J-OTAG jove--ctx-stack)          ; Start opening tag context
    (setq jove--expr-allowed nil))

   ;; '</tag>' or '<tag />'
   ((eq jove-JSX-TAG-END tt)
    (let ((out (pop jove--ctx-stack)))
      (if (or (and (eq jove-J-OTAG out)
                   (eq jove-SLASH prev-tt))
              (eq jove-J-CTAG out))
          (progn
            (pop jove--ctx-stack)
            (setq jove--expr-allowed (eq jove-J-EXPR (jove-current-ctx))))
        (setq jove--expr-allowed t))))

   ;; Quoted from acorn-jsx/inject.js
   ;;   Do not consider JSX expr -> JSX open tag -> ... anymore

   ;; NOTE: I do not get this. I will remove the `jove-J-EXPR' and
   ;; when the context updates again on the `jove-J-CTAG' it will
   ;; double pop the context stack, resulting in over removal.

   ;; TODO: Consider removal. I don't think this condition is
   ;; helpful for editing. For now consider '</>' a self closing
   ;; tag.

   ;; Now I get it! Its to reconsider the '<' as a closing tag
   ;; rather than a opening tag. If it is followed by a '/'.

   ;; Though if it runs across a '</>' it will double pop the
   ;; context on accident.

   ;; '/>'
   ((and (eq jove-SLASH tt)
         (eq jove-JSX-TAG-START prev-tt))
    (setq jove--ctx-stack (cons jove-J-CTAG
                            (cdr (cdr jove--ctx-stack)))
          jove--expr-allowed nil))

   ;; Otherwise set `jove-expr-allowed' to token type slot 'before-expr'.
   (t
    (setq jove--expr-allowed (jove-tt-before-expr tt)))))

(defun jove-in-generator-ctx ()
  (let ((head (car jove--ctx-stack))
        (tail (cdr jove--ctx-stack)))
    (while (and head
                (not (string-equal "function" (jove-ctx-token head))))
      (setq head (car tail)
            tail (cdr tail)))
    (and head
         (jove-ctx-is-generator head))))

;;; Token Reading Functions

(defun jove-finish-token (tt &optional face value)
  "Finish token of type TT and update the syntactic context.
Optionally set the token VALUE, otherwise set it to nil."
  (let ((prev-tt jove--tt))
    (setq jove--end (point)
          jove--tt tt
          jove--value value
          jove--face face)
    ;; Finishing the token before updating the ctx allows all
    ;; token data to be referenced in the stored context.
    (jove-update-ctx tt prev-tt)))

(defun jove-finish-punc (tt)
  "Finish punctuation token of type TT."
  (forward-char)
  (jove-finish-token tt))

(defun jove-finish-op (tt size)
  "Finish operator token of type TT and SIZE."
  ;; TODO: Could `forward-char' be used? All characters to be skipped
  ;; over have already been confirmed to exist.
  (forward-char size)
  ;; (goto-char (+ (point) size))
  (jove-finish-token tt))

(defun jove-read-token-dot ()
  "Read a token starting with a period."
  (cond
   ((jove-eat-re ".[0-9]+\\(?:[eE][-+]?[0-9]+\\)?")
    (jove-finish-token jove-NUM))
   ((and (eq ?. (jove-peek-char)) (eq ?. (jove-peek-char 2))) ; ...
    (forward-char 3)                    ; Why not `jove-finish-op'?
    (jove-finish-token jove-ELLIPSIS))
   (t
    (forward-char)
    (jove-finish-token jove-DOT))))

(defun jove-read-token-slash ()
  "Read a token starting with a forward slash."
  (let ((next (jove-peek-char)))
    (cond
     (jove--expr-allowed                   ; Must be a regular expression.
      (jove-read-regex))
     ((eq ?= next)
      (jove-finish-op jove-ASSIGN 2))
     (t
      (jove-finish-op jove-SLASH 1)))))

(defun jove-read-token-mult-modulo-exp (first)
  "Read a token starting with a ?* or ?%, FIRST indicates which."
  (let ((second (jove-peek-char)))
    (if (eq ?* first)
        ;; * *= ** **=
        (if (eq ?* second)
            (if (eq ?= (jove-peek-char 2))
                (jove-finish-op jove-ASSIGN 3)  ; **=
              (jove-finish-op jove-STARSTAR 2)) ; **
          (if (eq ?= second)
              (jove-finish-op jove-ASSIGN 2)    ; *=
            (jove-finish-op jove-STAR 1)))      ; *
      ;; % %=
      (if (eq ?= second)
          (jove-finish-op jove-ASSIGN 2)        ; %=
        (jove-finish-op jove-MODULO 1)))))      ; %

(defun jove-read-token-pipe-amp (first)
  "Read a token starting with a ?| or ?&, FIRST indicates which."
  (let ((second (jove-peek-char)))
    (cond
     ((eq first second)                 ; && ||
      (jove-finish-op (if (eq ?& first) jove-LOGICAL-AND jove-LOGICAL-OR) 2))
     ((eq ?= second)                    ; &= |=
      (jove-finish-op jove-ASSIGN 2))
     (t                                 ; & |
      (jove-finish-op (if (eq ?& first) jove-BITWISE-AND jove-BITWISE-OR) 1)))))

(defun jove-read-token-caret ()
  "Read a token starting with a ?^."
  (if (eq ?= (jove-peek-char))
      (jove-finish-op jove-ASSIGN 2)
    (jove-finish-op jove-BITWISE-XOR 1)))

(defun jove-read-token-plus-min (first)
  "Read a token starting with a ?+ or ?-, FIRST indicates which."
  (let ((second (jove-peek-char)))
    (cond
     ((eq first second)
      (jove-finish-op jove-INC-DEC 2))
     ((eq ?= second)
      (jove-finish-op jove-ASSIGN 2))
     (t
      (jove-finish-op jove-PLUS-MIN 1)))))

;; Not implementing XML-style comments for now <!--
(defun jove-read-token-lt-gt (first)
  "Read a token starting with a ?< or ?>, FIRST indicates which."
  (if (and (eq ?< first)
           jove--expr-allowed
           jove-jsx-allowed
           (not (eq ?\! (jove-peek-char)))) ; <!
      (jove-finish-punc jove-JSX-TAG-START)
    (let ((second (jove-peek-char))
          (third (jove-peek-char 2)))
      (if (eq first second)
          ;; << >> >>> <<= >>= >>>=
          (if (eq ?> third)
              (if (eq ?= (jove-peek-char 3))
                  (jove-finish-op jove-ASSIGN 4)  ; >>>=
                (jove-finish-op jove-BITSHIFT 3)) ; >>>
            (if (eq ?= third)
                (jove-finish-op jove-ASSIGN 3)    ; <<= >>=
              (jove-finish-op jove-BITSHIFT 2)))  ; << >>
        ;; < > <= >=
        (jove-finish-op jove-RELATIONAL (if (eq ?= second) 2 1))))))

(defun jove-read-token-eq-excl (first)
  "Read a token starting with a ?= or ?!, FIRST indicates which."
  (let ((second (jove-peek-char)))
    (cond
     ((eq ?> second)                    ; =>
      (forward-char 2)
      (jove-finish-token jove-ARROW))
     ((eq ?= second)                    ; == != === !==
      (jove-finish-op jove-EQUALITY (if (eq ?= (jove-peek-char 2)) 3 2)))
     (t                                 ; = !
      (jove-finish-op (if (eq ?= first) jove-EQ jove-PREFIX) 1)))))

(defsubst jove-buffer-to-number (start end radix)
  "Attempt to read a number from buffer from START to END in RADIX."
  (string-to-number (buffer-substring-no-properties start end) radix))

(defun jove-read-code-point ()
  "Read Unicode escape from buffer.
Return an integer representing the Unicode character if valid,
otherwise nil."
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
          ;; Only read the number if it is valid.
          (jove-buffer-to-number start (point) 16)
        (jove-eat-re "[0-9A-Fa-f]\\{1,3\\}")
        (jove-warn (- start 2) (point) "Invalid Unicode escape sequence")))))

(defun jove-read-escape-char ()
  "Read JavaScript escape sequence from buffer."
  (forward-char)                        ; Move over '\'
  (let ((start (1- (point)))
        (char (char-after)))
    ;; If invalid escapes are found attempt to warn about them.
    (cond
     ((eq ?u char)
      (forward-char)
      (jove-read-code-point))
     ((eq ?x char)
      (forward-char)
      (unless (jove-eat-re "[0-9A-Fa-f]\\{2\\}")
        (jove-eat-re jove-hexadecimal-re)
        (jove-warn start (point) "Invalid hexadecimal escape")))
     ((<= ?0 char ?7)
      ;; Don't warn about octals in strict or template strings.
      (jove-eat-re "[0-7]\\{1,3\\}"))
     (t                                 ; Any other escape
      (forward-char)))))

(defun jove-read-regex ()
  "Read a regular expression."
  (if (jove-eat-re jove-regular-expression-re)
      (when (< 0 (skip-syntax-forward "w_"))
        (jove-warn jove--end (point) "invalid identifier after regex"))
    (jove-warn (point) (goto-char (point-at-eol)) "invalid regex"))
  (jove-finish-token jove-REGEX 'font-lock-string-face))

(defun jove-read-string (punc)
  "Read a string.
Search for ending delimiter matching PUNC.  Signal error if the
eol or eof is reached before the matching delimiter."
  (unless (jove-eat-re (if (eq ?\' punc)
                       jove-string-single-quote-re
                     jove-string-double-quote-re))
    (goto-char (point-at-eol))
    (jove-warn jove--start (point) "missing string closing delimiter"))
  (jove-finish-token jove-STRING 'font-lock-string-face))

(defun jove-read-tmpl-token (char)
  "Read a template string."
  (let ((pos jove--start))
    (catch 'token
      (while t
        (setq pos (re-search-forward "[^`$\\\\\C-j]*" nil t)
              char (char-after))
        (cond
         ((eq ?\` char)
          (if (and (= jove--start pos)
                   (eq jove-TEMPLATE jove--tt))
              (progn
                (forward-char)
                (jove-finish-token jove-BACKQUOTE 'font-lock-string-face)
                (throw 'token nil))
            (jove-finish-token jove-TEMPLATE 'font-lock-string-face)
            (throw 'token nil)))
         ((eq ?\$ char)
          (if (eq ?\{ (jove-peek-char))
              (if (and (= jove--start pos)
                       (eq jove-TEMPLATE jove--tt))
                  (progn
                    (forward-char 2)
                    (jove-finish-token jove-DOLLAR-BRACE-L)
                    (throw 'token nil))
                (jove-finish-token jove-TEMPLATE 'font-lock-string-face)
                (throw 'token nil))
            ;; Its possible to catch a single '$'.
            (forward-char)))
         ((eq ?\\ char)
          (jove-read-escape-char))
         ;; TODO: Write a test to insure linum is incremented.
         ((eq ?\C-j char)
          ;; Increment line number.
          (jove-inc-linum)
          (forward-char))
         (t                             ; Hit eob.
          ;; Don't run the template hook because this isn't a real template literal.
          (jove-warn jove--start (goto-char (point-max)) "missing template string closing delimiter")
          (jove-finish-token jove-TEMPLATE)
          (throw 'token nil)))))))

(defun jove-read-word-escape ()
  "Read an escape in an identifier."
  ;; Already know there was a '\'
  (forward-char 2)
  (if (eq ?u (char-before))
        ;; The function below will warn of any invalid code points.
      (jove-read-code-point)
    (jove-warn (- (point) 2) (point) "invalid escape in identifier")))

(defun jove-read-word-internal ()
  "Read ECMAScript Identifier."
  (let (chunk
        invalid
        (looking t)
        (start (point)))
    (when (eq ?\\ (char-after))
      (setq jove--contains-esc t
            chunk (jove-read-word-escape)
            invalid (or invalid
                        (not chunk)
                        (not (eq ?w (char-syntax chunk)))))
      (if invalid
          (jove-warn jove--start (point) "Invalid identifier start character")
        (jove-add-to-string (string chunk)))) ; Here chunk is a character.
    (setq start (point))
    (while looking
      (cond
       ((eq ?\\ (char-after))
        (setq jove--contains-esc t)
        (setq chunk (jove-read-word-escape))
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

(defun jove-read-word ()
  "Read from buffer an ECMAScript Identifier Name or Keyword."
  (setq jove--contains-esc nil)
  (let ((word (jove-read-word-internal))
        (tt jove-NAME))
    (when word
      (setq tt (or (and (not jove--contains-esc)
                        (gethash word jove-keywords))
                   jove-NAME)))
    (jove-finish-token tt
                   (or (and (or (jove-tt-is-atom tt)
                                (memq tt (list jove-THIS jove-SUPER)))
                            'font-lock-builtin-face)
                       (and (jove-tt-is-keyword tt)
                            'font-lock-keyword-face))
                   word)))

(defun jove-jsx-read-expr-token (char)
  "Read token from inside a JSX expression context."
  (let ((looking t))
    (while looking
      (re-search-forward "[^<{\C-j]*")
      (setq char (char-after))
      (cond
       ((or (eq ?< char)
            (eq ?{ char))
        (if (= jove--start (point))
            (if (and (eq ?< char)
                     jove--expr-allowed)
                (progn
                  (forward-char)
                  (jove-finish-token jove-JSX-TAG-START))
              ;; Read as a regular token.
              (jove-read-token char))
          (jove-finish-token jove-JSX-TEXT))
        (setq looking nil))
       ;; Is parsing entities necessary? Might be helpful to hightlight them.
       ;; ((eq ?& char)
       ;;  (jove-read-entity))
       ((eq ?\C-j char)
        (forward-char)
        (jove-inc-linum))
       (t                               ; Hit eob.
        (setq looking nil)
        ;; Is it necessary to finish the token in anyway?
        (jove-warn jove--start (point) "Unterminated JSX contents"))))))

(defun jove-jsx-read-tag-token (char)
  "Read token from inside a JSX tag context."
  (cond
   ((eq ?w (char-syntax char))
    (jove-jsx-read-word))
   ((eq ?> char)
    (forward-char)
    (jove-finish-token jove-JSX-TAG-END))
   ((memq char '(?\" ?\'))
    (forward-char)
    (jove-jsx-read-string char))
   (t
    ;; Kludge
    (if (or (memq (char-syntax char) '(?w ?\\)))
        (jove-read-word)
      (jove-read-token char)))))

(defun jove-jsx-read-string (punc)
  ;; Not doing anything with the html entities.
  (let (char
        (regexp (if (eq ?\' punc) "[^'\C-j]*" "[^\"\C-j]*"))
        (looking t))
    (while looking
      (re-search-forward regexp nil t)
      (setq char (char-after))
      (cond
       ((eq punc char)
        (forward-char)
        (setq looking nil))
       ((eq ?\C-j char)
        (jove-inc-linum)
        (forward-char))
       (t                               ; Hit eob.
        (setq looking nil)
        (jove-warn jove--start (point) "Missing string closing delimiter"))))
    ;; Finish as a regular string token type.
    (jove-finish-token jove-STRING 'font-lock-string-face)))

(defun jove-jsx-read-word ()
  ;; First character will have already be checked for an
  ;; identifier start character.
  (while (or (< 0 (skip-syntax-forward "w_"))
             (and (eq ?- (char-after))
                  (progn (forward-char) t))))
  (jove-finish-token jove-JSX-NAME)
  (setq jove--value (buffer-substring-no-properties jove--start jove--end)))

(defun jove-read-token (char)
  "Read token using provided first CHAR."
  (cond
   ((eq ?\. char) (jove-read-token-dot))
   ((eq ?\( char) (jove-finish-punc jove-PAREN-L))
   ((eq ?\) char) (jove-finish-punc jove-PAREN-R))
   ((eq ?\; char) (jove-finish-punc jove-SEMI))
   ((eq ?\, char) (jove-finish-punc jove-COMMA))
   ((eq ?\[ char) (jove-finish-punc jove-BRACKET-L))
   ((eq ?\] char) (jove-finish-punc jove-BRACKET-R))
   ((eq ?{ char) (jove-finish-punc jove-BRACE-L))
   ((eq ?} char) (jove-finish-punc jove-BRACE-R))
   ((eq ?? char) (jove-finish-punc jove-QUESTION))
   ((eq ?: char) (jove-finish-punc jove-COLON))
   ((eq ?\` char)
    (jove-finish-punc jove-BACKQUOTE)
    (setq jove--face 'font-lock-string-face))
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
      (jove-finish-token jove-NUM)))
   ((<= ?1 char ?9)
    (jove-eat-re jove-number-re)
    (jove-finish-token jove-NUM))
   ((memq char '(?\' ?\")) (jove-read-string char))
   ((eq ?\/ char) (jove-read-token-slash))
   ((memq char '(?* ?%)) (jove-read-token-mult-modulo-exp char))
   ((memq char '(?\| ?\&)) (jove-read-token-pipe-amp char))
   ((eq ?^ char) (jove-read-token-caret))
   ((memq char '(?+ ?-)) (jove-read-token-plus-min char))
   ((memq char '(?< ?>)) (jove-read-token-lt-gt char))
   ((memq char '(?= ?!)) (jove-read-token-eq-excl char))
   ((eq ?~ char) (jove-finish-op jove-PREFIX 1))
   (t
    ;; Skip and try again.
    (jove-warn (point) (1+ (point)) "unexpected character")
    (forward-char)
    (jove-next-token))))

(defun jove-next-token ()
  "Read next token."
  (let ((char nil)
        (ctx (jove-current-ctx)))
    (when (or (not ctx)
              (not (jove-ctx-preserve-space ctx)))
      (jove-skip-space))
    ;; Initialize current token state.
    (setq jove--start (point)
          jove--value nil
          char (char-after))
    (cond
     ((eobp)
      (jove-finish-token jove-EOB))
     ((jove-ctx-override ctx)
      (funcall (jove-ctx-override ctx) char))
     ((or (memq (char-syntax char) '(?w ?\\)))
      (jove-read-word))
     (t
      (jove-read-token char)))))

(defun jove-config-lexer (&optional state)
  "Configure the lexer global variables."
  ;; TODO: This is not very elegent.
  (if state
      (setq jove--start (nth 0 state)
            jove--end (nth 1 state)
            jove--tt (nth 2 state)
            jove--value (nth 3 state)
            jove--linum (nth 4 state)
            jove--expr-allowed (nth 5 state)
            jove--newline-before (nth 6 state)
            jove--ctx-stack (nth 7 state))
    ;; Create an initial lexer state.
    (setq jove--start 1
          jove--end 1
          jove--tt jove-BOB
          jove--value nil
          jove--linum 1
          jove--expr-allowed t
          jove--newline-before nil
          jove--ctx-stack (jove-initial-ctx)))
  (goto-char jove--end))

(defun jove-flush-lexer-cache (&optional position)
  "Flush the token cache to a position before the last edit."
  (if (numberp position)
      (progn
        (while (and jove--cache
                    (< position (car (cdr (car jove--cache)))))
          (setq jove--cache (cdr jove--cache)))
        (setq jove--cache-end (if jove--cache
                              (car (cdr (car jove--cache)))
                            1)))
    (setq jove--cache-end 1
          jove--cache nil)))

;; Need to really think though the process of caching the tokens
;; and how to resume once the cache is used up.

;; For right not the cache will not be used by the parser...

(defun jove-lex-query (pos)
  "Query the lexer cache for a lexer state before pos."
  (let ((cache jove--cache))
    (while (and cache
                (< pos (car (cdr (car cache))))) ; End position
      (setq cache (cdr cache)))
    cache))

(defun jove-lex ()
  "Tokenize buffer contents as JavaScript."
  ;; NOTE: This function is just for testing the speed of the lexer.
  (save-restriction
    (widen)
    (save-excursion
      (let ((start-time (float-time)))
        (if (car jove--cache)
            (jove-config-lexer (car jove--cache))
          (jove-config-lexer)
          (jove-next-token)                 ; Skip over `jove-BOB'.
          (push (jove-copy-lexer-state) jove--cache))
        (save-match-data
          (while (not (eq jove-EOB jove--tt))
            (jove-next-token)
            (push (jove-copy-lexer-state) jove--cache))
          (let ((time (/ (truncate (* (- (float-time) start-time)
                                      10000))
                         10000.0)))
            (message "Lexer finished in %0.3fsec, count %d" time (length jove--cache)))))
      (setq jove--cache-end (point)))))

;; TODO: This function needs to not flush the cache.
;; If the cache exists use it, but don't modify it. Maybe it should
;; return a seperate stack of tokens than the cache, while also
;; loading the desired token into the global variables.

(defun jove-lex-to (&optional position)
  "Tokenize buffer contents as JavaScript."
  ;; NOTE: This function is just for testing the speed of the lexer.
  ;; The strategy is to lex to the token where point is and leave that
  ;; data in the global variables, the previous tokens are in the cache.
  (let ((start-time (float-time))
        (looking t))
    ;; TODO: Don't flush cache if unnecessary. Need an easy way to use
    ;; previously lexed tokens which are in the cache, and then resume
    ;; lexing and caching once they are used up.
    (jove-flush-lexer-cache position)
    (save-restriction
      (widen)
      (save-excursion
        (save-match-data
          (if (car jove--cache)
              (jove-config-lexer (car jove--cache))
            ;; Kludge to handle the need of the lexer to start after `jove-BOB'.
            (jove-config-lexer)
            (jove-next-token))
          (while looking
            (if (or (eq jove-EOB jove--tt)
                    (> jove--end position))
                (setq looking nil)
              (push (jove-copy-lexer-state) jove--cache) ; Push previous
              (jove-next-token)))                    ; Advance
          (let ((time (/ (truncate (* (- (float-time) start-time)
                                      10000))
                         10000.0)))
            (message "Lexer finished in %0.3fsec, count %d" time (length jove--cache))))
        (setq jove--cache-end (or (car (car jove--cache)) 0))))))

(defun jove-find-closing-paren ()
  "Find a closing parenthesis.
Intended to be called while the lexer is still 'on' the opening
parenthesis.  Tokens are cache while searching."
  (let ((stack))
    (catch 'pos
      (while t
        (jove-next-token)
        (push (jove-copy-lexer-state) jove--cache)
        (cond
         ((and (eq jove-PAREN-R jove--tt)
               (not (pop stack)))
          (throw 'pos jove--start))
         ((eq jove-EOB jove--tt)
          (throw 'pos nil))
         ((eq jove-PAREN-L jove--tt)
          (push t stack)))
        (not (or (eq jove-PAREN-R jove--tt)
                 (eq jove-EOB jove--tt)))))))

(provide 'jove-lexer)

;;; jove-lexer.el ends here
