;;;; jove-token-types.el --- A JavaScript Mode -*- lexical-binding: t; -*-

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

(defvar jove-keywords (make-hash-table :test 'equal)
  "Hash table to map keyword names to token types.")

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

(cl-defun jove--tt-create (label &key keyword before-expr starts-expr
                             is-loop is-assign prefix postfix binop)
  "Return a vector representing a token type."
  (vector label
          keyword
          before-expr
          starts-expr
          is-loop
          is-assign
          prefix
          postfix
          binop))

;; 0 label
;; 1 keyword
;; 2 before-expr
;; 3 starts-expr
;; 4 is-loop
;; 5 is-assign
;; 6 prefix
;; 7 postfix
;; 8 binop
;;   update-ctx is handled in `jove--update-ctx'


(defsubst jove--tt-label (tt)
  (aref tt 0))
(defsubst jove--tt-keyword (tt)
  (aref tt 1))
(defsubst jove--tt-before-expr (tt)
  (aref tt 2))
(defsubst jove--tt-starts-expr (tt)
  (aref tt 3))
(defsubst jove--tt-is-loop (tt)
  (aref tt 4))
(defsubst jove--tt-is-assign (tt)
  (aref tt 5))
(defsubst jove--tt-is-prefix (tt)
  (aref tt 6))
(defsubst jove--tt-is-postfix (tt)
  (aref tt 7))
(defsubst jove--tt-binop (tt)
  (aref tt 8))

(defun jove--binop-create (label prec)
  "Return a vector representing a binary operator token type."
  (jove--tt-create label :before-expr t :binop prec))

(defmacro jove--keyword-create (label &rest options)
  "Return a vector representing a keyword token type."
  `(puthash (intern ,label)
            (jove--tt-create ,label :keyword t ,@options)
            jove-keywords))

(defconst jove-NUM (jove--tt-create "num" :starts-expr t))
(defconst jove-REGEXP (jove--tt-create "regexp" :starts-expr t))
(defconst jove-STRING (jove--tt-create "string" :starts-expr t))
(defconst jove-NAME (jove--tt-create "name" :starts-expr t))
(defconst jove-EOF (jove--tt-create "eof"))

;;; Punctuation Token Types

(defconst jove-BRACE-L (jove--tt-create "{" :before-expr t :starts-expr t))
(defconst jove-BRACE-R (jove--tt-create "}"))
(defconst jove-PAREN-L (jove--tt-create "(" :before-expr t :starts-expr t))
(defconst jove-PAREN-R (jove--tt-create ")"))
(defconst jove-BRACKET-L (jove--tt-create "[" :before-expr t :starts-expr t))
(defconst jove-BRACKET-R (jove--tt-create "]"))
(defconst jove-COMMA (jove--tt-create "," :before-expr t))
(defconst jove-SEMI (jove--tt-create ";" :before-expr t))
(defconst jove-COLON (jove--tt-create ":" :before-expr t))
(defconst jove-DOT (jove--tt-create "."))
(defconst jove-QUESTION (jove--tt-create "?" :before-expr t))
(defconst jove-ARROW (jove--tt-create "=>" :before-expr t))
(defconst jove-TEMPLATE (jove--tt-create "template"))
(defconst jove-ELLIPSIS (jove--tt-create "..." :before-expr t))
(defconst jove-DOLLAR-BRACE-L (jove--tt-create "${" :before-expr t :starts-expr t))
(defconst jove-BACKQUOTE (jove--tt-create "`" :before-expr t))

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

(defconst jove-EQ (jove--tt-create "=" :before-expr t :is-assign t))
(defconst jove-ASSIGN (jove--tt-create "_=" :before-expr t :is-assign t))
(defconst jove-INC-DEC (jove--tt-create "++/--" :starts-expr t :prefix t :postfix t))
(defconst jove-PREFIX (jove--tt-create "prefix" :before-expr t :starts-expr t :prefix t))
(defconst jove-LOGICAL-OR (jove--binop-create "||" 1))
(defconst jove-LOGICAL-AND (jove--binop-create "&&" 2))
(defconst jove-BITWISE-OR (jove--binop-create "|" 3))
(defconst jove-BITWISE-XOR (jove--binop-create "^" 4))
(defconst jove-BITWISE-AND (jove--binop-create "&" 5))
(defconst jove-EQUALITY (jove--binop-create "==/!=" 6)) ; == != === !==
(defconst jove-RELATIONAL (jove--binop-create "</>" 7)) ; < > <= >=
(defconst jove-BITSHIFT (jove--binop-create "<</>>" 8)) ; << >> >>>
(defconst jove-PLUS-MIN (jove--tt-create "+/-"
                                 :binop 9
                                 :before-expr t
                                 :starts-expr t
                                 :prefix t))
(defconst jove-MODULO (jove--binop-create "%" 10))
(defconst jove-STAR (jove--binop-create "*" 10))
(defconst jove-SLASH (jove--binop-create "/" 10))
(defconst jove-STARSTAR (jove--tt-create "**" :before-expr t))

;;; Keywords Token Types

(defconst jove-BREAK (jove--keyword-create "break"))
(defconst jove-CASE (jove--keyword-create "case" :before-expr t))
(defconst jove-CATCH (jove--keyword-create "catch"))
(defconst jove-CONTINUE (jove--keyword-create "continue"))
(defconst jove-DEBUGGER (jove--keyword-create "debugger"))
(defconst jove-DEFAULT (jove--keyword-create "default"))
(defconst jove-DO (jove--keyword-create "do" :is-loop t :before-expr t))
(defconst jove-ELSE (jove--keyword-create "else" :before-expr t))
(defconst jove-FINALLY (jove--keyword-create "finally"))
(defconst jove-FOR (jove--keyword-create "for" :is-loop t))
(defconst jove-FUNCTION (jove--keyword-create "function" :starts-expr t))
(defconst jove-IF (jove--keyword-create "if"))
(defconst jove-RETURN (jove--keyword-create "return" :before-expr t))
(defconst jove-SWITCH (jove--keyword-create "switch"))
(defconst jove-THROW (jove--keyword-create "throw" :before-expr t))
(defconst jove-TRY (jove--keyword-create "try"))
(defconst jove-VAR (jove--keyword-create "var"))
(defconst jove-CONST (jove--keyword-create "const"))
(defconst jove-WHILE (jove--keyword-create "while" :is-loop t))
(defconst jove-WITH (jove--keyword-create "with"))
(defconst jove-NEW (jove--keyword-create "new" :before-expr t :starts-expr t))
(defconst jove-THIS (jove--keyword-create "this" :starts-expr t))
(defconst jove-SUPER (jove--keyword-create "super" :starts-expr t))
(defconst jove-CLASS (jove--keyword-create "class"))
(defconst jove-EXTENDS (jove--keyword-create "extends" :before-expr t))
(defconst jove-EXPORT (jove--keyword-create "export"))
(defconst jove-IMPORT (jove--keyword-create "import"))
(defconst jove-NULL (jove--keyword-create "null" :starts-expr t))
(defconst jove-UNDEFINED (jove--keyword-create "undefined" :starts-expr t))
(defconst jove-TRUE (jove--keyword-create "true" :starts-expr t))
(defconst jove-FALSE (jove--keyword-create "false" :starts-expr t))
(defconst jove-IN (jove--keyword-create "in" :binop 7 :before-expr t))
(defconst jove-INSTANCEOF (jove--keyword-create "instanceof" :binop 7 :before-expr t))
(defconst jove-TYPEOF (jove--keyword-create "typeof" :prefix t :before-expr t :starts-expr t))
(defconst jove-VOID (jove--keyword-create "void" :prefix t :before-expr t :starts-expr t))
(defconst jove-DELETE (jove--keyword-create "delete" :prefix t :before-expr t :starts-expr t))

(provide 'jove-token-types)

;;; jove-token-types.el ends here
