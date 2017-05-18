;;;; jove-lexer-test.el --- Tests for the Jove Lexer -*- lexical-binding: t; -*-

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

(require 'ert)
(require 'jove-mode)

(defun jove-test-lexer (depth token)
  (let ((index 1)                       ; HACK
        (looping t))
    (jove-next-token)                       ; Load initial token.
    (while looping
      (if (or (eq jove-EOB jove--tt)
              (>= index depth))
          (setq looping nil)
        (jove-next-token)
        (setq index (1+ index))))
    (should (equal (nth 0 token)        ; start
                   jove--start))
    (should (equal (nth 1 token)        ; end
                   jove--end))
    (should (equal (nth 2 token)        ; tt
                   jove--tt))
    ;; Only test value if provided.
    (when (nth 3 token)
      (should (equal (nth 3 token)      ; value
                     jove--value)))))

(defun jove-test-lexer-warn (start end)
  (jove-next-token)                       ; Load initial token.
  (while (not (eq jove-EOB jove--tt))
    (jove-next-token))
  (let ((warning (car jove--warnings)))
    (should (vectorp warning))
    (should (equal start (aref warning 0)))
    (should (equal end (aref warning 1)))))

(cl-defmacro jove-test-env (content bind func)
  ;; The test environment.
  `(let ,(append '()                    ; Put any default variables here.
                 bind)
     (with-temp-buffer
       (insert ,content)
       (jove-mode)
       (jove-disable-parser)
       (jove-config-lexer)
       (funcall ,func))))

(cl-defmacro jove-deftest-lexer (name content tt &key bind value length (start 1) (depth 1))
  (declare (indent defun))
  `(ert-deftest ,(intern (format "jove-test-lex-%s" name)) ()
     (jove-test-env ,content ,bind
                #'(lambda ()
                    (jove-test-lexer ,depth (list ,start ,(+ start length) ,tt ,value))))))

(cl-defmacro jove-deftest-lexer-warning (name content &key start end bind)
  (declare (indent defun))
  `(ert-deftest ,(intern (format "jove-test-lex-warning-%s" name)) ()
     (jove-test-env ,content ,bind
                #'(lambda ()
                    (jove-test-lexer-warn ,start ,end)))))


;;; Punctuation

(jove-deftest-lexer punc-brace-l
  "{" jove-BRACE-L :length 1)

(jove-deftest-lexer punc-brace-r
  "}" jove-BRACE-R :length 1)

(jove-deftest-lexer punc-paren-l
  "(" jove-PAREN-L :length 1)

(jove-deftest-lexer punc-paren-r
  ")" jove-PAREN-R :length 1)

(jove-deftest-lexer punc-bracket-l
  "[" jove-BRACKET-L :length 1)

(jove-deftest-lexer punc-bracket-r
  "]" jove-BRACKET-R :length 1)

(jove-deftest-lexer punc-comma
  "," jove-COMMA :length 1)

(jove-deftest-lexer punc-semi
  ";" jove-SEMI :length 1)

(jove-deftest-lexer punc-colon
  ":" jove-COLON :length 1)

(jove-deftest-lexer punc-dot
  "." jove-DOT :length 1)

(jove-deftest-lexer punc-question
  "?" jove-QUESTION :length 1)

(jove-deftest-lexer punc-arrow
  "=>" jove-ARROW :length 2)

(jove-deftest-lexer punc-ellipsis
  "..." jove-ELLIPSIS :length 3)

(jove-deftest-lexer punc-dollar-brace-l
  ;; An empty `jove-TEMPLATE' is read before the `jove-DOLLAR-BRACE-L',
  ;; hence the :depth of 3.
  "`${" jove-DOLLAR-BRACE-L :length 2 :start 2 :depth 3)

(jove-deftest-lexer punc-backquote
  "`" jove-BACKQUOTE :length 1)

;;; Operators

(jove-deftest-lexer op-slash
  "4/5" jove-SLASH :start 2 :length 1 :depth 2)

(jove-deftest-lexer op-slash-after-single-line-comment
  "// foo bar \n 4/4" jove-SLASH :start 15 :length 1 :depth 2)

(jove-deftest-lexer op-assign-division
  "foo/=10" jove-ASSIGN :start 4 :length 2 :depth 2)

(jove-deftest-lexer op-star
  "*" jove-STAR :length 1)

(jove-deftest-lexer op-star-assign
  "*=" jove-ASSIGN :length 2)

(jove-deftest-lexer op-starstar
  "**" jove-STARSTAR :length 2)

(jove-deftest-lexer op-starstar-assign
  "**=" jove-ASSIGN :length 3)

(jove-deftest-lexer op-modulo
  "%" jove-MODULO :length 1)

(jove-deftest-lexer op-modulo-assign
  "%=" jove-ASSIGN :length 2)

(jove-deftest-lexer op-logical-and
  "&&" jove-LOGICAL-AND :length 2)

(jove-deftest-lexer op-logical-or
  "||" jove-LOGICAL-OR :length 2)

(jove-deftest-lexer op-bitwise-and
  "&" jove-BITWISE-AND :length 1)

(jove-deftest-lexer op-bitwise-and-assign
  "&=" jove-ASSIGN :length 2)

(jove-deftest-lexer op-bitwise-or
  "|" jove-BITWISE-OR :length 1)

(jove-deftest-lexer op-bitwise-or-assign
  "|=" jove-ASSIGN :length 2)

(jove-deftest-lexer op-bitwise-xor
  "^" jove-BITWISE-XOR :length 1)

(jove-deftest-lexer op-bitwise-xor-assign
  "^=" jove-ASSIGN :length 2)

(jove-deftest-lexer op-increment
  "++" jove-INC-DEC :length 2)

(jove-deftest-lexer op-decrement
  "--" jove-INC-DEC :length 2)

(jove-deftest-lexer op-plus
  "+" jove-PLUS-MIN :length 1)

(jove-deftest-lexer op-plus-assign
  "+=" jove-ASSIGN :length 2)

(jove-deftest-lexer op-minus
  "-" jove-PLUS-MIN :length 1)

(jove-deftest-lexer op-minus-assign
  "-=" jove-ASSIGN :length 2)

(jove-deftest-lexer op-greater-than
  ">" jove-RELATIONAL :length 1)

(jove-deftest-lexer op-greater-than-or-equal
  ">=" jove-RELATIONAL :length 2)

(jove-deftest-lexer op-less-than
  "<" jove-RELATIONAL :length 1)

(jove-deftest-lexer op-less-than-or-equal
  "<=" jove-RELATIONAL :length 2)

(jove-deftest-lexer op-bitshift-left
  "<<" jove-BITSHIFT :length 2)

(jove-deftest-lexer op-bitshift-left-assign
  "<<=" jove-ASSIGN :length 3)

(jove-deftest-lexer op-bitshift-right
  ">>" jove-BITSHIFT :length 2)

(jove-deftest-lexer op-bitshift-right-assign
  ">>=" jove-ASSIGN :length 3)

(jove-deftest-lexer op-bitshift-right-zero
  ">>>" jove-BITSHIFT :length 3)

(jove-deftest-lexer op-bitshift-right-zero-assign
  ">>>=" jove-ASSIGN :length 4)

(jove-deftest-lexer op-eq
  "=" jove-EQ :length 1)

(jove-deftest-lexer op-negation
  "!" jove-PREFIX :length 1)

(jove-deftest-lexer op-equal
  "==" jove-EQUALITY :length 2)

(jove-deftest-lexer op-equal-strict
  "===" jove-EQUALITY :length 3)

(jove-deftest-lexer op-not-equal
  "!=" jove-EQUALITY :length 2)

(jove-deftest-lexer op-not-equal-strict
  "!==" jove-EQUALITY :length 3)

;;; Names & Identifiers

(jove-deftest-lexer name
  "foo" jove-NAME :length 3 :value "foo")

(jove-deftest-lexer name-dollar
  "foo$" jove-NAME :length 4 :value "foo$")

(jove-deftest-lexer name-dollar-start
  "$foo" jove-NAME :length 4 :value "$foo")

(jove-deftest-lexer name-underscore
  "foo_bar" jove-NAME :length 7 :value "foo_bar")

(jove-deftest-lexer name-underscore-start
  "_foo" jove-NAME :length 4 :value "_foo")

(jove-deftest-lexer name-number-end
  "foo256" jove-NAME :length 6 :value "foo256")

(jove-deftest-lexer name-unicode-escape
  "\\u0066oo" jove-NAME :length 8 :value "foo")

(jove-deftest-lexer name-unicode-code-point
  "\\u{62}ar" jove-NAME :length 8 :value "bar")

(jove-deftest-lexer keyword-unicode-escape
  "\\u0073witch" jove-SWITCH :length 11 :value "switch")

;;; Name & Identifier Warnings

(jove-deftest-lexer-warning identifier-invalid-escape
  "foo\\x0035" :start 4 :end 6)

(jove-deftest-lexer-warning identifier-invalid-start
  "\\u0010foo" :start 1 :end 7)

(jove-deftest-lexer-warning identifier-invalid-part
  "foo\\u0010" :start 4 :end 10)

;;; Numbers

(jove-deftest-lexer integer-zero
  "0" jove-NUM :length 1)

(jove-deftest-lexer float-zero
  "0.0" jove-NUM :length 3)

(jove-deftest-lexer integer-base-two
  "0b10000000000000000" jove-NUM :length 19)

(jove-deftest-lexer integer-base-two-upper
  "0B10000000000000000" jove-NUM :length 19)

(jove-deftest-lexer integer-base-eight
  "0o200000" jove-NUM :length 8)

(jove-deftest-lexer integer-base-eight-upper
  "0O200000" jove-NUM :length 8)

(jove-deftest-lexer integer-base-eight-leading-zero
  "0200000" jove-NUM :length 7)

(jove-deftest-lexer integer-base-ten
  "65536" jove-NUM :length 5)

(jove-deftest-lexer integer-base-twelve
  "0x10000" jove-NUM :length 7)

(jove-deftest-lexer integer-base-twelve-upper
  "0X10000" jove-NUM :length 7)

(jove-deftest-lexer float-basic
  "65536.0" jove-NUM :length 7)

(jove-deftest-lexer float-exponent
  "6.5536e4" jove-NUM :length 8)

(jove-deftest-lexer float-exponent-positive
  "6.5536e+4" jove-NUM :length 9)

(jove-deftest-lexer float-exponent-negative
  "655360000e-4" jove-NUM :length 12)

(jove-deftest-lexer float-exponent-capital
  "6.5536E+4" jove-NUM :length 9)

(jove-deftest-lexer float-start-with-dot
  ".65536e5" jove-NUM :length 8)

;;; Strings

(jove-deftest-lexer string-single-quotes
  "'hello foo'" jove-STRING :length 11)

(jove-deftest-lexer string-single-quotes-escape-single-quote
  "'hello \\\'foo\\\''" jove-STRING :length 15)

(jove-deftest-lexer string-double-quotes
  "\"hello foo\"" jove-STRING :length 11)

(jove-deftest-lexer string-double-quotes-escape-double-quote
  "\"hello \\\"foo\\\"\"" jove-STRING :length 15)

(jove-deftest-lexer string-hex-escape
  "\"fo\\x6f bar\"" jove-STRING :length 12)

(jove-deftest-lexer string-escape-sequence
  "'foo \\u0620ar'" jove-STRING :length 14)

(jove-deftest-lexer string-code-point
  "'foo \\u{62}ar'" jove-STRING :length 14)

(jove-deftest-lexer string-allow-octal-in-non-strict
  "\"foo \\42ar\"" jove-STRING :length 11
  :bind ((jove--strict nil)))

;;; String Warnings

(jove-deftest-lexer-warning string-missing-delimiter
  "\"foo bar baz" :start 1 :end 13)

;;; Template Strings

(jove-deftest-lexer template-basic
  "`foo bar`" jove-TEMPLATE :length 7 :start 2 :depth 2)

(jove-deftest-lexer template-hex-escape
  "`fo\\x6f bar`" jove-TEMPLATE :length 10 :start 2 :depth 2)

(jove-deftest-lexer template-escape-sequence
  "`foo \\u0062ar`" jove-TEMPLATE :length 12 :start 2 :depth 2)

(jove-deftest-lexer template-code-point
  "`foo \\u{62}ar`" jove-TEMPLATE :length 12 :start 2 :depth 2)

;;; Template String Warnings

;; (jove-deftest-lexer-warning template-invalid-octal
;;   "`foo \\42ar`" :start 6 :end 9)

;;; Regular Expressions

(jove-deftest-lexer regexp-no-flags
  "/foo ba[rR]/" jove-REGEXP :length 12)

(jove-deftest-lexer regexp-with-flags
  "/foo ba[rR]/gi" jove-REGEXP :length 14)

(jove-deftest-lexer regexp-escape-closing-delimiter
  "/foo\\/bar/" jove-REGEXP :length 10)

(jove-deftest-lexer regexp-forward-slash-in-class
  "/foo[/]bar/" jove-REGEXP :length 11)

(jove-deftest-lexer regexp-escape-closing-class-delimiter
  "/foo[\\]]bar/" jove-REGEXP :length 12)

(jove-deftest-lexer regexp-with-octal-escape
  "/f[oO]*\\142a[rR]/" jove-REGEXP :length 17 :bind ((jove--strict nil)))

(jove-deftest-lexer regexp-with-octal-escape-in-class
  "/f[oO]*ba[\\162R]/" jove-REGEXP :length 17 :bind ((jove--strict nil)))

(jove-deftest-lexer regexp-with-hex-escape
  "/f[oO]*\\x62a[rR]/" jove-REGEXP :length 17)

(jove-deftest-lexer regexp-with-hex-escape-in-class
  "/f[oO]*ba[\\x72R]/" jove-REGEXP :length 17)

(jove-deftest-lexer regexp-with-escape-sequence
  "/f[oO]*\\u0062a[rR]/" jove-REGEXP :length 19)

(jove-deftest-lexer regexp-with-escape-sequence-in-class
  "/f[oO]*ba[\\u0072R]/" jove-REGEXP :length 19)

(jove-deftest-lexer regexp-with-code-point
  "/f[oO]*\\u{62}a[rR]/" jove-REGEXP :length 19)

(jove-deftest-lexer regexp-with-code-point-in-class
  "/f[oO]*ba[\\u{72}R]/" jove-REGEXP :length 19)

;;; Regular Expression Warnings

(jove-deftest-lexer-warning regexp-missing-delimiter
  "/ba[rz]" :start 1 :end 8)

(jove-deftest-lexer-warning regexp-missing-delimiter-class
  "/ba[rz/" :start 1 :end 8)

;;; Keywords

(mapc #'(lambda (tt)
          (eval
           `(jove-deftest-lexer
              ,(intern (format "keyword-%s" (symbol-name tt)))
              ,(downcase (symbol-name tt))
              ,(intern (format "jove-%s" (symbol-name tt)))
              :length ,(length (symbol-name tt))
              :value ,(downcase (symbol-name tt)))))
      '(BREAK
        CASE
        CATCH
        CONTINUE
        DEBUGGER
        DEFAULT
        DO
        ELSE
        FINALLY
        FOR
        FUNCTION
        IF
        RETURN
        SWITCH
        THROW
        TRY
        VAR
        CONST
        WHILE
        WITH
        NEW
        THIS
        SUPER
        CLASS
        EXTENDS
        EXPORT
        IMPORT
        NULL
        TRUE
        FALSE
        IN
        INSTANCEOF
        TYPEOF
        VOID
        DELETE))

;;; Comment Errors

;; FIX: For some reason this test is staring an endless loop.
;; (jove-deftest-lexer-warning comment-missing-delimiter
;;   "foo /* bar \n qux" :start 5 :end 17)
