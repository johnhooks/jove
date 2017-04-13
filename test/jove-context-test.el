;;;; jove-context-test.el --- Tests for the Jove Lexer Context -*- lexical-binding: t; -*-

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

(require 'ert)
(require 'jove)

(defun jove-test-env (body)
  `(with-temp-buffer
     (jove-mode)
     ,@body))

(defmacro jove-deftest (name &rest body)
  (declare (indent defun))
  `(ert-deftest ,(intern (format "jove-test-ctx-%s" name)) ()
     ,(jove-test-env body)))

(cl-defmacro jove-deftest-ctx-update (name (type expect &key push prev) &rest body)
  (declare (indent defun))
  `(jove-deftest ,(format "update-%s" name) ; `jove-deftest' will add on prefix
     (let ((state (jove--lex-create)))
       ,@(mapcar #'(lambda (ctx)
                     `(jove--ctx-stack-push state ,ctx))
                 push)
       (jove--set-type state ,type)
       ,@body
       (jove--update-ctx state ,(or prev jove-EOF))
       (should (eq (jove--expr-allowed state) ,expect)))))

(cl-defmacro jove-deftest-ctx-stack (name (type expect &key push prev) &rest body)
  (declare (indent defun))
  `(jove-deftest ,(format "stack-%s" name) ; `jove-deftest' will add on prefix
     (let ((state (jove--lex-create)))
       ,@(mapcar #'(lambda (ctx)
                     `(jove--ctx-stack-push state ,ctx))
                 push)
       (jove--set-type state ,type)
       ,@body
       (jove--update-ctx state ,(or prev jove-EOF))
       (should (equal (jove--ctx-stack state) ,expect)))))

(cl-defmacro jove-deftest-ctx-before-expr (tt)
  `(jove-deftest ,(intern (format "before-expr-%s" (symbol-name tt)))
     (let ((state (jove--lex-create)))
       (jove--set-type state ,(intern (format "jove-%s" (symbol-name tt))))
       (jove--update-ctx state jove-EOF)
       (should (eq t (jove--expr-allowed state))))))

(cl-defmacro jove-deftest-ctx-not-before-expr (tt)
  `(jove-deftest ,(intern (format "not-before-expr-%s" (symbol-name tt)))
     (let ((state (jove--lex-create)))
       (jove--set-type state ,(intern (format "jove-%s" (symbol-name tt))))
       (jove--update-ctx state jove-EOF)
       (should (eq nil (jove--expr-allowed state))))))

(jove-deftest brace-is-block-colon-b-stat
  (let ((state (jove--lex-create)))
    (jove--ctx-stack-push state jove-B-STAT)
    (should (eq t (jove--brace-is-block-p state jove-COLON)))))

(jove-deftest brace-is-block-colon-b-expr
  (let ((state (jove--lex-create)))
    (jove--ctx-stack-push state jove-B-EXPR)
    (should (eq nil (jove--brace-is-block-p state jove-COLON)))))

(jove-deftest brace-is-block-return
  (let ((state (jove--lex-create)))
    (insert "return {}")
    (goto-char 1)
    (setq state (jove--next state))
    (jove--skip-space state)
    (should (eq nil (jove--brace-is-block-p state jove-RETURN)))))

(jove-deftest brace-is-block-return-newline
  (let ((state (jove--lex-create)))
    (insert "return \n {}")
    (goto-char 1)
    (setq state (jove--next state))
    (jove--skip-space state)
    (should (eq t (jove--brace-is-block-p state jove-RETURN)))))

(jove-deftest brace-is-block-else
  (should (eq t (jove--brace-is-block-p (jove--lex-create) jove-ELSE))))

(jove-deftest brace-is-block-semi
  (should (eq t (jove--brace-is-block-p (jove--lex-create) jove-SEMI))))

(jove-deftest brace-is-block-eof
  (should (eq t (jove--brace-is-block-p (jove--lex-create) jove-EOF))))

(jove-deftest brace-is-block-paren-r
  (should (eq t (jove--brace-is-block-p (jove--lex-create) jove-PAREN-R))))

(jove-deftest brace-is-block-brace-l-b-stat
  (let ((state (jove--lex-create)))
    (jove--ctx-stack-push state jove-B-STAT)
    (should (eq t (jove--brace-is-block-p state jove-BRACE-L)))))

(jove-deftest brace-is-block-expr-allowed
  ;; The argument to `jove--brace-is-block-p' is just a place holder.
  (let ((state (jove--lex-create)))
    (jove--set-expr-allowed state t)
    (should (eq nil (jove--brace-is-block-p state jove-ASSIGN)))))

(jove-deftest brace-is-block-expr-not-allowed
  ;; The argument to `jove--brace-is-block-p' is just a place holder.
  (let ((state (jove--lex-create)))
    (jove--set-expr-allowed state nil)
    (should (eq t (jove--brace-is-block-p state jove-BRACE-R)))))

;;; Update Context Functions

;; Update Context jove-BRACE-L

(jove-deftest-ctx-update brace-l
  ;; Whether or not brace is a block, expr-allowed should be t
  (jove-BRACE-L t))

(jove-deftest-ctx-stack brace-l-b-stat
  ;; When `jove--brace-is-block-p' returns t, `jove--update-ctx' pushs `jove-B-STAT'
  ;; on to the top of `jove--ctx-stack'.
  (jove-BRACE-L (list jove-B-STAT jove-B-STAT))
  (jove--set-expr-allowed state nil))     ; `jove--brace-is-block-p' returns t

(jove-deftest-ctx-stack brace-l-b-expr
  ;; When `jove--brace-is-block-p' returns nil, `jove--update-ctx' pushs `jove-B-EXPR'
  ;; on to the top of `jove--ctx-stack'.
  ;; HACK: `jove-COMMA' passes through `jove--brace-is-block-p', `jove-EOF' does not.
  (jove-BRACE-L (list jove-B-EXPR jove-B-STAT) :prev jove-COMMA)
  (jove--set-expr-allowed state t))

;;; Update Context jove-BRACE-R
;; Note: jove-PAREN-R uses the exact same function as well.

(jove-deftest-ctx-update brace-r
  (jove-BRACE-R t))

(jove-deftest-ctx-update brace-r-f-expr
  (jove-BRACE-R nil :push (jove-F-EXPR jove-B-STAT)))

(jove-deftest-ctx-update brace-r-b-tmpl
  (jove-BRACE-R t :push (jove-B-TMPL)))

(jove-deftest-ctx-update brace-r-b-expr
  (jove-BRACE-R nil :push (jove-B-EXPR)))

(jove-deftest-ctx-update brace-r-b-stat
  (jove-BRACE-R t :push (jove-B-STAT)))

(jove-deftest-ctx-stack brace-r-f-expr
  (jove-BRACE-R (list jove-B-STAT) :push (jove-F-EXPR jove-B-STAT)))

(jove-deftest-ctx-stack brace-r-b-tmpl
  (jove-BRACE-R (list jove-B-STAT) :push (jove-B-TMPL)))

(jove-deftest-ctx-stack brace-r-b-expr
  (jove-BRACE-R (list jove-B-STAT) :push (jove-B-EXPR)))

(jove-deftest-ctx-stack brace-r-b-stat
  (jove-BRACE-R (list jove-B-STAT) :push (jove-B-STAT)))

;;; Update Context jove-PAREN-L

(jove-deftest-ctx-update paren-l
  (jove-PAREN-L t))

(jove-deftest-ctx-stack paren-l-if
  (jove-PAREN-L (list jove-P-STAT jove-B-STAT) :prev jove-IF))

(jove-deftest-ctx-stack paren-l-for
  (jove-PAREN-L (list jove-P-STAT jove-B-STAT) :prev jove-FOR))

(jove-deftest-ctx-stack paren-l-with
  (jove-PAREN-L (list jove-P-STAT jove-B-STAT) :prev jove-WITH))

(jove-deftest-ctx-stack paren-l-while
  (jove-PAREN-L (list jove-P-STAT jove-B-STAT) :prev jove-WHILE))

;; Anything else should push jove-P-EXPR
(jove-deftest-ctx-stack paren-l-p-expr
  (jove-PAREN-L (list jove-P-EXPR jove-B-STAT)))

;;; Update Context jove-DOLLAR-BRACE-L

(jove-deftest-ctx-update dollar-brace-l
  (jove-DOLLAR-BRACE-L t))

(jove-deftest-ctx-stack dollar-brace-l
  (jove-DOLLAR-BRACE-L (list jove-B-TMPL jove-B-STAT)))

;;; Update Context jove-BACKQUOTE

(jove-deftest-ctx-update backquote
  (jove-BACKQUOTE nil))

(jove-deftest-ctx-stack backquote-start
  (jove-BACKQUOTE (list jove-Q-TMPL jove-B-STAT)))
(jove-deftest-ctx-stack backquote-end
  (jove-BACKQUOTE (list jove-B-STAT) :push (jove-Q-TMPL)))

;;; Update Context jove-INC-DEC
;; Note: `jove--expr-allowed' should be left alone

(jove-deftest-ctx-update inc-dec-previous-nil
  (jove-INC-DEC nil)
  ;; `state' is defined inside the macro
  (jove--set-expr-allowed state nil))

(jove-deftest-ctx-update inc-dec-previous-t
  (jove-INC-DEC t)
  ;; `state' is defined inside the macro
  (jove--set-expr-allowed state t))

;;; Update Context jove-FUNCTION
;;  Note: `jove--expr-allowed' should always be switched to nil

(jove-deftest-ctx-update function
  (jove-FUNCTION nil))

(jove-deftest-ctx-stack function-prev-tt-not-before-expr
  (jove-FUNCTION (list jove-B-STAT))
  (jove--set-expr-allowed state nil))

(jove-deftest-ctx-stack function-prev-tt-before-expr-default
  ;; NOTE: jove-B-STAT also has to be jove--current-ctx
  ;; NOTE: Using throw away time just default before-expr t behavior
  (jove-FUNCTION (list jove-F-EXPR jove-B-STAT) :prev (jove--tt-create "test" :before-expr t)))

(jove-deftest-ctx-stack function-prev-tt-before-expr-semi
  (jove-FUNCTION (list jove-B-STAT) :prev jove-SEMI)
  (jove--set-expr-allowed state t))

(jove-deftest-ctx-stack function-prev-tt-before-expr-else
  (jove-FUNCTION (list jove-B-STAT) :prev jove-ELSE)
  (jove--set-expr-allowed state t))

(jove-deftest-ctx-stack function-prev-tt-before-expr-colon
  (jove-FUNCTION (list jove-B-STAT) :prev jove-COLON)
  (jove--set-expr-allowed state t))

(jove-deftest-ctx-stack function-prev-tt-before-expr-brace-l
  (jove-FUNCTION (list jove-B-STAT) :prev jove-BRACE-L)
  (jove--set-expr-allowed state t))

;; For the remaining Token Types, the presence or absence of the
;; alist label 'before-expr is used to update the `parse-js--ctx-stack'.
(mapc #'(lambda (tt)
          (eval `(jove-deftest-ctx-before-expr ,tt))) ; HACK!
      '(;; BRACKET-L                       ; Has own update-ctx function.
        ;; BRACE-L                         ; Has own update-ctx function.
        ;; PAREN-L                         ; Has own update-ctx function.
        COMMA
        SEMI
        COLON
        QUESTION
        ARROW
        ELLIPSIS
        ;; DOLLOR-BRACE-L                  ; Has own update-ctx function.
        EQ
        ASSIGN
        PREFIX
        LOGICAL-OR
        LOGICAL-AND
        BITWISE-OR
        BITWISE-XOR
        BITWISE-AND
        EQUALITY
        RELATIONAL
        BITSHIFT
        PLUS-MIN
        MODULO
        STAR
        STARSTAR
        SLASH
        CASE
        DO
        ELSE
        RETURN
        THROW
        NEW
        EXTENDS
        IN
        INSTANCEOF
        TYPEOF
        VOID
        DELETE))

(mapc #'(lambda (tt)
          (eval
           `(jove-deftest-ctx-not-before-expr ,tt))) ; HACK!
      '(NUM
        REGEXP
        STRING
        NAME
        EOF
        ;; BRACKET-R                       ; Has own update-ctx function.
        DOT
        TEMPLATE
        ;; BACKQUOTE                       ; Has own update-ctx function.
        ;; INC-DEC                         ; Has own update-ctx function.
        BREAK
        CATCH
        DEFAULT
        CONTINUE
        DEBUGGER
        FINALLY
        FOR
        FUNCTION
        IF
        SWITCH
        TRY
        VAR
        CONST
        WHILE
        WITH
        THIS
        SUPER
        CLASS
        EXPORT
        IMPORT
        NULL
        TRUE
        FALSE))
