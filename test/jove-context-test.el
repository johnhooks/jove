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
(require 'jove-mode)

(defun jove-test-env (body)
  `(with-temp-buffer
     (jove-mode)
     ,@body))

(defmacro jove-deftest (name &rest body)
  (declare (indent defun))
  `(ert-deftest ,(intern (format "jove-test-ctx-%s" name)) ()
     ,(jove-test-env body)))

(cl-defmacro jove-deftest-ctx-update (name (tt expect &key push prev) &rest body)
  (declare (indent defun))
  `(jove-deftest ,(format "update-%s" name) ; `jove-deftest' will add on prefix
     (let ((token (jove-token-make)))
       ,@(mapcar #'(lambda (ctx)
                     `(jove-ctx-stack-push token ,ctx))
                 push)
       (jove-set-tt token ,tt)
       ,@body
       (jove-update-ctx token ,(or prev jove-EOF))
       (should (eq (jove-expr-allowed token) ,expect)))))

(cl-defmacro jove-deftest-ctx-stack (name (tt expect &key push prev) &rest body)
  (declare (indent defun))
  `(jove-deftest ,(format "stack-%s" name) ; `jove-deftest' will add on prefix
     (let ((token (jove-token-make)))
       ,@(mapcar #'(lambda (ctx)
                     `(jove-ctx-stack-push token ,ctx))
                 push)
       (jove-set-tt token ,tt)
       ,@body
       (jove-update-ctx token ,(or prev jove-EOF))
       (should (equal (jove-ctx-stack token) ,expect)))))

(cl-defmacro jove-deftest-ctx-before-expr (tt)
  `(jove-deftest ,(intern (format "before-expr-%s" (symbol-name tt)))
     (let ((token (jove-token-make)))
       (jove-set-tt token ,(intern (format "jove-%s" (symbol-name tt))))
       (jove-update-ctx token jove-EOF)
       (should (eq t (jove-expr-allowed token))))))

(cl-defmacro jove-deftest-ctx-not-before-expr (tt)
  `(jove-deftest ,(intern (format "not-before-expr-%s" (symbol-name tt)))
     (let ((token (jove-token-make)))
       (jove-set-tt token ,(intern (format "jove-%s" (symbol-name tt))))
       (jove-update-ctx token jove-EOF)
       (should (eq nil (jove-expr-allowed token))))))

(jove-deftest brace-is-block-colon-b-stat
  (let ((token (jove-token-make)))
    (jove-ctx-stack-push token jove-B-STAT)
    (should (eq t (jove-brace-is-block-p token jove-COLON)))))

(jove-deftest brace-is-block-colon-b-expr
  (let ((token (jove-token-make)))
    (jove-ctx-stack-push token jove-B-EXPR)
    (should (eq nil (jove-brace-is-block-p token jove-COLON)))))

(jove-deftest brace-is-block-return
  (let ((token (jove-token-make)))
    (insert "return {}")
    (goto-char 1)
    (setq token (jove-next-token token))
    (jove-skip-space token)
    (should (eq nil (jove-brace-is-block-p token jove-RETURN)))))

(jove-deftest brace-is-block-return-newline
  (let ((token (jove-token-make)))
    (insert "return \n {}")
    (goto-char 1)
    (setq token (jove-next-token token))
    (jove-skip-space token)
    (should (eq t (jove-brace-is-block-p token jove-RETURN)))))

(jove-deftest brace-is-block-else
  (should (eq t (jove-brace-is-block-p (jove-token-make) jove-ELSE))))

(jove-deftest brace-is-block-semi
  (should (eq t (jove-brace-is-block-p (jove-token-make) jove-SEMI))))

(jove-deftest brace-is-block-eof
  (should (eq t (jove-brace-is-block-p (jove-token-make) jove-EOF))))

(jove-deftest brace-is-block-paren-r
  (should (eq t (jove-brace-is-block-p (jove-token-make) jove-PAREN-R))))

(jove-deftest brace-is-block-brace-l-b-stat
  (let ((token (jove-token-make)))
    (jove-ctx-stack-push token jove-B-STAT)
    (should (eq t (jove-brace-is-block-p token jove-BRACE-L)))))

(jove-deftest brace-is-block-expr-allowed
  ;; The argument to `jove-brace-is-block-p' is just a place holder.
  (let ((token (jove-token-make)))
    (jove-set-expr-allowed token t)
    (should (eq nil (jove-brace-is-block-p token jove-ASSIGN)))))

(jove-deftest brace-is-block-expr-not-allowed
  ;; The argument to `jove-brace-is-block-p' is just a place holder.
  (let ((token (jove-token-make)))
    (jove-set-expr-allowed token nil)
    (should (eq t (jove-brace-is-block-p token jove-BRACE-R)))))

;;; Update Context Functions

;; Update Context jove-BRACE-L

(jove-deftest-ctx-update brace-l
  ;; Whether or not brace is a block, expr-allowed should be t.
  (jove-BRACE-L t))

(jove-deftest-ctx-stack brace-l-b-stat
  ;; When `jove-brace-is-block-p' returns t, `jove--update-ctx' pushs `jove-B-STAT'
  ;; on to the top of `jove--ctx-stack'.
  (jove-BRACE-L (list jove-B-STAT jove-B-STAT))
  (jove-set-expr-allowed token nil))     ; `jove--brace-is-block-p' returns t

(jove-deftest-ctx-stack brace-l-b-expr
  ;; When `jove-brace-is-block-p' returns nil, `jove-update-ctx' pushs `jove-B-EXPR'
  ;; on to the top of `jove--ctx-stack'.
  ;; HACK: `jove-COMMA' passes through `jove-brace-is-block-p', `jove-EOF' does not.
  (jove-BRACE-L (list jove-B-EXPR jove-B-STAT) :prev jove-COMMA)
  (jove-set-expr-allowed token t))

;;; Update Context jove-BRACE-R
;; Note: jove-PAREN-R is updated the same as jove-BRACE-R.

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

;; Anything else should push jove-P-EXPR.
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
  ;; NOTE: `token' is defined inside the macro
  (jove-set-expr-allowed token nil))

(jove-deftest-ctx-update inc-dec-previous-t
  (jove-INC-DEC t)
  ;; NOTE: `token' is defined inside the macro
  (jove-set-expr-allowed token t))

;;; Update Context jove-FUNCTION
;;  Note: `jove--expr-allowed' should always be switched to nil.

(jove-deftest-ctx-update function
  (jove-FUNCTION nil))

(jove-deftest-ctx-stack function-prev-tt-not-before-expr
  (jove-FUNCTION (list jove-B-STAT))
  (jove-set-expr-allowed token nil))

(jove-deftest-ctx-stack function-prev-tt-before-expr-default
  ;; NOTE: jove-B-STAT must also be the current context.
  ;; NOTE: Using a dummy tt for testing.
  (jove-FUNCTION (list jove-F-EXPR jove-B-STAT) :prev (jove-tt-make "dummy" :before-expr t)))

(jove-deftest-ctx-stack function-prev-tt-before-expr-semi
  (jove-FUNCTION (list jove-B-STAT) :prev jove-SEMI)
  (jove-set-expr-allowed token t))

(jove-deftest-ctx-stack function-prev-tt-before-expr-else
  (jove-FUNCTION (list jove-B-STAT) :prev jove-ELSE)
  (jove-set-expr-allowed token t))

(jove-deftest-ctx-stack function-prev-tt-before-expr-colon
  (jove-FUNCTION (list jove-B-STAT) :prev jove-COLON)
  (jove-set-expr-allowed token t))

(jove-deftest-ctx-stack function-prev-tt-before-expr-brace-l
  (jove-FUNCTION (list jove-B-STAT) :prev jove-BRACE-L)
  (jove-set-expr-allowed token t))

;; For the remaining token types, the 'before-expr' slot is used to
;; update the lexer state 'ctx-stack' slot.
(mapc #'(lambda (tt)
          (eval `(jove-deftest-ctx-before-expr ,tt))) ; HACK!
      '(;; BRACKET-L                       Handled by `jove-update-ctx'
        ;; BRACE-L                         Handled by `jove-update-ctx'
        ;; PAREN-L                         Handled by `jove-update-ctx'
        COMMA
        SEMI
        COLON
        QUESTION
        ARROW
        ELLIPSIS
        ;; DOLLOR-BRACE-L                  Handled by `jove-update-ctx'
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
        ;; BRACKET-R                       Handled by `jove-update-ctx'
        DOT
        TEMPLATE
        ;; BACKQUOTE                       Handled by `jove-update-ctx'
        ;; INC-DEC                         Handled by `jove-update-ctx'
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
