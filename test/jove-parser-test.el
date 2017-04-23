;;;; jove-parser-test.el --- Tests for the Jove Lexer Context -*- lexical-binding: t; -*-

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

;; (load (expand-file-name "jove-test-util.el"))

;; FIXME: Find the best way to add this to all the tests.
(defmacro jove-deftest (name bindlist &optional docstring &rest body)
  "Define an `ert-test' with NAME.
\(fn NAME BINDLIST &optional DOCSTRING &rest BODY)"
  (declare (doc-string 3) (indent 2))
  (let (docstring-supplied-p)
    (when (stringp docstring)
      (setq docstring-supplied-p t))
    `(ert-deftest ,(intern (format "jove-test-%s" name)) ()
       ,docstring
       (with-temp-buffer
         (jove-mode)
         (let (,@bindlist)
           ,@body)))))

(jove-deftest parser-state-token ()
  "Test `jove-token' returns the current token."
  (insert "  let foo = bar(qux")
  (jove-config)
  (jove-next)
  (should (equal (jove-start (jove-token))
                 3))
  (should (equal (jove-end (jove-token))
                 6))
  (should (equal (jove-value (jove-token))
                 "let")))

(jove-deftest parser-node-init ()
  "Test `jove-node-init' for correct return value."
  (insert "  let foo = bar(qux")
  (jove-config)
  (jove-next)
  (should (equal (jove-node-init)
                 '(([3 nil nil nil])))))

(jove-deftest parser-node-finish ()
  "Test `jove-node-first' for correct return value."
  (insert "  let foo = bar(qux")
  (jove-config)
  (jove-next)
  (should (equal (jove-node-finish (prog1 (jove-node-init)
                                 (jove-next))
                               'identifier)
                 '(([3 6 identifier nil])))))

(jove-deftest parser-node-finish-at ()
  "Test `jove-node-first' for correct return value."
  (insert "  let foo = bar(qux")
  (jove-config)
  (jove-next)
  (should (equal (jove-node-finish-at (jove-node-init)
                                  'identifier
                                  6)
                 '(([3 6 identifier nil])))))

(jove-deftest parser-add-child ((parent (jove-node-init 1 3 'foo))
                            (child (jove-node-init 4 7 'bar)))
  "Test `jove-add-child' correctly adds child to parent."
  (should (equal (jove-first-child (jove-add-child parent child))
                 child)))

(jove-deftest parser-add-children ((parent (jove-node-init 1 3 'foo))
                               (child-one (jove-node-init 4 7 'bar))
                               (child-two (jove-node-init 8 11 'qux)))
  "Test `jove-add-children' correctly adds children to parent."
  (jove-add-children parent child-one child-two)
  (should (equal (jove-first-child parent)
                 child-one))
  (should (equal (jove-next-sibling (jove-first-child parent))
                 child-two)))

(jove-deftest parser-add-children* ((parent (jove-node-init 1 3 'foo))
                                (child-one (jove-node-init 4 7 'bar))
                                (child-two (jove-node-init 8 11 'qux)))
  "Test `jove-add-children' correctly adds children to parent."
  (jove-add-children* parent (list child-one child-two))
  (should (equal (jove-first-child parent)
                 child-one))
  (should (equal (jove-next-sibling (jove-first-child parent))
                 child-two)))

(jove-deftest parser-clear-children ((parent (jove-node-init 1 3 'foo))
                                 (child-one (jove-node-init 4 7 'bar))
                                 (child-two (jove-node-init 8 11 'qux)))
  "Test `jove-add-children' correctly adds children to parent."
  (jove-add-children* parent (list child-one child-two))
  (jove-clear-children parent)
  (should (equal (jove-first-child parent)
                 nil))
  (should (equal (jove-next-sibling (jove-first-child parent))
                 nil)))
