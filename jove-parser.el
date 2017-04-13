;;;; jove-parser.el --- The Jove Parser -*- lexical-binding: t; -*-

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

;;; Think the parser will needs its own state like the lexer.

;;; Node

;; Node Siblings (cdr node)

;; Node Children (cdr (car node))

;; Node Data (car (car node))
;; 0 start
;; 1 end
;; 2 type

(defsubst jove--node-start (node)
  (aref (car (car node)) 0))
(defsubst jove--node->start (node value)
  (aset (car (car node)) 0 value))
(defsubst jove--node-end (node)
  (aref (car (car node)) 1))
(defsubst jove--node->end (node value)
  (aset (car (car node)) 1 value))
(defsubst jove--node-type (node)
  (aref (car (car node)) 2))
(defsubst jove--node->type (node value)
  (aset (car (car node)) 2 value))

(defun jove--node-create (&optional start end type)
  "Return a list representing a node containing DATA."
  (cons (cons (vector start end type) nil) nil))

(defun jove--node-p (object)
  "Return non-nil if OBJECT could represent a node."
  (and (listp node)
       (listp (car node))
       (vectorp (car (car node)))
       (= 3 (length (car (car node))))))

(defun jove--node-data (node)
  "Return the data contained in NODE."
  (car (car node)))

(defun jove--node->child (node child)
  "Add CHILD to NODE."
  (nconc (car node) child)
  node)

(defun jove--node->children (node &rest children)
  "Add CHILDREN to NODE."
  (apply #'nconc (cons (car node) children))
  node)

(defun jove--node-first-child (node)
  "Return a reference to the first child of NODE."
  (cdr (car node)))

(defun jove--node-next-sibling (node)
  "Return a reference to the next sibling of NODE."
  (cdr node))

(defun jove--node-start (token)
  "Return a node begun at TOKEN's start position."
  (jove--node-create (jove--token-start token)))

;; Don't think this is necessary since `jove--node-create' can create
;; a blank node or insert any data provided.
(defun jove--node-start-at (pos)
  "Start node at POS."
  (jove--node-create (cons pos nil)))

;;; Code below has not been reviewed

(defun jove--node-finish (node type &optional info)
  "Finish NODE of TYPE at the `jove--prev-end' position."
  ;; The parser will have already advanced to the next token,
  ;; which is why `jove--prev-end' is used.
  (nconc (car (car node))
         (cons jove--prev-end (cons type (when info (cons info nil)))))
  node)                                 ; Return node.

(defun jove--node-finish-at (node type pos)
  "Finish NODE of TYPE at POS."
  (jove--node->type node type)
  (jove--node->end node pos)
  node)

(defun jove--node-make (type &rest children)
  "Create a node of TYPE from current token and advance."
  (jove--node-finish (prog1 (jove--node-start) (jove--next)) type))

(defun jove--node-finish-with (node type &rest children)
  "Finish NODE of TYPE with CHILDREN."
  (jove--node-finish (jove--add-children node children) type))

(provide 'jove-parser)

;;; jove-parser .el ends here
