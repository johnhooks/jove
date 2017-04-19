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

(require 'jove-lexer)

;;; Buffer Local Variables

(defvar-local jove--index 0)
(defvar-local jove--length 0)
(defvar-local jove--token nil)
(defvar-local jove--prev-token nil)
(defvar-local jove--in-function nil)
(defvar-local jove--in-async nil)
(defvar-local jove--in-generator nil)
(defvar-local jove--potential-arrow-at -1)

;;; Initialization

(defun jove--parser-config ()
  "Initialize parser global variables."
  (setq jove--index 0
        jove--length (length jove--cache)
        ;; NOTE: If buffer is empty `jove--token' will be nil
        jove--token (when (> jove--length 0) (aref jove--cache 0))
        jove--prev-token (seq-take (jove--lex-create) 5)  ; HACK
        jove--in-function nil
        jove--in-async nil
        jove--in-generator nil))

(defun jove-next ()
  "Advance parser to next token."
  ;; At the moment, if lexer for some reason fails to push a EOF token
  ;; into the cache, this function will remain on the last token.
  (let ((next (1+ jove--index)))
    ;; Attempting to protect from referencing beyond the end of the vector.
    (when (< next jove--length)
      (setq jove--index next
            jove--prev-token jove--token
            jove--token (aref jove--cache jove--index)))))

;;; Nodes

;; Siblings (cdr node)
;; Children (cdr (car node))
;; Data (car (car node))

(defun jove--node-create (&optional start end type info)
  "Return a node.
If START is not supplied, its value will be set to start of the
current token.  Optionally supply any or all START, END, TYPE or
INFO arguments."
  (cons (cons (vector (or start         ; 0
                          (jove--start jove--token))
                      end               ; 1
                      type              ; 2
                      info)             ; 3
              nil)
        nil))

;; Node Data Getter and Setter Functions

(defsubst jove--node-start (node)
  "Return the 'start' slot of the NODE."
  (aref (car (car node)) 0))
(defsubst jove--set-node-start (node value)
  "Set the 'start' slot of the NODE to VALUE."
  (aset (car (car node)) 0 value))

(defsubst jove--node-end (node)
  "Return the 'end' slot of the NODE."
  (aref (car (car node)) 1))
(defsubst jove--set-node-end (node value)
  "Set the 'end' slot of the NODE to VALUE."
  (aset (car (car node)) 1 value))

(defsubst jove--node-type (node)
  "Return the 'type' slot of the NODE."
  (aref (car (car node)) 2))
(defsubst jove--set-node-type (node value)
  "Set the 'type' slot of the NODE to VALUE."
  (aset (car (car node)) 2 value))

(defsubst jove--node-info (node)
  "Return the 'info' slot of the NODE."
  (aref (car (car node)) 3))
(defsubst jove--set-node-info (node value)
  "Set the 'info' slot of the NODE to VALUE."
  (aset (car (car node)) 3 value))

(defun jove--node-p (object)
  "Return non-nil if OBJECT could represent a node."
  (and (listp object)
       (listp (car object))
       (vectorp (car (car object)))
       (= 3 (length (car (car object))))))

(defun jove--node-data (node)
  "Return the data contained in NODE."
  (car (car node)))

;; Node Building Functions

(defun jove--node-add-child (node child)
  "Add to NODE children the node CHILD.
Return NODE."
  (nconc (car node) child)
  node)

(defun jove--node-add-children (node &rest children)
  "Add to NODE children the collected node arguments CHILDREN.
Return NODE."
  (apply #'nconc (cons (car node) children))
  node)

(defun jove--node-add-children* (node children)
  "Add to NODE children the list of nodes CHILDREN.
Return NODE."
  (apply #'nconc (cons (car node) children))
  node)

(defun jove--node-clear-children (node)
  "Remove children from NODE.
Return NODE."
  (setf (car node) (cons (car (car node)) nil))
  node)

(defun jove--node-first-child (node)
  "Return a reference to the first child of NODE."
  (cdr (car node)))

(defun jove--node-next-sibling (node)
  "Return a reference to the next sibling of NODE."
  (cdr node))

(defun jove--node-finish (node type &optional info)
  "Finish NODE of TYPE at the `jove--prev-end' position.
Optionally add INFO into the info slot."
  ;; The parser will have already advanced to the next token,
  ;; which is why the `jove--end' of `jove--prev-token' is used.
  (jove--set-node-type node type)
  (jove--set-node-end node (jove--end jove--prev-token))
  (when info
    (jove--set-node-info node info))
  node)                                 ; Return node.

(defun jove--node-finish-at (node type pos &optional info)
  "Finish NODE of TYPE at POS.
Optionally add INFO into the info slot."
  (jove--set-node-type node type)
  (jove--set-node-end node pos)
  (when info
    (jove--set-node-info node info))
  node)

;;; Utility Functions

(defun jove--unexpected ()
  "Signal an unexpected token parse error."
  (signal 'jove-parse-error
          `((start ,(jove--start jove--token))
            (end ,(jove--end jove--token))
            (message ,(format "Unexpected token: %s"
                              (jove--tt-label (jove--type jove--token)))))))

(defun jove--eat (type)
  "Test whether the next token is of TYPE, if so consume it as a side effect.
Returns t if a token was consumed, otherwise nil."
  (when (eq type (jove--type jove--token))
    (jove-next)
    t))

(defun jove--expect (type)
  "Test whether current token is of TYPE, consumed it, otherwise error."
  (if (eq type (jove--type jove--token))
      (jove-next)
    (jove--unexpected)))

(defun jove--after-trailing-comma (type &optional not-next)
  "Return non-nil if the current token is of TYPE.
Advance to next token unless NOT-NEXT."
  ;; NOTE: Found in acorn/src/parseutil.js
  (when (eq type (jove--type jove--token))
    (unless not-next
      (jove-next))
    t))

(defun jove--is-contextual (name)
  "Test whether current token is a contextual keyword NAME."
  (and (eq jove-NAME (jove--type jove--token))
       (string-equal name (jove--value jove--token))))

(defun jove--eat-contextual (name)
  "Consume contextual keyword NAME is possible."
  (and (jove--is-contextual name)
       (jove--eat jove-NAME)))

(defun jove--can-insert-semicolon-p ()
  "Test whether or not a semi-colon can be inserted."
  (or (eq jove-EOF (jove--type jove--token))
      (eq jove-BRACE-R (jove--type jove--token))
      (jove--newline-before jove--token)))

(defun jove--semicolon ()
  "Consume a semicolon or pretend there is a semicolon."
  (unless (or (jove--eat jove-SEMI)
              (jove--can-insert-semicolon-p))
    (jove--unexpected)))

;;; Expression Parsing Functions

(defun jove--parse-expression (&optional no-in)
  "Parse a full expression.
The boolean flag NO-IN forbids the 'in' operator."
  (let ((start-pos (jove--start jove--token))
        (expr (jove--parse-maybe-assign no-in)))
    (if (eq jove-COMMA (jove--type jove--token))
        (let ((node (jove--node-create start-pos)))
          (jove--node-add-child node expr)
          (while (jove--eat jove-COMMA)
            (jove--node-add-child node (jove--parse-maybe-assign no-in)))
          (jove--node-finish node 'sequence-expression))
      expr)))

(defun jove--parse-maybe-assign (&optional no-in)
  "Maybe parse an assignment expression.
The boolean flag NO-IN forbids the 'in' operator."
  (if (and jove--in-generator (jove--is-contextual "yield"))
      (jove--parse-yield)
    (when (or (eq jove-PAREN-L (jove--type jove--token))
              (eq jove-NAME (jove--type jove--token)))
      (setq jove--potential-arrow-at (jove--start jove--token)))
    (let ((start-pos (jove--start jove--token))
          (left (jove--parse-maybe-conditional no-in)))
      ;; Find where afterLeftParse is used.
      (if (jove--tt-is-assign (jove--type jove--token))
          (let ((node (jove--node-create start-pos nil nil (jove--value jove--token))))
            (jove--node-add-child node (if (eq jove-EQ (jove--type jove--token))
                                       (jove--to-assignable left)
                                     left))
            (jove-next)                     ; Move over the operator.
            (jove--node-add-child node (jove--parse-maybe-conditional no-in))
            (jove--node-finish node 'assign-expression))
        left))))

(defun jove--parse-maybe-conditional (&optional no-in)
  "Maybe parse a ternary conditional expression.
The boolean flag NO-IN forbids the 'in' operator."
  (let ((start-pos (jove--start jove--token))
        (expr (jove--parse-expr-ops no-in)))
    (if (eq jove-QUESTION (jove--type jove--token))
        (let ((node (jove--node-create start-pos nil nil (jove--start jove--token))))
          ;; Putting question mark position into ConditionalExpression node info slot.
          ;; Getting weird I know. But should be helpful for indentation.
          (jove-next)                       ; Move over '?'
          (jove--node-add-children node
                               expr
                               (jove--parse-maybe-assign) ; no NO-IN here???
                               (progn
                                 (jove--expect jove-COLON)
                                 (jove--parse-maybe-assign no-in)))
          (jove--node-finish node 'conditional-expression))
      expr)))

(defun jove--parse-expr-ops (no-in)
  "Start the precedence parser.
If current expression is an ArrowFunctionExpression, just return
the expression.  The boolean flag NO-IN forbids the 'in' operator."
  (let ((start-pos (jove--start jove--token))
        (expr (jove--parse-maybe-unary nil)))
    (if (and (= start-pos (jove--node-start expr))
             (eq 'arrow-function-expression (jove--node-type expr)))
        expr
      (jove--parse-expr-op expr start-pos -1 no-in))))

(defun jove--parse-expr-op (left left-start-pos min-prec no-in)
  "Parse binary operators with the op precedence parsing algorithm.
LEFT is the left-hand side of the operation.  Subsequent nodes wrap
the previous and are initialize at LEFT-START-POS.  MIN-PREC
provides context that allows the function to stop and defer
further parser to one of its callers when it encounters an
operator that has a lower precedence than the set it is parsing.
The boolean flag NO-IN forbids the 'in' operator."
  (let ((start-pos nil)
        (prec (and (or (not no-in)
                       (not (eq jove-IN (jove--type jove--token))))
                   (jove--tt-binop (jove--type jove--token)))))
    (if (and prec (> prec min-prec))
        (let ((logical (or (eq jove-LOGICAL-OR (jove--type jove--token))
                           (eq jove-LOGICAL-AND (jove--type jove--token))))
              (op (prog1 (jove--value jove--token)
                    (jove-next)
                    (setq start-pos (jove--start jove--token))))
              (right (jove--parse-expr-op (jove--parse-maybe-unary nil) start-pos prec no-in)))
          (jove--parse-expr-op (jove--build-binary left-start-pos
                                           left
                                           right
                                           op
                                           logical)
                           left-start-pos
                           min-prec
                           no-in))
      left)))

(defun jove--build-binary (start-pos left right op logical)
  "Create a node for either a logical or binary expression.
Initialize the node at START-POS.  Add LEFT, RIGHT and OP as children.
The boolean flag LOGICAL toogles a logical or binary expression"
  ;; Putting operator string into info slot
  (let ((node (jove--node-create start-pos)))
    (jove--set-node-info node op)
    (jove--node-add-children node left right)
    (jove--node-finish node (if logical 'logical-expression 'binary-expression))))

(defun jove--parse-maybe-unary (saw-unary)
  "Parse both prefix and postfix unary operators.
Unless SAW-UNARY, build a binary expression when '**' is encountered."
  (let ((expr nil)
        (start-pos (jove--start jove--token)))
    (cond
     ((and jove--in-async
           (jove--is-contextual "await"))
      (setq expr (jove--parse-await)))
     ((jove--tt-prefix (jove--type jove--token))
      (let ((node (jove--node-create (jove--start jove--token) nil nil 'prefix))
            (update (eq jove-INC-DEC (jove--type jove--token))))
        (jove--set-node-info node (jove--value jove--token))
        (jove-next)
        (jove--node-add-child node (jove--parse-maybe-unary t))
        (setq expr (jove--node-finish node (if update
                                           'update-expression
                                         'unary-expression)))
        (unless update (setq saw-unary t))))
     (t
      (setq expr (jove--parse-expr-subscripts))
      (while (and (jove--tt-postfix (jove--type jove--token))
                  (not (jove--can-insert-semicolon-p)))
        (let ((node (jove--node-create start-pos nil nil 'postfix)))
          (jove--node-add-child node expr)
          (jove--set-node-info (jove--value jove--token))
          (jove-next)
          (setq expr (jove--node-finish node 'update-expression))))))
    (if (and (not saw-unary)
             (eq jove-STARSTAR (jove--type jove--token)))
        (jove--build-binary start-pos
                        expr
                        (jove--parse-maybe-unary nil)
                        (jove--value jove--token)
                        nil)
      expr)))

(defun jove--parse-expr-subscripts ()
  "Parse call, dot, and bracket notation subscript expressions."
  (let ((start-pos (jove--start jove--token))
        (expr (jove--parse-expr-atom)))
    (jove--parse-subscripts expr start-pos)))

(defun jove--parse-subscripts (base start-pos &optional no-calls)
  "Possibly parse the subscripts of BASE.
All subsequent nodes wrap BASE and initialize at START-POS.
Optionally if NO-CALLS disallow the parsing of call expressions."
  (let ((computed nil)
        (maybe-async-arrow (and (eq 'identifier (jove--node-type base))
                                (string-equal "async" (jove--node-info base))
                                (= (jove--end jove--prev-token) (jove--node-end base))
                                (not (jove--can-insert-semicolon-p)))))
    (catch 'node
      (while t
        (cond
         ;; Member Expression
         ((or (setq computed (jove--eat jove-BRACKET-L))
              (jove--eat jove-DOT))
          (let ((node (jove--node-create start-pos nil nil (when computed 'computed))))
            (jove--node-add-children node
                                 base
                                 (if computed
                                     (jove--parse-expression)
                                   (jove--parse-identifier t)))
            (when computed (jove--expect jove-BRACKET-R))
            (setq base (jove--node-finish node 'member-expression))))
         ;; Call Expression
         ((and (not no-calls)
               (eq jove-PAREN-L (jove--type jove--token)))
          (let ((exprs (jove--node-create)))
            (jove-next)                     ; Move over '('
            (jove--node-add-children* exprs (nreverse (jove--parse-expr-list jove-PAREN-R)))
            (jove--set-node-end exprs (jove--end jove--prev-token))
            ;; 'exprs' is not finished like most nodes.  It is almost
            ;; complete, only the type slot remains to be set.
            (if (and maybe-async-arrow
                     (not (jove--can-insert-semicolon-p))
                     (jove--eat jove-ARROW))
                (throw 'node (progn
                               (jove--set-node-type exprs 'parameters)
                               (jove--parse-arrow-expr (jove--node-create start-pos)
                                                   exprs
                                                   t)))
              (let ((node (jove--node-create start-pos)))
                (jove--set-node-type exprs 'arguments)
                (jove--node-add-children node base exprs)
                (jove--node-finish node 'call-expression)
                (setq base node)))))
         ;; Tagged Template Expression
         ((eq jove-BACKQUOTE (jove--type jove--token))
          (jove--node-finish (jove--node-add-children (jove--node-create start-pos)
                                              base
                                              (jove--parse-template))
                         'tagged-template-expression))
         (t
          (throw 'node base)))))))

(defun jove--parse-expr-atom ()
  "Parse an atomic expression."
  (let ((type (jove--type jove--token))
        (can-be-arrow (= (jove--start jove--token) jove--potential-arrow-at)))
    (cond
     ((eq jove-SUPER type)
      (when (not jove--in-function)
        (jove--warn (jove--start jove--token) (jove--end jove--token) "'super' outside of function or class"))
      (prog1 (jove--node-create (jove--start jove--token)
                            (jove--end jove--token)
                            'super)
        (jove-next)))
     ((eq jove-THIS type)
      (prog1 (jove--node-create (jove--start jove--token)
                            (jove--end jove--token)
                            'this-expression)
        (jove-next)))
     ((eq jove-NAME type)
      (let ((start-pos (jove--start jove--token))
            (id (jove--parse-identifier nil)))
        (cond
         ((and (string-equal "async" (jove--node-info id))
               (not (jove--can-insert-semicolon-p))
               (jove--eat jove-FUNCTION))
          ;; Parse async function
          (jove--parse-function (jove--node-create start-pos) nil nil t))
         ((and can-be-arrow
               (not (jove--can-insert-semicolon-p)))
          (cond
           ((jove--eat jove-ARROW)
            (let ((params (jove--node-create (jove--node-start id)
                                         (jove--node-end id)
                                         'parameters)))
              ;; Wrap the single parameter in a 'parameters' node.
              (jove--node-add-child params id)
              (jove--parse-arrow-expr (jove--node-create start-pos) params nil)))
           ((and (string-equal "async" (jove--node-info id))
                 (eq jove-NAME (jove--type jove--token)))
            (setq id (jove--parse-identifier))
            (if (or (jove--can-insert-semicolon-p)
                    (not (jove--eat jove-ARROW)))
                (jove--unexpected)
              (let ((params (jove--node-create (jove--node-start id)
                                           (jove--node-end id)
                                           'parameters)))
                ;; Wrap the single parameter in a 'parameters' node.
                (jove--node-add-child params id)
                (jove--parse-arrow-expr (jove--node-create start-pos params t)))))
           (t           ; Still need to return ID if none of the above match.
            id)))
         (t
          id))))
     ((memq type (list jove-REGEXP jove-STRING jove-NUM jove-NULL jove-UNDEFINED jove-TRUE jove-FALSE))
      (prog1 (jove--node-create (jove--start jove--token)
                            (jove--end jove--token)
                            'literal)
        (jove-next)))
     ((eq jove-PAREN-L type)
      (jove--parse-paren-expr can-be-arrow))
     ((eq jove-BRACKET-L type)
      (let ((node (jove--node-create)))
        (jove-next)
        (jove--node-add-children* node (jove--parse-expr-list jove-BRACKET-R))
        (jove--node-finish node 'array-expression)))
     ((eq jove-BRACE-L type)
      (jove--parse-object nil))
     ((eq jove-FUNCTION type)
      (let ((node (jove--node-create)))
        (jove-next)
        (jove--parse-function node nil)))
     ((eq jove-CLASS type)
      (jove--parse-class (jove--node-create) nil))
     ((eq jove-NEW type)
      (jove--parse-new))
     ((eq jove-BACKQUOTE type)
      (jove--parse-template))
     (t
      (jove--unexpected)))))

;; Look at the difference between the expression collection of `jove--parse-expr-list'
;; and see if it can be used here. Since I want to allow trailing commas and empty
;; expressions (because that is the best course while editting), I can probably
;; used that function to collect the list. Then figure out what to do with it after.

;; Also to note, when changing an expression list to an assignable, I can change the
;; node names though I will not worry if there are invalid expressions in it. Just
;; parse destructured assignments as liberally as possible.

(defun jove--parse-paren-expr (&optional can-be-arrow)
  "Parse a parenthesized expression.
As of ECMAScript 6, possiblities are SequenceExpression or
ArrowExpression.  ArrowExpressions are allowed when the boolean
flag CAN-BE-ARROW is non-nil.  A sequence is wrapped in
ParenthesizedExpression.  Trailing commas and empty expressions
are allowed."
  (let ((val nil)
        (first t)
        (expr-list '())
        (inner-end-pos nil)
        (start-pos (jove--start jove--token))
        (inner-start-pos (progn (jove-next) (jove--start jove--token))))
    ;; FIXME: Could `parse-expr-list' be used here?
    (while (not (eq jove-PAREN-R (jove--type jove--token)))
      (if first
          (setq first nil)
        (jove--expect jove-COMMA))
      ;; Allow empty expressions and trailing commas.
      (unless (or (jove--after-trailing-comma jove-PAREN-R t)
                  (eq jove-COMMA (jove--type jove--token)))
        (if (eq jove-ELLIPSIS (jove--type jove--token))
            (push (jove--parse-rest) expr-list)
          (unless (jove--after-trailing-comma jove-PAREN-R t)
            (push (jove--parse-maybe-assign nil) expr-list)))))
    (setq inner-end-pos (jove--start jove--token))
    (jove--expect jove-PAREN-R)

    ;; Return a ArrowFunctionExpression or ParenthesizedExpression
    (if (and can-be-arrow
             (not (jove--can-insert-semicolon-p))
             (eq jove-ARROW (jove--type jove--token)))
        (let ((params (jove--node-create start-pos)))
          (jove--node-add-children* params (nreverse expr-list))
          (jove--node-finish params 'parameters)
          (jove-next)                         ; Move over '=>'
          (jove--parse-arrow-expr (jove--node-create start-pos) params))

      ;; If more than one expression in 'expr-list', wrap them in a
      ;; SequenceExpression.
      (if (< 1 (length expr-list))
          (progn
            (setq val (jove--node-create inner-start-pos))
            (jove--node-add-children* val (nreverse expr-list))
            (jove--node-finish-at val 'sequence-expression inner-end-pos))
        (setq val (car expr-list)))
      ;; Wrap in a ParenthesizedExpression
      (jove--node-finish (jove--node-add-child (jove--node-create start-pos)
                                       val)
                     'parenthesized-expression))))

(defun jove--parse-new ()
  "Parse a new expression."
  (let ((node (jove--node-create))
        (meta (jove--parse-identifier t)))
    (if (jove--eat jove-DOT)
        (jove--node-finish (jove--node-add-children node
                                            meta
                                            (jove--parse-identifier t))
                       'meta-property)
      (let ((start-pos (jove--start jove--token)))
        (jove--node-add-child node (jove--parse-subscripts (jove--parse-expr-atom)
                                                   start-pos
                                                   t))
        (when (jove--eat jove-PAREN-L)
          (jove--node-add-children* node (jove--parse-expr-list jove-PAREN-R)))
        (jove--node-finish node 'new-expression)))))

(defun jove--parse-template-element ()
  "Parse template element."
  (let ((node (jove--node-create)))
    (jove-next)
    (when (eq jove-BACKQUOTE (jove--type jove--token))
      (jove--set-node-info node 'tail))
    (jove--node-finish node 'template-element)))

(defun jove--parse-template ()
  "Parse template literal."
  (let ((elt nil)
        (elt-list '())
        (node (jove--node-create)))
    (jove-next)                             ; Move over '`'
    (setq elt (jove--parse-template-element))
    (push elt elt-list)
    (while (not (eq 'tail (jove--node-info elt)))
      (jove--expect jove-DOLLAR-BRACE-L)
      (push (jove--parse-expression) elt-list)
      (jove--expect jove-BRACE-R)
      (push (setq elt (jove--parse-template-element)) elt-list))
    (jove-next)                             ; Move over '`'
    (jove--node-add-children* node (nreverse elt-list))
    (jove--node-finish node 'template-literal)))

(defun jove--parse-object (&optional is-pattern)
  "Parse an object literal or binding pattern.
Optionally if IS-PATTERN, parse as an ObjectPattern."
  (let ((first t)
        (start-pos nil)
        (prop nil)
        (prop-list '())
        (is-async nil)
        (is-generator nil)
        (node (jove--node-create)))
    (jove-next)                             ; Move over '{'
    (while (not (jove--eat jove-BRACE-R))
      (if first
          (setq first nil)
        (jove--expect jove-COMMA))
      ;; If there is a trailing comma, or empty expression, do nothing
      ;; and let the next round of the loop find the `jove-BRACE-R'
      (unless (or (jove--after-trailing-comma jove-BRACE-R t)
                  (eq jove-COMMA (jove--type jove--token)))
        (setq prop (jove--node-create))
        (when (not is-pattern)
          (setq is-generator (jove--eat jove-STAR)))
        (setq start-pos (jove--start jove--token))
        (jove--parse-property-name prop)
        ;; FIXME: Clean up this mess.
        (when (and (not is-pattern)
                   (not is-generator)
                   (not (eq 'computed (jove--node-info prop)))
                   (eq 'identifier (jove--node-type (jove--node-first-child prop)))
                   (string-equal "async" (jove--node-info (jove--node-first-child prop)))
                   (not (eq jove-PAREN-L (jove--type jove--token)))
                   (not (eq jove-COLON (jove--type jove--token))))
          (setq is-async t)
          (jove--node-clear-children prop)
          (jove--parse-property-name prop))
        (jove--parse-property-value prop is-pattern is-generator is-async start-pos)
        (push (jove--node-finish prop 'property) prop-list)))
    (jove--node-add-children* node (nreverse prop-list))
    (jove--node-finish node (if is-pattern 'object-pattern 'object-expression))))

;; TODO: Need a better way to flag different node options.
(defun jove--parse-property-value (prop is-pattern is-generator is-async start-pos)
  "Parse object property value.
Add as a child to the node PROP.  Boolean flags IS-PATTERN,
IS-GENERATOR, and IS-ASYNC are used to parse the property value
according to context.  START-POS is the position at the beginning
of the property."
  (when (and (or is-generator is-async)
             (eq jove-COLON (jove--type jove--token)))
    (jove--unexpected))
  (cond
   ((jove--eat jove-COLON)
    (jove--node-add-child prop (if is-pattern
                               (jove--parse-maybe-default start-pos)
                             (jove--parse-maybe-assign))))
   ((eq jove-PAREN-L (jove--type jove--token))
    (when is-pattern
      (jove--unexpected))
    (jove--node-add-child prop (jove--parse-method is-generator is-async)))
   ((and (not (eq 'computed (jove--node-info prop)))
         (eq 'identifier (jove--node-type (jove--node-first-child prop)))
         (or (string-equal "get" (jove--node-info (jove--node-first-child prop)))
             (string-equal "set" (jove--node-info (jove--node-first-child prop))))
         (not (eq jove-COMMA (jove--type jove--token)))
         (not (eq jove-BRACE-R (jove--type jove--token))))
    (when (or is-generator is-async is-pattern)
      (jove--unexpected))
    (jove--node-clear-children prop)
    (jove--parse-property-name prop)
    ;; Raise no warnings about getter or setter arguments.
    (jove--node-add-child prop (jove--parse-method)))
   ((and (not (eq 'computed (jove--node-info prop)))
         (eq 'identifier (jove--node-type (jove--node-first-child prop))))
    (if (or is-pattern
            (eq jove-EQ (jove--type jove--token)))
        (let ((id (jove--node-create (jove--start jove--prev-token)
                                 (jove--end jove--prev-token)
                                 'identifier
                                 (jove--value jove--prev-token))))
          (jove--node-add-child prop (jove--parse-maybe-default start-pos id)))
      ;; FIXME: Create a function to copy a node.
      (jove--node-add-child prop (jove--node-create (jove--start jove--prev-token)
                                            (jove--end jove--prev-token)
                                            'indentifier
                                            (jove--value jove--prev-token)))))
   (t
    (jove--unexpected))))

(defun jove--parse-property-name (prop)
  "Parse object property name.
Add as a child to the node PROP."
  (if (jove--eat jove-BRACKET-L)
      (progn
        (jove--set-node-info prop 'computed)
        (jove--node-add-child prop (jove--parse-maybe-assign))
        (jove--expect jove-BRACKET-R))
    (if (or (eq jove-NUM (jove--type jove--token))
            (eq jove-STRING (jove--type jove--token)))
        (jove--node-add-child prop (jove--parse-expr-atom))
      (jove--node-add-child prop (jove--parse-identifier t)))))

(defun jove--parse-function (node &optional is-stat allow-expr-body is-async)
  "Parse a function using the supplied NODE.
Depending on IS-STAT parse as declaration or literal.  The boolean flag
ALLOW-EXPR-BODY permits an expression body if parsing an arrow function.
The boolean flag IS-ASYNC is used to set the global `jove--in-async'."
  ;; With lexical scope the setting of global var in the `let'
  ;; should protect the global while in scope.
  (let ((jove--in-generator (and (not is-async)
                             (jove--eat jove-STAR)))
        (jove--in-async is-async)
        (jove--in-function t))
    ;; NOTE: Not tracking scope.
    (when (eq jove-NAME (jove--type jove--token))
        (jove--set-node-info node (jove--value jove--token)))

    (jove--parse-function-params node)
    (jove--parse-function-body node allow-expr-body)

    (jove--node-finish node (if is-stat
                            'function-statement
                          'function-expression))))

(defun jove--parse-method (&optional is-generator is-async)
  "Parse an object or class method.
Boolean flags IS-GENERATOR and IS-ASYNC set the global variables
`jove--in-generator' and `jove--in-async'."
  (let ((node (jove--node-create))
        (jove--in-function t)
        (jove--in-generator is-generator)
        (jove--in-async is-async)
        (params (prog1 (jove--node-create)
                  (jove--expect jove-PAREN-L))))
    (jove--node-finish (jove--node-add-children* params
                                         (jove--parse-binding-list jove-PAREN-R))
                   'parameters)
    (jove--node-add-child node params)
    (jove--parse-function-body node)
    (jove--node-finish node 'function-expression)))

(defun jove--parse-arrow-expr (node params &optional is-async)
  "Parse arrow function using supplied NODE with given PARAMS.
Boolean flag IS-ASYNC sets the global variable `jove--in-asynce'."
  (let ((jove--in-function t)
        (jove--in-generator nil)
        (jove--in-async is-async))
    (jove--node-add-child node (jove--to-assignable params))
    (jove--parse-function-body node t)
    (jove--node-finish node 'arrow-function-expression)))

(defun jove--parse-function-params (parent)
  "Parse function parameters and add as a child to PARENT."
  (let ((params (prog1 (jove--node-create)
                  (jove--expect jove-PAREN-L)))
        (param-list (jove--parse-binding-list jove-PAREN-R)))
    (jove--node-finish (jove--node-add-children* params
                                         param-list)
                   'parameters)
    (jove--node-add-child parent params)))

(defun jove--parse-function-body (parent &optional is-arrow-func)
  "Parse function body and add as child to PARENT.
If IS-ARROW-FUNC is non-nil, possibly parse body as an expression,
unless the body is contained in a brace block."
  (if (and is-arrow-func
           (not (eq jove-BRACE-L (jove--type jove--token))))
      (jove--node-add-child parent (jove--parse-maybe-assign))
    (jove--node-add-child parent (jove--parse-block nil))))

;; TODO: Combine `jove--parse-expr-list' and `jove--parse-binding-list'
(defun jove--parse-expr-list (close)
  "Parse a comma seperated list of expressions.
Return a list of nodes.  Advance parser over CLOSE, the token
type which ends the list.  Always allow empty expressions and
trailing commas."
  ;; NOTE: This function returns a list of nodes, NOT a node!
  (let ((first t)
        (expr-list '()))
    (while (not (jove--eat close))
      (if first
          (setq first nil)
        (jove--expect jove-COMMA))
      ;; Skip trailing commas and empty expressions.
      (unless (or (jove--after-trailing-comma close t)
                  (eq jove-COMMA (jove--type jove--token)))
        (push (if (eq jove-ELLIPSIS (jove--type jove--token))
                  (jove--parse-spread)
                (jove--parse-maybe-assign))
              expr-list)))
    (nreverse expr-list)))

(defun jove--parse-identifier (&optional liberal)
  "Parse token as an identifier.
If LIBERAL is non-nil keywords are converted to identifiers."
  (let ((node (jove--node-create)))
    (cond
     ((eq jove-NAME (jove--type jove--token))
      (when (and jove--in-generator
                 (string-equal "yield" (jove--value jove--token)))
        (jove--warn (jove--start jove--token) (jove--end jove--token)
                "'yield' used as an identifier inside a generator"))
      (when (and jove--in-async
                 (string-equal "await" (jove--value jove--token)))
        (jove--warn (jove--start jove--token) (jove--end jove--token)
                "'await' used as an identifier inside an async function"))
      (jove--set-node-info node (jove--value jove--token)))
     ((and liberal
           (jove--tt-keyword (jove--type jove--token)))
      (jove--set-node-info node (jove--tt-label (jove--type jove--token))))
     (t
      (jove--unexpected)))
    (jove-next)
    (jove--node-finish node 'identifier)))

(defun jove--to-assignable (node)
  "Convert existing expression NODE to assignable pattern.
Updates node type, does not perform check for valid lvalues."
  (let ((type (and node (jove--node-type node))))
    (cond
     ((eq 'object-expression type)
      (jove--set-node-type node 'object-pattern)
      (let ((prop (jove--node-first-child node)))
        (while prop
          ;; First child is 'key' and second child is 'value'.
          (jove--to-assignable (jove--node-next-sibling (jove--node-first-child prop)))
          (setq prop (jove--node-next-sibling prop)))))
     ((eq 'array-expression type)
      (jove--set-node-type node 'array-pattern)
      (let ((expr (jove--node-first-child node)))
        (while expr
          (if (eq 'rest-element (jove--node-type expr))
              (progn
                (jove--set-node-type expr 'spread-element)
                (jove--to-assignable (jove--node-first-child expr)))
            (jove--to-assignable expr))
          (setq expr (jove--node-next-sibling expr)))))
     ((eq 'assignment-expression type)
      (jove--set-node-type node 'assign-pattern)
      ;; First child is 'left' operand.
      (jove--to-assignable (jove--node-first-child node)))
     ((eq 'parenthesized-expression type)
      (jove--to-assignable (jove--node-first-child node)))))
  node)                                 ; Return the node.

(defun jove--parse-spread ()
  "Parse spread element."
  (jove--node-finish (jove--node-add-child (prog1 (jove--node-create)
                                     (jove-next))
                                   (jove--parse-maybe-assign nil))
                 'spread-element))

(defun jove--parse-rest (&optional allow-non-identifier)
  "Parse rest element.
Optionally ALLOW-NON-IDENTIFIER arguments."
  (jove--node-finish (jove--node-add-child (prog1 (jove--node-create)
                                     (jove-next))
                                   (if allow-non-identifier
                                       (if (eq jove-NAME (jove--type jove--token))
                                           (jove--parse-identifier)
                                         (jove--unexpected))
                                     (if (or (eq jove-NAME (jove--type jove--token))
                                             (eq jove-BRACKET-L (jove--type jove--token)))
                                         (jove--parse-binding-atom)
                                       (jove--unexpected))))
                 'rest-element))

(defun jove--parse-binding-atom ()
  "Parse assignable atom."
  (cond
   ((eq jove-NAME (jove--type jove--token))
    (jove--parse-identifier))
   ((eq jove-BRACKET-L (jove--type jove--token))
    ;; `jove--parse-binding-list' returns a plan list. The items need to be
    ;; added as children to the node
    (let ((node (jove--node-create))
          (bindings (progn
                      (jove-next)
                      (jove--parse-binding-list jove-BRACKET-R))))
      (jove--node-add-children* node bindings)
      (jove--node-finish node 'array-pattern)))
   ((eq jove-BRACE-L (jove--type jove--token))
    (jove--parse-object t))
   (t
    (jove--unexpected))))

;; This function is used to parse function and method parameters.
;; Perhaps return a node of type 'parameters'... nope it also used
;; in parseBindingAtom for ArrayPatterns.
(defun jove--parse-binding-list (close &optional allow-non-identifier)
  "Parse assignable list.
CLOSE is the token type which ends the list.  Always allow empty
expressions and trailing commas.  Optionally ALLOW-NON-IDENTIFIER
arguments to RestElement."
  (let ((first t)
        (elts '()))
    (while (not (jove--eat close))
      (if first
          (setq first nil)
        ;; Perhaps if the comma is not there, we should pretend it is
        ;; to prevent errors and just get an AST.
        (jove--expect jove-COMMA))
      ;; Skip trailing commas and empty expressions.
      (unless (or (jove--after-trailing-comma close t)
                  (eq jove-COMMA (jove--type jove--token)))
        (push (if (eq jove-ELLIPSIS (jove--type jove--token))
                  (jove--parse-rest allow-non-identifier)
                (jove--parse-maybe-default (jove--start jove--token)))
              elts)))
    (nreverse elts)))

(defun jove--parse-maybe-default (start-pos &optional left)
  "Parse assignment pattern around a given atom if possible.
Begin the AssignmentPattern at START-POS.  Optionally supply LEFT
operand."
  (setq left (or left (jove--parse-binding-atom)))
  (if (eq jove-EQ (jove--type jove--token))
      (let ((node (jove--node-create start-pos)))
        (jove--set-node-info node (jove--value jove--token))
        (jove-next)                         ; Move over '='
        (jove--node-finish (jove--node-add-children node
                                            left
                                            (jove--parse-maybe-assign))
                       'assignment-pattern))
    left))

(provide 'jove-parser)

;;; jove-parser.el ends here
