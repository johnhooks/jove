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

(defun jove-parser-state-make (&optional lex-state)
  "Return a vector representing a top level parser state.
If LEX-STATE not provided create an initial lexer state."
  ;; All location data is stored in the `lex-state'.
  (vector (or lex-state                 ; 0 token
              (jove-lex-state-make))
          ;; Set prev-token to a dummy state, to ensure array
          ;; functions don't accidently attempt to access nil.
          (jove-lex-state-make)             ; 1 prev-token
          nil                           ; 2 in-function
          nil                           ; 3 in-async
          nil                           ; 4 in-generator
          -1))                          ; 5 potential-arrow-at

(defsubst jove-token ()
  "Return the 'token' slot of the parser state."
  ;; Mutated in place.
  (aref jove--state 0))

(defsubst jove-prev-token ()
  "Return the 'prev-token' slot of the parser state."
  (aref jove--state 1))
(defsubst jove-set-prev-token (lex-state)
  "Set the 'prev-token' slot of the parser state to LEX-STATE."
  (aset jove--state 1 lex-state))

(defsubst jove-in-function ()
  "Return the 'in-function' slot of the parser state."
  (aref jove--state 2))
(defsubst jove-set-in-function (value)
  "Set the 'in-function' slot of the parser state to VALUE."
  (aset jove--state 2 value))

(defsubst jove-in-async ()
  "Return the 'is-async' slot of the parser state."
  (aref jove--state 3))
(defsubst jove-set-in-async (value)
  "Set the 'is-async' slot of the parser state to VALUE."
  (aset jove--state 3 value))

(defsubst jove-in-generator ()
  "Return the 'in-generator' slot of the parser state."
  (aref jove--state 4))
(defsubst jove-set-in-generator (value)
  "Set the 'in-generator' slot of the parser state to VALUE."
  (aset jove--state 4 value))

(defsubst jove-potential-arrow-at ()
  "Return the 'potential-arrow-at' slot of the parser state."
  (aref jove--state 5))
(defsubst jove-set-potential-arrow-at (value)
  "Set the 'potential-arrow-at' slot of the parser state to VALUE."
  (aset jove--state 5 value))

;;; Initialization

(defun jove-config (&optional state)
  "Initialize the parser.
If STATE not supplied create an initial state."
  (setq jove--state (or state (jove-parser-state-make)))
  (goto-char (jove-start (jove-token))))

;;; Token Movement Functions

(defsubst jove-next ()
  "Advance parser to next token."
  ;; Mutates the lexer state in place.
  (jove-set-prev-token (vconcat (jove-token)))
  (jove-next-token (jove-token)))

(defun jove-peek (&optional count)
  "Peek ahead either one or COUNT number of tokens.
Return a copy of the updated LEX-STATE."
  (save-excursion
    ;; Use `vconcat' to make a copy of LEX-STATE.
    (let ((lex-state (jove-next-token (vconcat (jove-token)))))
      (when (numberp count)
        (while (< 0 (setq count (1- count)))
          (setq lex-state (jove-next-token lex-state))))
      lex-state)))

;;; Nodes

;; Siblings (cdr node)
;; Children (cdr (car node))
;; Data (car (car node))

(defun jove-node-init (&optional start end type info)
  "Return a node.
If START is not supplied, its value will be set to start of the
current token.  Optionally supply any or all START, END, TYPE or
INFO arguments."
  (cons (cons (vector (or start         ; 0
                          (jove-start (jove-token)))
                      end               ; 1
                      type              ; 2
                      info)             ; 3
              nil)
        nil))

;; Node Data Getter and Setter Functions

(defsubst jove-node-start (node)
  "Return the 'start' slot of the NODE."
  (aref (car (car node)) 0))
(defsubst jove-set-node-start (node value)
  "Set the 'start' slot of the NODE to VALUE."
  (aset (car (car node)) 0 value))

(defsubst jove-node-end (node)
  "Return the 'end' slot of the NODE."
  (aref (car (car node)) 1))
(defsubst jove-set-node-end (node value)
  "Set the 'end' slot of the NODE to VALUE."
  (aset (car (car node)) 1 value))

(defsubst jove-node-type (node)
  "Return the 'type' slot of the NODE."
  (aref (car (car node)) 2))
(defsubst jove-set-node-type (node value)
  "Set the 'type' slot of the NODE to VALUE."
  (aset (car (car node)) 2 value))

(defsubst jove-node-info (node)
  "Return the 'info' slot of the NODE."
  (aref (car (car node)) 3))
(defsubst jove-set-node-info (node value)
  "Set the 'info' slot of the NODE to VALUE."
  (aset (car (car node)) 3 value))

(defun jove-node-p (object)
  "Return non-nil if OBJECT could represent a node."
  (and (listp object)
       (listp (car object))
       (vectorp (car (car object)))
       (= 3 (length (car (car object))))))

(defun jove-node-data (node)
  "Return the data contained in NODE."
  (car (car node)))

;; Node Building Functions

(defun jove-add-child (node child)
  "Add to NODE children the node CHILD.
Return NODE."
  (nconc (car node) child)
  node)

(defun jove-add-children (node &rest children)
  "Add to NODE children the collected node arguments CHILDREN.
Return NODE."
  (apply #'nconc (cons (car node) children))
  node)

(defun jove-add-children* (node children)
  "Add to NODE children the list of nodes CHILDREN.
Return NODE."
  (apply #'nconc (cons (car node) children))
  node)

(defun jove-clear-children (node)
  "Remove children from NODE.
Return NODE."
  (setf (car node) (cons (car (car node)) nil))
  node)

(defun jove-first-child (node)
  "Return a reference to the first child of NODE."
  (cdr (car node)))

(defun jove-next-sibling (node)
  "Return a reference to the next sibling of NODE."
  (cdr node))

(defun jove-node-finish (node type &optional info)
  "Finish NODE of TYPE.
Uses position at the end of the previous token.  Optionally add
INFO into the info slot."
  ;; The parser will have already advanced to the next token,
  ;; which is why the `jove-end' of `(jove-prev-token)' is used.
  (jove-set-node-type node type)
  (jove-set-node-end node (jove-end (jove-prev-token)))
  (when info
    (jove-set-node-info node info))
  node)                                 ; Return node.

(defun jove-node-finish-at (node type pos &optional info)
  "Finish NODE of TYPE at POS.
Optionally add INFO into the info slot."
  (jove-set-node-type node type)
  (jove-set-node-end node pos)
  (when info
    (jove-set-node-info node info))
  node)

;;; Utility Functions

(cl-defun jove-error (message &key start end type)
  "Signal an error with a MESSAGE.
If START or END not supplied, data from the current token is
used.  The error start and end locations are added to the
beginning of the message.  Unless the error TYPE is supplied
throw a `jove-parse-error'."
  (let ((start (or start (jove-start (jove-token))))
        (end (or end (jove-end (jove-token)))))
    (signal (or type 'jove-parse-error)
            (list (format "[%d,%d] Parse error: %s" start end message) start end))))

(defun jove-unexpected ()
  "Signal an unexpected token parse error."
  (jove-error (format "unexpected token: %s" (jove-tt-label (jove-tt (jove-token))))))

(defsubst jove-is (tt)
  "Return non-nil if the current token is of the type TT."
  (eq tt (jove-tt (jove-token))))

(defsubst jove-is-not (tt)
  "Return non-nil if the current token is not of the type TT."
  (not (eq tt (jove-tt (jove-token)))))

(defun jove-eat (tt)
  "Return non-nil if the current token is of the type TT.
If the test passes consume the token as a side effect."
  (when (eq tt (jove-tt (jove-token)))
    (jove-next)
    t))

(defun jove-expect (tt)
  "Expect the current token to be of the type TT.
If the test passes consumed the token as a side effect, otherwise
throw an error."
  (if (eq tt (jove-tt (jove-token)))
      (jove-next)
    (let ((expected (jove-tt-label tt))
          (found (if (jove-is jove-NAME)
                     (jove-value (jove-token))
                   (jove-tt-label (jove-tt (jove-token))))))
      (jove-error (format "expected '%s' found '%s'" expected found)))))

(defun jove-after-trailing-comma-p (tt &optional not-next)
  "Return non-nil if the current token is of the type TT.
Advance to next token, unless NOT-NEXT."
  ;; NOTE: Found in corn/src/parseutil.js
  (when (eq tt (jove-tt (jove-token)))
    (unless not-next
      (jove-next))
    t))

(defun jove-is-contextual (name)
  "Test whether current token is a contextual keyword NAME."
  (and (eq jove-NAME (jove-tt (jove-token)))
       (string-equal name (jove-value (jove-token)))))

(defun jove-eat-contextual (name)
  "Consume contextual keyword NAME is possible."
  (and (jove-is-contextual name)
       (jove-eat jove-NAME)))

(defun jove-can-insert-semicolon-p ()
  "Test whether or not a semi-colon can be inserted."
  (or (eq jove-EOF (jove-tt (jove-token)))
      (eq jove-BRACE-R (jove-tt (jove-token)))
      (jove-newline-before (jove-token))))

(defun jove-semicolon ()
  "Consume a semicolon or pretend there is a semicolon."
  (unless (or (jove-eat jove-SEMI)
              (jove-can-insert-semicolon-p))
    (jove-unexpected)))

(defun jove-null-expression ()
  "Create a null expression.
Used as a place holder is some type of nodes."
  (let ((end (jove-end (jove-prev-token))))
    (jove-node-init end end 'null-expression)))

;;; Expression Parsing Functions

(defun jove-parse-expression (&optional no-in)
  "Parse a full expression.
The boolean flag NO-IN forbids the 'in' operator."
  (let ((start-pos (jove-start (jove-token)))
        (expr (jove-parse-maybe-assign no-in)))
    (if (eq jove-COMMA (jove-tt (jove-token)))
        (let ((node (jove-node-init start-pos)))
          (jove-add-child node expr)
          (while (jove-eat jove-COMMA)
            (jove-add-child node (jove-parse-maybe-assign no-in)))
          (jove-node-finish node 'sequence-expression))
      expr)))

(defun jove-parse-maybe-assign (&optional no-in)
  "Maybe parse an assignment expression.
The boolean flag NO-IN forbids the 'in' operator."
  (if (and (jove-in-generator) (jove-is-contextual "yield"))
      (jove-parse-yield)
    (when (or (eq jove-PAREN-L (jove-tt (jove-token)))
              (eq jove-NAME (jove-tt (jove-token))))
      (jove-set-potential-arrow-at (jove-start (jove-token))))
    (let ((start-pos (jove-start (jove-token)))
          (left (jove-parse-maybe-conditional no-in)))
      ;; Find where afterLeftParse is used.
      (if (jove-tt-is-assign (jove-tt (jove-token)))
          (let ((node (jove-node-init start-pos nil nil (jove-value (jove-token)))))
            (jove-add-child node (if (eq jove-EQ (jove-tt (jove-token)))
                                       (jove-to-assignable left)
                                     left))
            (jove-next)                     ; Move over the operator.
            (jove-add-child node (jove-parse-maybe-assign no-in))
            (jove-node-finish node 'assign-expression))
        left))))

(defun jove-parse-maybe-conditional (&optional no-in)
  "Maybe parse a ternary conditional expression.
The boolean flag NO-IN forbids the 'in' operator."
  (let ((start-pos (jove-start (jove-token)))
        (expr (jove-parse-expr-ops no-in)))
    (if (eq jove-QUESTION (jove-tt (jove-token)))
        (let ((node (jove-node-init start-pos nil nil (jove-start (jove-token)))))
          ;; Putting question mark position into ConditionalExpression node info slot.
          ;; Getting weird I know. But should be helpful for indentation.
          (jove-next)                       ; Move over '?'
          (jove-add-children node
                         expr
                         (jove-parse-maybe-assign) ; no NO-IN here???
                         (progn
                           (jove-expect jove-COLON)
                           (jove-parse-maybe-assign no-in)))
          (jove-node-finish node 'conditional-expression))
      expr)))

(defun jove-parse-expr-ops (no-in)
  "Start the precedence parser.
If current expression is an ArrowFunctionExpression, just return
the expression.  The boolean flag NO-IN forbids the 'in' operator."
  (let ((start-pos (jove-start (jove-token)))
        (expr (jove-parse-maybe-unary nil)))
    (if (and (= start-pos (jove-node-start expr))
             (eq 'arrow-function-expression (jove-node-type expr)))
        expr
      (jove-parse-expr-op expr start-pos -1 no-in))))

(defun jove-parse-expr-op (left left-start-pos min-prec no-in)
  "Parse binary operators with the op precedence parsing algorithm.
LEFT is the left-hand side of the operation.  Subsequent nodes wrap
the previous and are initialize at LEFT-START-POS.  MIN-PREC
provides context that allows the function to stop and defer
further parser to one of its callers when it encounters an
operator that has a lower precedence than the set it is parsing.
The boolean flag NO-IN forbids the 'in' operator."
  (let ((start-pos nil)
        (prec (and (or (not no-in)
                       (not (eq jove-IN (jove-tt (jove-token)))))
                   (jove-tt-binop (jove-tt (jove-token))))))
    (if (and prec (> prec min-prec))
        (let ((logical (or (eq jove-LOGICAL-OR (jove-tt (jove-token)))
                           (eq jove-LOGICAL-AND (jove-tt (jove-token)))))
              (op (prog1 (jove-value (jove-token))
                    (jove-next)
                    (setq start-pos (jove-start (jove-token)))))
              (right (jove-parse-expr-op (jove-parse-maybe-unary nil) start-pos prec no-in)))
          (jove-parse-expr-op (jove-build-binary left-start-pos
                                           left
                                           right
                                           op
                                           logical)
                           left-start-pos
                           min-prec
                           no-in))
      left)))

(defun jove-build-binary (start-pos left right op logical)
  "Create a node for either a logical or binary expression.
Initialize the node at START-POS.  Add LEFT, RIGHT and OP as children.
The boolean flag LOGICAL toogles a logical or binary expression"
  ;; Putting operator string into info slot
  (let ((node (jove-node-init start-pos)))
    (jove-set-node-info node op)
    (jove-add-children node left right)
    (jove-node-finish node (if logical 'logical-expression 'binary-expression))))

(defun jove-parse-maybe-unary (saw-unary)
  "Parse both prefix and postfix unary operators.
Unless SAW-UNARY, build a binary expression when '**' is encountered."
  (let ((expr nil)
        (start-pos (jove-start (jove-token))))
    (cond
     ((and (jove-in-async)
           (jove-is-contextual "await"))
      (setq expr (jove-parse-await)))
     ((jove-tt-prefix (jove-tt (jove-token)))
      (let ((node (jove-node-init (jove-start (jove-token)) nil nil 'prefix))
            (update (eq jove-INC-DEC (jove-tt (jove-token)))))
        (jove-set-node-info node (jove-value (jove-token)))
        (jove-next)
        (jove-add-child node (jove-parse-maybe-unary t))
        (setq expr (jove-node-finish node (if update
                                           'update-expression
                                         'unary-expression)))
        (unless update (setq saw-unary t))))
     (t
      (setq expr (jove-parse-expr-subscripts))
      (while (and (jove-tt-postfix (jove-tt (jove-token)))
                  (not (jove-can-insert-semicolon-p)))
        (let ((node (jove-node-init start-pos nil nil 'postfix)))
          (jove-add-child node expr)
          (jove-set-node-info node (jove-value (jove-token)))
          (jove-next)
          (setq expr (jove-node-finish node 'update-expression))))))
    (if (and (not saw-unary)
             (eq jove-STARSTAR (jove-tt (jove-token))))
        (jove-build-binary start-pos
                        expr
                        (jove-parse-maybe-unary nil)
                        (jove-value (jove-token))
                        nil)
      expr)))

(defun jove-parse-expr-subscripts ()
  "Parse call, dot, and bracket notation subscript expressions."
  (let ((start-pos (jove-start (jove-token)))
        (expr (jove-parse-expr-atom)))
    (jove-parse-subscripts expr start-pos)))

(defun jove-parse-subscripts (base start-pos &optional no-calls)
  "Possibly parse the subscripts of BASE.
All subsequent nodes wrap BASE and initialize at START-POS.
Optionally if NO-CALLS disallow the parsing of call expressions."
  (let ((computed nil)
        (maybe-async-arrow (and (eq 'identifier (jove-node-type base))
                                (string-equal "async" (jove-node-info base))
                                (= (jove-end (jove-prev-token)) (jove-node-end base))
                                (not (jove-can-insert-semicolon-p)))))
    (catch 'node
      (while t
        (cond
         ;; Member Expression
         ((or (setq computed (jove-eat jove-BRACKET-L))
              (jove-eat jove-DOT))
          (let ((node (jove-node-init start-pos nil nil (when computed 'computed))))
            (jove-add-children node
                                 base
                                 (if computed
                                     (jove-parse-expression)
                                   (jove-parse-identifier t)))
            (if computed
                (jove-expect jove-BRACKET-R)
              (let ((token (jove-prev-token)))
                (jove-set-face (jove-start token) (jove-end token) 'js2-object-property)))
            (setq base (jove-node-finish node 'member-expression))))
         ;; Call Expression
         ((and (not no-calls)
               (eq jove-PAREN-L (jove-tt (jove-token))))
          (let ((token (jove-prev-token)))
            (when (eq jove-NAME (jove-tt token))
            (jove-set-face (jove-start token) (jove-end token) 'js2-function-call)))
          (let ((exprs (jove-node-init)))
            (jove-next)                     ; Move over '('
            (jove-add-children* exprs (jove-parse-expr-list jove-PAREN-R))
            (jove-set-node-end exprs (jove-end (jove-prev-token)))
            ;; 'exprs' is not finished like most nodes.  It is almost
            ;; complete, only the type slot remains to be set.
            (if (and maybe-async-arrow
                     (not (jove-can-insert-semicolon-p))
                     (jove-eat jove-ARROW))
                (throw 'node (progn
                               (jove-set-node-type exprs 'parameters)
                               (jove-parse-arrow-expr (jove-node-init start-pos)
                                                   exprs
                                                   t)))
              (let ((node (jove-node-init start-pos)))
                (jove-set-node-type exprs 'arguments)
                (jove-add-children node base exprs)
                (jove-node-finish node 'call-expression)
                (setq base node)))))
         ;; Tagged Template Expression
         ((eq jove-BACKQUOTE (jove-tt (jove-token)))
          (jove-node-finish (jove-add-children (jove-node-init start-pos)
                                              base
                                              (jove-parse-template))
                         'tagged-template-expression))
         (t
          (throw 'node base)))))))

(defun jove-parse-expr-atom ()
  "Parse an atomic expression."
  (let ((type (jove-tt (jove-token)))
        (can-be-arrow (= (jove-start (jove-token)) (jove-potential-arrow-at))))
    (cond
     ((eq jove-SUPER type)
      (when (not (jove-in-function))
        (jove-warn (jove-start (jove-token)) (jove-end (jove-token)) "'super' outside of function or class"))
      (prog1 (jove-node-init (jove-start (jove-token))
                         (jove-end (jove-token))
                         'super)
        (jove-next)))
     ((eq jove-THIS type)
      (prog1 (jove-node-init (jove-start (jove-token))
                         (jove-end (jove-token))
                         'this-expression)
        (jove-next)))
     ((eq jove-NAME type)
      (let ((start-pos (jove-start (jove-token)))
            (id (jove-parse-identifier nil)))
        (cond
         ((and (string-equal "async" (jove-node-info id))
               (not (jove-can-insert-semicolon-p))
               (jove-eat jove-FUNCTION))
          ;; Parse async function
          (jove-parse-function (jove-node-init start-pos) nil nil t))
         ((and can-be-arrow
               (not (jove-can-insert-semicolon-p)))
          (cond
           ((jove-eat jove-ARROW)
            (let ((params (jove-node-init (jove-node-start id)
                                         (jove-node-end id)
                                         'parameters)))
              ;; Wrap the single parameter in a 'parameters' node.
              (jove-add-child params id)
              (jove-parse-arrow-expr (jove-node-init start-pos) params nil)))
           ((and (string-equal "async" (jove-node-info id))
                 (eq jove-NAME (jove-tt (jove-token))))
            (setq id (jove-parse-identifier))
            (if (or (jove-can-insert-semicolon-p)
                    (not (jove-eat jove-ARROW)))
                (jove-unexpected)
              (let ((params (jove-node-init (jove-node-start id)
                                           (jove-node-end id)
                                           'parameters)))
                ;; Wrap the single parameter in a 'parameters' node.
                (jove-add-child params id)
                (jove-parse-arrow-expr (jove-node-init start-pos params t) params))))
           (t           ; Still need to return ID if none of the above match.
            id)))
         (t
          id))))
     ((memq type (list jove-REGEXP jove-STRING jove-NUM jove-NULL jove-UNDEFINED jove-TRUE jove-FALSE))
      (prog1 (jove-node-init (jove-start (jove-token))
                         (jove-end (jove-token))
                         'literal)
        (jove-next)))
     ((eq jove-PAREN-L type)
      (jove-parse-paren-expr can-be-arrow))
     ((eq jove-BRACKET-L type)
      (let ((node (jove-node-init)))
        (jove-next)
        (jove-add-children* node (jove-parse-expr-list jove-BRACKET-R))
        (jove-node-finish node 'array-expression)))
     ((eq jove-BRACE-L type)
      (jove-parse-object nil))
     ((eq jove-FUNCTION type)
      (let ((node (jove-node-init)))
        (jove-next)
        (jove-parse-function node nil)))
     ((eq jove-CLASS type)
      (jove-parse-class (jove-node-init) nil))
     ((eq jove-NEW type)
      (jove-parse-new))
     ((eq jove-BACKQUOTE type)
      (jove-parse-template))
     (t
      (jove-unexpected)))))

;; Look at the difference between the expression collection of `jove-parse-expr-list'
;; and see if it can be used here. Since I want to allow trailing commas and empty
;; expressions (because that is the best course while editting), I can probably
;; used that function to collect the list. Then figure out what to do with it after.

;; Also to note, when changing an expression list to an assignable, I can change the
;; node names though I will not worry if there are invalid expressions in it. Just
;; parse destructured assignments as liberally as possible.

(defun jove-parse-paren-expr (&optional can-be-arrow)
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
        (start-pos (jove-start (jove-token)))
        (inner-start-pos (progn (jove-next) (jove-start (jove-token)))))
    ;; FIXME: Could `parse-expr-list' be used here?
    (while (not (eq jove-PAREN-R (jove-tt (jove-token))))
      (if first
          (setq first nil)
        (jove-expect jove-COMMA))
      ;; Allow empty expressions and trailing commas.
      (unless (or (jove-after-trailing-comma-p jove-PAREN-R t)
                  (eq jove-COMMA (jove-tt (jove-token))))
        (if (eq jove-ELLIPSIS (jove-tt (jove-token)))
            (push (jove-parse-rest) expr-list)
          (unless (jove-after-trailing-comma-p jove-PAREN-R t)
            (push (jove-parse-maybe-assign nil) expr-list)))))
    (setq inner-end-pos (jove-start (jove-token)))
    (jove-expect jove-PAREN-R)

    ;; Return a ArrowFunctionExpression or ParenthesizedExpression
    (if (and can-be-arrow
             (not (jove-can-insert-semicolon-p))
             (eq jove-ARROW (jove-tt (jove-token))))
        (let ((params (jove-node-init start-pos)))
          (mapc #'jove-to-assignable expr-list)
          (jove-add-children* params (nreverse expr-list))
          (jove-node-finish params 'parameters)
          (jove-next)                         ; Move over '=>'
          (jove-parse-arrow-expr (jove-node-init start-pos) params))

      ;; If more than one expression in 'expr-list', wrap them in a
      ;; SequenceExpression.
      (if (< 1 (length expr-list))
          (progn
            (setq val (jove-node-init inner-start-pos))
            (jove-add-children* val (nreverse expr-list))
            (jove-node-finish-at val 'sequence-expression inner-end-pos))
        (setq val (car expr-list)))
      ;; Wrap in a ParenthesizedExpression
      (jove-node-finish (jove-add-child (jove-node-init start-pos)
                                       val)
                     'parenthesized-expression))))

(defun jove-parse-new ()
  "Parse a new expression."
  (let ((node (jove-node-init))
        (meta (jove-parse-identifier t)))
    (if (jove-eat jove-DOT)
        (jove-node-finish (jove-add-children node
                                            meta
                                            (jove-parse-identifier t))
                       'meta-property)
      (let ((start-pos (jove-start (jove-token))))
        (jove-add-child node (jove-parse-subscripts (jove-parse-expr-atom)
                                                   start-pos
                                                   t))
        (when (jove-is jove-PAREN-L)
          (let ((params (jove-node-init)))
            (jove-next)
            (jove-add-children* params
                                  (jove-parse-expr-list jove-PAREN-R))
            (jove-node-finish params 'arguments)
            (jove-add-child node params)))
        (jove-node-finish node 'new-expression)))))

(defun jove-parse-template-element ()
  "Parse template element."
  (let ((node (jove-node-init)))
    (jove-next)
    (when (eq jove-BACKQUOTE (jove-tt (jove-token)))
      (jove-set-node-info node 'tail))
    (jove-node-finish node 'template-element)))

(defun jove-parse-template ()
  "Parse template literal."
  (let ((elt nil)
        (elt-list '())
        (node (jove-node-init)))
    (jove-next)                             ; Move over '`'
    (setq elt (jove-parse-template-element))
    (push elt elt-list)
    (while (not (eq 'tail (jove-node-info elt)))
      (jove-expect jove-DOLLAR-BRACE-L)
      (push (jove-parse-expression) elt-list)
      (jove-expect jove-BRACE-R)
      (push (setq elt (jove-parse-template-element)) elt-list))
    (jove-next)                             ; Move over '`'
    (jove-add-children* node (nreverse elt-list))
    (jove-node-finish node 'template-literal)))

(defun jove-parse-object (&optional is-pattern)
  "Parse an object literal or binding pattern.
Optionally if IS-PATTERN, parse as an ObjectPattern."
  (let ((first t)
        (start-pos nil)
        (prop nil)
        (prop-list '())
        (is-async nil)
        (is-generator nil)
        (node (jove-node-init)))
    (jove-next)                             ; Move over '{'
    (while (not (jove-eat jove-BRACE-R))
      (if first
          (setq first nil)
        (jove-expect jove-COMMA))
      ;; If there is a trailing comma, or empty expression, do nothing
      ;; and let the next round of the loop find the `jove-BRACE-R'
      (unless (or (jove-after-trailing-comma-p jove-BRACE-R t)
                  (eq jove-COMMA (jove-tt (jove-token))))
        (setq prop (jove-node-init))
        (when (not is-pattern)
          (setq is-generator (jove-eat jove-STAR)))
        (setq start-pos (jove-start (jove-token)))
        (jove-parse-property-name prop is-pattern)
        ;; FIXME: Clean up this mess.
        (when (and (not is-pattern)
                   (not is-generator)
                   (not (eq 'computed (jove-node-info prop)))
                   (eq 'identifier (jove-node-type (jove-first-child prop)))
                   (string-equal "async" (jove-node-info (jove-first-child prop)))
                   (not (eq jove-PAREN-L (jove-tt (jove-token))))
                   (not (eq jove-COLON (jove-tt (jove-token)))))
          (setq is-async t)
          (jove-clear-children prop)
          (jove-parse-property-name prop is-pattern))
        (jove-parse-property-value prop is-pattern is-generator is-async start-pos)
        (push (jove-node-finish prop 'property) prop-list)))
    (jove-add-children* node (nreverse prop-list))
    (jove-node-finish node (if is-pattern 'object-pattern 'object-expression))))

;; TODO: Need a better way to flag different node options.
(defun jove-parse-property-value (prop is-pattern is-generator is-async start-pos)
  "Parse object property value.
Add as a child to the node PROP.  Boolean flags IS-PATTERN,
IS-GENERATOR, and IS-ASYNC are used to parse the property value
according to context.  START-POS is the position at the beginning
of the property."
  (when (and (or is-generator is-async)
             (eq jove-COLON (jove-tt (jove-token))))
    (jove-unexpected))
  (cond
   ((jove-is jove-COLON)
    (jove-set-face (jove-start (jove-prev-token)) (jove-end (jove-prev-token)) 'js2-object-property)
    (jove-next)                             ; Move over ':'
    (jove-add-child prop (if is-pattern
                               (jove-parse-maybe-default start-pos)
                             (jove-parse-maybe-assign))))
   ((eq jove-PAREN-L (jove-tt (jove-token)))
    (when is-pattern
      (jove-unexpected))
    (jove-add-child prop (jove-parse-method is-generator is-async)))
   ((and (not (eq 'computed (jove-node-info prop)))
         (eq 'identifier (jove-node-type (jove-first-child prop)))
         (or (string-equal "get" (jove-node-info (jove-first-child prop)))
             (string-equal "set" (jove-node-info (jove-first-child prop))))
         (not (eq jove-COMMA (jove-tt (jove-token))))
         (not (eq jove-BRACE-R (jove-tt (jove-token)))))
    (when (or is-generator is-async is-pattern)
      (jove-unexpected))
    (jove-clear-children prop)
    (jove-parse-property-name prop)
    ;; Raise no warnings about getter or setter arguments.
    (jove-add-child prop (jove-parse-method)))
   ((and (not (eq 'computed (jove-node-info prop)))
         (eq 'identifier (jove-node-type (jove-first-child prop))))
    (if (or is-pattern
            (eq jove-EQ (jove-tt (jove-token))))
        (let ((id (jove-node-init (jove-start (jove-prev-token))
                                 (jove-end (jove-prev-token))
                                 'identifier
                                 (jove-value (jove-prev-token)))))
          (jove-add-child prop (jove-parse-maybe-default start-pos id)))
      ;; FIXME: Create a function to copy a node.
      (jove-add-child prop (jove-node-init (jove-start (jove-prev-token))
                                            (jove-end (jove-prev-token))
                                            'indentifier
                                            (jove-value (jove-prev-token))))))
   (t
    (jove-unexpected))))

(defun jove-parse-property-name (prop &optional is-pattern)
  "Parse object property name.
Add the parsed node to the node PROP.  The boolean IS-PATTERN
flags whether parsing an object pattern.  Add as a child to the
node PROP."
  (if (jove-eat jove-BRACKET-L)
      (progn
        (jove-set-node-info prop 'computed)
        (jove-add-child prop (jove-parse-maybe-assign))
        (jove-expect jove-BRACKET-R))
    (if (or (eq jove-NUM (jove-tt (jove-token)))
            (eq jove-STRING (jove-tt (jove-token))))
        (jove-add-child prop (jove-parse-expr-atom))
      (when is-pattern
        (jove-set-face (jove-start (jove-token)) (jove-end (jove-token)) font-lock-variable-name-face))
      (jove-add-child prop (jove-parse-identifier t)))))

(defun jove-parse-method (&optional is-generator is-async)
  "Parse an object or class method.
Boolean flags IS-GENERATOR and IS-ASYNC set the global variables
`jove-in-generator' and `jove--inasync'."
  (let ((node (jove-node-init))
        (old-in-function (jove-in-function))
        (old-in-generator (jove-in-generator))
        (old-in-async (jove-in-async))
        (params (prog1 (jove-node-init)
                  (jove-expect jove-PAREN-L))))

    (jove-set-in-function t)
    (jove-set-in-generator is-generator)
    (jove-set-in-async is-async)

    (jove-node-finish (jove-add-children* params
                                  (jove-parse-binding-list jove-PAREN-R nil t))
                  'parameters)

    (jove-set-in-function old-in-function)
    (jove-set-in-generator old-in-generator)
    (jove-set-in-async old-in-async)

    (jove-add-child node params)
    (jove-parse-function-body node)
    (jove-node-finish node 'function-expression)))

(defun jove-parse-arrow-expr (node params &optional is-async)
  "Parse arrow function using supplied NODE with given PARAMS.
Boolean flag IS-ASYNC sets the global variable `jove--in-asynce'."
  (let ((old-in-function (jove-in-function))
        (old-in-generator (jove-in-generator))
        (old-in-async (jove-in-async)))

    (jove-set-in-function t)
    (jove-set-in-generator nil)
    (jove-set-in-async is-async)

    (jove-add-child node (jove-to-assignable params))
    (jove-parse-function-body node t)

    (jove-set-in-function old-in-function)
    (jove-set-in-generator old-in-generator)
    (jove-set-in-async old-in-async)

    (jove-node-finish node 'arrow-function-expression)))

(defun jove-parse-function-body (parent &optional is-arrow-func)
  "Parse function body and add as child to PARENT.
If IS-ARROW-FUNC is non-nil, possibly parse body as an expression,
unless the body is contained in a brace block."
  (if (and is-arrow-func
           (not (eq jove-BRACE-L (jove-tt (jove-token)))))
      (jove-add-child parent (jove-parse-maybe-assign))
    (jove-add-child parent (jove-parse-block))))

;; TODO: Combine `jove-parse-expr-list' and `jove-parse-binding-list'
(defun jove-parse-expr-list (close)
  "Parse a comma seperated list of expressions.
Return a list of nodes.  Advance parser over CLOSE, the token
type which ends the list.  Always allow empty expressions and
trailing commas."
  ;; NOTE: This function returns a list of nodes, NOT a node!
  (let ((first t)
        (expr-list '()))
    (while (not (jove-eat close))
      (if first
          (setq first nil)
        (jove-expect jove-COMMA))
      ;; Skip trailing commas and empty expressions.
      (unless (or (jove-after-trailing-comma-p close t)
                  (eq jove-COMMA (jove-tt (jove-token))))
        (push (if (eq jove-ELLIPSIS (jove-tt (jove-token)))
                  (jove-parse-spread)
                (jove-parse-maybe-assign))
              expr-list)))
    (nreverse expr-list)))

(defun jove-parse-identifier (&optional liberal)
  "Parse token as an identifier.
If LIBERAL is non-nil keywords are converted to identifiers."
  (let ((node (jove-node-init)))
    (cond
     ((eq jove-NAME (jove-tt (jove-token)))
      (when (and (jove-in-generator)
                 (string-equal "yield" (jove-value (jove-token))))
        (jove-warn (jove-start (jove-token)) (jove-end (jove-token))
                "'yield' used as an identifier inside a generator"))
      (when (and (jove-in-async)
                 (string-equal "await" (jove-value (jove-token))))
        (jove-warn (jove-start (jove-token)) (jove-end (jove-token))
                "'await' used as an identifier inside an async function"))
      (jove-set-node-info node (jove-value (jove-token))))
     ((and liberal
           (jove-tt-keyword (jove-tt (jove-token))))
      (jove-set-node-info node (jove-tt-label (jove-tt (jove-token)))))
     (t
      (jove-unexpected)))
    (jove-next)
    (jove-node-finish node 'identifier)))

(defun jove-parse-yield ()
  "Parse a yield expression."
  ;; TODO:
  )

(defun jove-parse-await ()
  "Parse an await expression."
  ;; TODO:
  )
;;; Left Value Functions

(defun jove-to-assignable (node)
  "Convert existing expression NODE to assignable pattern.
Updates node type, does not perform check for valid lvalues."
  (let ((type (and node (jove-node-type node))))
    (cond
     ((eq 'identifier type)
      (jove-set-face (jove-node-start node) (jove-node-end node) 'font-lock-variable-name-face))
     ((eq 'object-expression type)
      (jove-set-node-type node 'object-pattern)
      (let ((prop (jove-first-child node)))
        (while prop
          ;; (let ((key (jove-first-child prop)))
          ;;   (when (eq 'identifier (jove-node-type key))
          ;;     (jove-set-face (jove-node-start key) (jove-node-end key) 'font-lock-variable-name-face)))
          ;; First child is 'key' and second child is 'value'.
          (jove-to-assignable (jove-next-sibling (jove-first-child prop)))
          (setq prop (jove-next-sibling prop)))))
     ((eq 'array-expression type)
      (jove-set-node-type node 'array-pattern)
      (let ((expr (jove-first-child node)))
        (while expr
          (if (eq 'rest-element (jove-node-type expr))
              (progn
                (jove-set-node-type expr 'spread-element)
                (jove-to-assignable (jove-first-child expr)))
            (jove-to-assignable expr))
          (setq expr (jove-next-sibling expr)))))
     ((eq 'assignment-expression type)
      (jove-set-node-type node 'assign-pattern)
      ;; First child is 'left' operand.
      (jove-to-assignable (jove-first-child node)))
     ((eq 'parenthesized-expression type)
      (jove-to-assignable (jove-first-child node)))))
  node)                                 ; Return the node.

(defun jove-parse-spread ()
  "Parse spread element."
  (jove-node-finish (jove-add-child (prog1 (jove-node-init)
                                     (jove-next))
                                   (jove-parse-maybe-assign nil))
                 'spread-element))

(defun jove-parse-rest (&optional allow-non-identifier)
  "Parse rest element.
Optionally ALLOW-NON-IDENTIFIER arguments."
  (jove-node-finish (jove-add-child (prog1 (jove-node-init)
                                     (jove-next))
                                   (if allow-non-identifier
                                       (if (eq jove-NAME (jove-tt (jove-token)))
                                           (jove-parse-identifier)
                                         (jove-unexpected))
                                     (if (or (eq jove-NAME (jove-tt (jove-token)))
                                             (eq jove-BRACKET-L (jove-tt (jove-token))))
                                         (jove-parse-binding-atom t)
                                       (jove-unexpected))))
                 'rest-element))

(defun jove-parse-binding-atom (&optional highlight)
  "Parse assignable atom.
The boolean HIGHLIGHT flags to set variable name face."
  (cond
   ((eq jove-NAME (jove-tt (jove-token)))
    (when highlight
      (jove-set-face (jove-start (jove-token)) (jove-end (jove-token)) font-lock-variable-name-face))
    (jove-parse-identifier))
   ((eq jove-BRACKET-L (jove-tt (jove-token)))
    ;; `jove-parse-binding-list' returns a plan list. The items need to be
    ;; added as children to the node
    (let ((node (jove-node-init))
          (bindings (progn
                      (jove-next)
                      (jove-parse-binding-list jove-BRACKET-R nil highlight))))
      (jove-add-children* node bindings)
      (jove-node-finish node 'array-pattern)))
   ((eq jove-BRACE-L (jove-tt (jove-token)))
    (jove-parse-object t))
   (t
    (jove-unexpected))))

;; This function is used to parse function and method parameters.
;; Perhaps return a node of type 'parameters'... nope it also used
;; in parseBindingAtom for ArrayPatterns.
(defun jove-parse-binding-list (close &optional allow-non-identifier highlight)
  "Parse assignable list.
CLOSE is the token type which ends the list.  Always allow empty
expressions and trailing commas.  Optionally ALLOW-NON-IDENTIFIER
arguments to RestElement.  The boolean HIGHLIGHT flags to set
variable name face."
  (let ((first t)
        (elts '()))
    (while (not (jove-eat close))
      (if first
          (setq first nil)
        ;; Perhaps if the comma is not there, we should pretend it is
        ;; to prevent errors and just get an AST.
        (jove-expect jove-COMMA))
      ;; Skip trailing commas and empty expressions.
      (unless (or (jove-after-trailing-comma-p close t)
                  (eq jove-COMMA (jove-tt (jove-token))))
        (push (if (eq jove-ELLIPSIS (jove-tt (jove-token)))
                  (jove-parse-rest allow-non-identifier)
                (jove-parse-maybe-default (jove-start (jove-token)) nil highlight))
              elts)))
    (nreverse elts)))

(defun jove-parse-maybe-default (start-pos &optional left highlight)
  "Parse assignment pattern around a given atom if possible.
Begin the AssignmentPattern at START-POS.  Optionally supply LEFT
operand.  The boolean HIGHLIGHT flags to set variable name face."
  (setq left (or left (jove-parse-binding-atom highlight)))
  (if (eq jove-EQ (jove-tt (jove-token)))
      (let ((node (jove-node-init start-pos)))
        (jove-set-node-info node (jove-value (jove-token)))
        (jove-next)                         ; Move over '='
        (jove-node-finish (jove-add-children node
                                     left
                                     (jove-parse-maybe-assign))
                      'assignment-pattern))
    left))

;;; Statement Parsing Functions

(defun jove-is-let ()
  "Return non-nil if looking at a 'let'."
  (and (eq jove-NAME (jove-tt (jove-token)))
       (string-equal "let" (jove-value (jove-token)))
       (let ((type (jove-tt (jove-peek))))
         (or (eq jove-BRACE-L type)
             (eq jove-BRACKET-L type)
             (eq jove-NAME type)))))

(defun jove-is-function ()
  "Return non-nil if next token starts a function."
  (or (jove-is jove-FUNCTION)
      (jove-is-async-function)))

(defun jove-is-async-function ()
  "Return non-nil if looking at an 'async' function."
  (and (eq jove-NAME (jove-tt (jove-token)))
       (string-equal "async" (jove-value (jove-token)))
       (let ((next (jove-peek)))
         (and (not (jove-newline-before next))
              (eq jove-FUNCTION (jove-tt next))))))

(defun jove-parse-top-level (node)
  "Parse a program."
  (let ((statements '()))
    (condition-case err
        (while (jove-is-not jove-EOF)
          (push (jove-parse-statement t) statements))
      (jove-parse-error
       (when jove-verbose (message "%s" (cadr err)))))
    (jove-add-children* node (nreverse statements))
    (jove-next)                             ; Move over EOF.
    (jove-node-finish node 'program)))

(defun jove-parse-statement (&optional declaration)
  "Parse a single statement"
  (let ((kind nil)
        (tt (jove-tt (jove-token)))
        (node (jove-node-init)))
    (when (jove-is-let)
      (setq tt jove-VAR)
      (setq kind 'let)
      (jove-set-face (jove-start (jove-token)) (jove-end (jove-token)) font-lock-keyword-face))
    (cond
     ((or (eq jove-BREAK tt)
          (eq jove-CONTINUE tt))
      (jove-parse-break-continue-statement node tt))
     ((eq jove-DEBUGGER tt) (jove-parse-debugger-statement node))
     ((eq jove-DO tt) (jove-parse-do-statement node))
     ((eq jove-FOR tt) (jove-parse-for-statement node))
     ((eq jove-FUNCTION tt)
      (if (not declaration)
          (jove-unexpected)
        (jove-parse-function-statement node)))
     ((eq jove-CLASS tt)
      (if (not declaration)
          (jove-unexpected)
        (jove-parse-class node t)))
     ((eq jove-IF tt) (jove-parse-if-statement node))
     ((eq jove-RETURN tt) (jove-parse-return-statement node))
     ((eq jove-SWITCH tt) (jove-parse-switch-statement node))
     ((eq jove-THROW tt) (jove-parse-throw-statement node))
     ((eq jove-TRY tt) (jove-parse-try-statement node))
     ((or (eq jove-CONST tt)
          (eq jove-VAR tt))
      (setq kind (or kind (intern (jove-value (jove-token)))))
      (if (and (not declaration)
               (not (eq 'var kind)))
          (jove-unexpected)
        (jove-parse-var-statement node kind)))
     ((eq jove-WHILE tt) (jove-parse-while-statement node))
     ((eq jove-WITH tt) (jove-parse-with-statement node))
     ((eq jove-BRACE-L tt) (jove-parse-block))
     ((eq jove-SEMI tt) (jove-parse-empty-statement node))
     ((eq jove-IMPORT tt) (jove-parse-import node))
     ((eq jove-EXPORT tt) (jove-parse-export node))
     ((and declaration
           (jove-is-async-function))
      (jove-next)
      (jove-parse-function-statement node t))
     (t
      (let ((maybe-name (jove-value (jove-token)))
            (expr (jove-parse-expression)))
        (if (and (eq jove-NAME tt)
                 (eq 'identifier (jove-node-type expr))
                 (jove-eat jove-COLON))
            (jove-parse-labeled-statement node maybe-name expr)
          (jove-parse-expression-statement node expr)))))))

(defun jove-parse-break-continue-statement (node tt-type)
  "Return NODE as either a 'break' or 'continue' statement."
  (jove-next)
  (unless (or (jove-eat jove-SEMI)
              (jove-can-insert-semicolon-p))
    (if (not (eq jove-NAME (jove-tt (jove-token))))
        (jove-unexpected)
      (jove-add-child node (jove-parse-identifier))
      (jove-semicolon)))
  (jove-node-finish node (if (eq jove-BREAK tt-type)
                          'break-statement
                        'continue-statement)))

(defun jove-parse-debugger-statement (node)
  "Return NODE as a 'debugger' statement."
  (jove-next)
  (jove-semicolon)
  (jove-node-finish node 'debugger-statement))

(defun jove-parse-do-statement (node)
  "Return NODE as a 'do' statement."
  (jove-next)
  (jove-add-child node (jove-parse-statement))
  (jove-expect jove-WHILE)
  (jove-add-child node (jove-parse-paren-expr))
  (jove-eat jove-SEMI)
  (jove-node-finish node 'do-while-statement))

(defun jove-parse-for-statement (node)
  "Return NODE as a 'for' statement.
Distinguish between 'regular for', 'for in' and 'for of' statements."
  (jove-next)
  (jove-expect jove-PAREN-L)
  (let ((is-let (jove-is-let)))
    (cond
     ((eq jove-SEMI (jove-tt (jove-token)))
      (jove-parse-for node nil))
     ((or is-let (jove-is jove-VAR) (jove-is jove-CONST))
      (let ((init (jove-node-init))
            (kind (if is-let 'let (intern (jove-value (jove-token))))))
        (jove-next)
        (jove-parse-var init t kind)
        (jove-node-finish init 'variable-declaration)
        (if (or (jove-is jove-IN)
                (jove-is-contextual "of"))
            ;; There are a few more tests in parseForStatement
            ;; need to look though more thoroughly.
            (jove-parse-for-in node init)
          (jove-parse-for node init))))
     (t
      (let ((init (jove-parse-expression t)))
        (if (or (jove-is jove-IN)
                (jove-is-contextual "of"))
            (jove-parse-for-in node (jove-to-assignable init))
          (jove-parse-for node init)))))))

(defun jove-parse-function-statement (node &optional is-async)
  "Return NODE as a 'function' statement."
  (jove-next)
  (jove-parse-function node t nil is-async))

(defun jove-parse-if-statement (node)
  "Return NODE as a 'if' statement."
  (jove-next)
  (jove-add-children node
                       (jove-parse-paren-expr)
                       (jove-parse-statement (jove-is-function))
                       (when (jove-eat jove-ELSE)
                         (jove-parse-statement (jove-is-function))))
  (jove-node-finish node 'if-statement))

(defun jove-parse-return-statement (node)
  "Return NODE as a 'return' statement."
  (jove-next)
  (unless (or (jove-eat jove-SEMI)
              (jove-can-insert-semicolon-p))
    (jove-add-child node (jove-parse-expression))
    (jove-semicolon))
  (jove-node-finish node 'return-statement))

(defun jove-parse-switch-statement (node)
  "Return NODE as a 'switch' statement."
  (jove-next)
  (jove-add-child node (jove-parse-paren-expr))
  (let (cases
        current                         ; Current switch case.
        is-case)
    (jove-expect jove-BRACE-L)
    (while (not (eq jove-BRACE-R (jove-tt (jove-token))))
      (if (or (setq is-case (jove-is jove-CASE))
              (jove-is jove-DEFAULT))
          (progn
            (when current (jove-node-finish current 'switch-case))
            (push (setq current (jove-node-init)) cases)
            (jove-next)
            (if is-case
                (jove-add-child current (jove-parse-expression))
              (jove-add-child current (jove-null-expression)))
            (jove-expect jove-COLON))

        (if (not current)
            (jove-unexpected)
          (jove-add-child current (jove-parse-statement t)))))
    (when current (jove-node-finish current 'switch-case))

    (jove-next)                             ; Move over '}'
    (jove-node-finish node 'switch-statement)))

(defun jove-parse-throw-statement (node)
  "Return NODE as a 'throw' statement."
  (jove-next)
  ;; A newline between the 'throw' keyword and the expression
  ;; is illegal.  Though I assume if it isn't there it just
  ;; hasn't been typed yet, so fill in with a null expression.
  (jove-add-child node (if (jove-newline-before (jove-token))
                             (jove-null-expression)
                           (jove-parse-expression)))
  (jove-semicolon)
  (jove-node-finish node 'throw-statement))

(defun jove-parse-try-statement (node)
  "Return NODE as a 'try' statement."
  (jove-next)
  (jove-add-child node (jove-parse-block))
  (when (jove-is jove-CATCH)
    (let ((clause (jove-node-init)))
      (jove-next)
      (jove-expect jove-PAREN-L)
      (jove-add-child clause (jove-parse-binding-atom))
      (jove-expect jove-PAREN-R)
      (jove-add-child clause (jove-parse-block))
      (jove-add-child node (jove-node-finish clause 'catch-clause))))
  (when (jove-eat jove-FINALLY)
    (jove-add-child node (jove-parse-block)))
  (jove-node-finish node 'try-statement))

(defun jove-parse-var-statement (node kind)
  "Return NODE as a 'var' statement."
  (jove-next)
  (jove-parse-var node nil kind)
  (jove-semicolon)
  (jove-node-finish node 'variable-declaration))

(defun jove-parse-while-statement (node)
  "Return NODE as a 'while' statement."
  (jove-next)
  (jove-add-children node
                       (jove-parse-paren-expr)
                       (jove-parse-statement nil))
  (jove-node-finish node 'while-statement))

(defun jove-parse-with-statement (node)
  "Return NODE as a 'with' statement."
  (jove-next)
  (jove-add-children node
                       (jove-parse-paren-expr)
                       (jove-parse-statement nil))
  (jove-node-finish node 'with-statement))

(defun jove-parse-empty-statement (node)
  "Return NODE as a empty statement."
  ;; TODO: Double check the positions returned.
  (jove-next)
  (jove-node-finish node 'empty-statement))

(defun jove-parse-labeled-statement (node maybe-name expr)
  "Return NODE as a labeled statement."
  ;; TODO:
  )

(defun jove-parse-expression-statement (node expression)
  "Return NODE as an expression statement.
EXPRESSION is supplied by `jove-parse-statement'."
  (jove-add-child node expression)
  (jove-semicolon)
  (jove-node-finish node 'expression-statement))

(defun jove-parse-block ()
  "Return NODE as a block of statements."
  (let ((body '())
        (node (jove-node-init)))
    (jove-expect jove-BRACE-L)
    (while (not (jove-eat jove-BRACE-R))
      (push (jove-parse-statement t) body))
    (jove-add-children* node (nreverse body))
    (jove-node-finish node 'block-statement)))

(defun jove-parse-for (node initializer)
  "Return NODE as a regular 'for' statement.
INITIALIZER is supplied by `jove-parse-statement'."
  (jove-add-child node initializer)
  (jove-expect jove-SEMI)
  (jove-add-child node (if (jove-is jove-SEMI)
                             (jove-null-expression)
                           (jove-parse-expression)))
  (jove-expect jove-SEMI)
  (jove-add-child node (if (jove-is jove-PAREN-R)
                             (jove-null-expression)
                           (jove-parse-expression)))
  (jove-expect jove-PAREN-R)
  (jove-add-child node (jove-parse-statement))
  (jove-node-finish node 'for-statement))

(defun jove-parse-for-in (node initializer)
  "Return NODE as either a 'for in' or 'for of' statement.
INITIALIZER is supplied by `jove-parse-statement'."
  (let ((type (if (jove-is jove-IN) 'for-in-statement 'for-of-statement)))
    (jove-next)
    (jove-add-children node
                         initializer
                         (prog1 (jove-parse-expression)
                           (jove-expect jove-PAREN-R))
                         (jove-parse-statement))
    (jove-node-finish node type)))


(defun jove-parse-var (node is-for kind)
  "Return NODE as a variable declarator.
Boolean flag IS-FOR indicates declarations are inside a 'for'
statement initializer. KIND should be a symbol of either 'var,
'let or 'const."
  ;; NOTE: Returned node is unfinished.
  (let ((collecting t)
        (decl nil)
        (decl-list '()))
    (jove-set-node-info node kind)
    (while collecting
      ;; Skip empty declarators.  Should allow for less errors
      ;; while editting.
      (unless (jove-eat jove-COMMA)
        (setq decl (jove-node-init))
        (jove-add-child decl (jove-parse-binding-atom t))
        (when (eq jove-EQ (jove-tt (jove-token)))
          (jove-next)              ; FIXME: not advancing!
          ;; Add initializer as child if present.
          (jove-add-child decl (jove-parse-maybe-assign is-for)))
        (push (jove-node-finish decl 'variable-declarator) decl-list)
        (unless (jove-eat jove-COMMA)
          (setq collecting nil))))
    (jove-add-children* node (nreverse decl-list))))

(defun jove-parse-function (node &optional is-stat allow-expr-body is-async)
  "Parse a function using the supplied NODE.
Depending on IS-STAT parse as declaration or literal.  The boolean flag
ALLOW-EXPR-BODY permits an expression body if parsing an arrow function.
The boolean flag IS-ASYNC is used to set the global `jove-in-async'."
  ;; With lexical scope the setting of global var in the `let'
  ;; should protect the global while in scope.
  (let ((old-in-function (jove-in-function))
        (old-in-generator (jove-in-generator))
        (old-in-async (jove-in-async)))

    (jove-set-in-function t)
    (jove-set-in-generator (and (not is-async) (jove-eat jove-STAR)))
    (jove-set-in-async is-async)

    (when (eq jove-NAME (jove-tt (jove-token)))
      (jove-set-face (jove-start (jove-token)) (jove-end (jove-token)) font-lock-function-name-face)
      (jove-set-node-info node (jove-value (jove-token)))
      (jove-next))

    (jove-parse-function-params node)
    (jove-parse-function-body node allow-expr-body)

    (jove-set-in-function old-in-function)
    (jove-set-in-generator old-in-generator)
    (jove-set-in-async old-in-async)

    (jove-node-finish node (if is-stat
                            'function-statement
                          'function-expression))))

(defun jove-parse-function-params (parent)
  "Parse function parameters and add as a child to PARENT."
  (let ((params (prog1 (jove-node-init)
                  (jove-expect jove-PAREN-L)))
        (param-list (jove-parse-binding-list jove-PAREN-R nil t)))
    (jove-node-finish (jove-add-children* params
                                         param-list)
                   'parameters)
    (jove-add-child parent params)))

(defun jove-parse-class (node &optional is-statement)
  "Return NODE as 'class' declaration or literal.
IF boolean flag IS-STATEMENT is non-nil parse as declaration."
  ;; TODO:
  )

(defun jove-parse-export (node)
  "Return NODE as 'export' declaration."
  ;; TODO:
  )

(defun jove-parse-import (node)
  "Return NODE as 'import' declaration."
  ;; TODO:
  )

(defun jove-parse ()
  "Run the Jove parser."
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (jove-config)
      (setq jove-ast (jove-node-init))
      (let ((start-pos (point))
            (start-time (float-time)))
        (jove-next)                         ; Load initial token
        (save-match-data
          (setq jove-ast (jove-parse-top-level jove-ast)))
        (when (and jove-verbose)
          (let ((time (/ (truncate (* (- (float-time) start-time)
                                      10000))
                         10000.0)))
            (message "Parser finished in %0.3fsec" time)))
        (jove-apply-fontifications start-pos (point))))))

(provide 'jove-parser)

;;; jove-parser.el ends here
