;;;; jove-parser.el --- The Jove Mode Parser -*- lexical-binding: t; -*-

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

(require 'jove-lexer)

;;; Buffer Local Variables

(defun jove-parser-state-make (&optional lex-state)
  "Return a vector representing a top level parser state.
If LEX-STATE not provided create an initial lexer state."
  ;; All location data is stored in the `lex-state'.
  (vector (or lex-state                 ; 0 token
              (jove-token-make))
          ;; Set prev-token to a dummy state, to ensure vector
          ;; functions don't accidently attempt to access nil.
          (jove-token-make)             ; 1 prev-token
          nil                           ; 2 in-function
          nil                           ; 3 in-async
          nil                           ; 4 in-generator
          nil                           ; 5 in-declaration
          nil                           ; 6 error-p
          -1))                          ; 7 potential-arrow-at

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
  "Return the 'in-async' slot of the parser state."
  (aref jove--state 3))
(defsubst jove-set-in-async (value)
  "Set the 'in-async' slot of the parser state to VALUE."
  (aset jove--state 3 value))

(defsubst jove-in-generator ()
  "Return the 'in-generator' slot of the parser state."
  (aref jove--state 4))
(defsubst jove-set-in-generator (value)
  "Set the 'in-generator' slot of the parser state to VALUE."
  (aset jove--state 4 value))

(defsubst jove-in-declaration ()
  "Return the 'in-declaration' slot of the parser state."
  (aref jove--state 5))
(defsubst jove-set-in-declaration (value)
  "Set the 'in-declaration' slot of the parser state to VALUE."
  (aset jove--state 5 value))

(defsubst jove-error ()
  "Return the 'error' slot of the parser state."
  (aref jove--state 6))
(defsubst jove-set-error (value)
  "Set the 'error' slot of the parser state to VALUE."
  (aset jove--state 6 value))

(defsubst jove-potential-arrow-at ()
  "Return the 'potential-arrow-at' slot of the parser state."
  (aref jove--state 7))
(defsubst jove-set-potential-arrow-at (value)
  "Set the 'potential-arrow-at' slot of the parser state to VALUE."
  (aset jove--state 7 value))

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

;; Node

(defun jove-node-make (&optional start end type)
  "Return a node.
If START is not supplied, its value will be set to start of the
current token.  Optionally supply any or all START, END, or TYPE."
  (vector 'jove-node                        ; 0
          (or start (jove-start (jove-token)))  ; 1
          end                           ; 2
          type                          ; 3
          nil                           ; 4 props
          nil                           ; 5 parent
          nil))                         ; 6 children

(defun jove-node-make* (token &optional type) ; Can be token or node
  "Return a node.
Use TOKEN data to set start and end location data.  Optionally
supply node TYPE."
  (vector 'jove-node                        ; 0
          (jove-start token)                ; 1
          (jove-end token)                  ; 2
          type                          ; 3
          nil                           ; 4 props
          nil                           ; 5 parent
          nil))                         ; 6 children

;; Data Getter and Setter Functions.

(defsubst jove-node-p (object)
  "Return t if OBJECT is a node."
  (and (vectorp object)
       (= 7 (length object))
       (eq 'jove-node (aref object 0))))

;; NOTE: `start', `end', and `type' work on tokens and nodes.

;;; Node Specific Functions.

(defsubst jove-props (node)
  "Return the 'props' slot of the NODE."
  (aref node 4))
(defsubst jove-get-prop (node property)
  "Get NODE information PROPERTY from the alist 'node-props'.
Uses `assq' for PROPERTY lookup."
  (car (cdr (assq property (aref node 4)))))
(defsubst jove-set-prop (node property value)
  "Set NODE information PROPERTY with VALUE into the alist 'node-props'."
  (setf (aref node 4)
        (cons (list property value) (jove-props node))))

(defsubst jove-parent (node)
  "Return the 'parent' slot of the NODE."
  (aref node 5))
(defsubst jove-set-parent (node parent)
  "Set the 'parent' slot of the NODE to PARENT."
  (aset node 5 parent))

(defsubst jove-children (node)
  "Return the 'children' slot of the NODE."
  (aref node 6))
(defsubst jove-set-children (node value)
  "Set the 'children' slot of the NODE to VALUE."
  (aset node 6 value))
(defsubst jove-pop-child (node)
  "Pop top child off the 'children' slot of NODE."
  (prog1 (car (aref node 6))
    (aset node 6 (cdr (aref node 6)))))
(defsubst jove-push-child (node child)
  "Push on top of NODE's 'children' slot CHILD."
  (aset node 6 (cons child (aref node 6))))

(defsubst jove-add-child (node child)
  "Add to NODE's 'children' slot CHILD."
  ;; NOTE: Children are built in reverse order, `nreverse' is
  ;; used in `jove-finish' to correct this.
  (when child
    (jove-set-parent child node)
    (jove-push-child node child))
  node)

(defun jove-add-children (node &rest child-args)
  "Add to NODE's 'children' slot each child in CHILD-ARGS."
  ;; NOTE: Children are built in reverse order, `nreverse' is
  ;; used in `jove-finish' to correct this.
  (let ((child nil)
        (children '()))
    (while child-args
      (setq child (car child-args)
            child-args (cdr child-args))
      (when child                       ; Protect from nil.
        (jove-set-parent child node)
        (setq children (cons child children))))
    (aset node 6 (append (aref node 6) children)))
  node)

(defun jove-finish (node &optional type)
  "Finish NODE of TYPE.
Uses position at the end of the previous token."
  ;; Reverse children.
  (aset node 6 (nreverse (aref node 6)))
  (when type (jove-set-type node type))
  ;; The parser will have already advanced to the next token,
  ;; which is why the `jove-end' of `(jove-prev-token)' is used.
  (jove-set-end node (jove-end (jove-prev-token)))
  node)

(defun jove-finish* (node &optional type)
  "Finish NODE of TYPE.
This function is for the rare occasion a node does not use the
`jove-prev-token' location data for its end location.  All the
information has previously been set, all that is left is to is
set node 'type' slot and `nreverse' its children."
  (aset node 6 (nreverse (aref node 6)))
  (when type (jove-set-type node type))
  node)

;;; Utility Functions

(defsubst jove-format-error (start end message)
  "Use START and END to prepend location data onto MESSAGE."
  (format "[%d,%d] Parse error: %s" start end message))

(cl-defun jove-signal (message &key start end type)
  "Signal an error with a MESSAGE.
If START or END not supplied, data from the current token is
used.  Unless the error TYPE is supplied throw a `jove-parse-error'."
  (let ((start (or start (jove-start (jove-token))))
        (end (or end (jove-end (jove-token)))))
    (jove-set-face start end 'js2-error)
    (signal (or type 'jove-parse-error)
            (list (jove-format-error start end message) start end))))

(defun jove-unexpected (&optional key)
  "Signal an unexpected token parse error.
If KEY is provided attempt to look up the message in `jove-messages'."
  (let ((message (gethash key jove-messages)))
    (jove-set-face* (jove-token) 'js2-error)
    (jove-signal (or message
                (format "unexpected token: %s" (jove-tt-label (jove-tt (jove-token))))))))

(defsubst jove-is (tt)
  "Return non-nil if the current token is of the type TT."
  (eq tt (jove-tt (jove-token))))

(defun jove-is* (&rest tts)
  "Return non-nil if the current token is one of the types TTS."
  (memq (jove-tt (jove-token)) tts))

(defsubst jove-is-not (tt)
  "Return non-nil if the current token is not of the type TT."
  (not (eq tt (jove-tt (jove-token)))))

(defun jove-eat (tt)
  "Return non-nil if the current token is of the type TT.
If the test passes consume the token as a side effect."
  (when (jove-is tt)
    (jove-next)
    t))

(defun jove-after-trailing-comma-p (tt &optional not-next)
  "Return non-nil if the current token is of the type TT.
Advance to next token, unless NOT-NEXT."
  ;; NOTE: Found in corn/src/parseutil.js
  (when (jove-is tt)
    (unless not-next
      (jove-next))
    t))

(defun jove-is-contextual (name &optional token)
  "Test whether current token is a contextual keyword NAME.
Optionally test against TOKEN if provided."
  (and (eq jove-NAME (jove-tt (or token (jove-token))))
       (string-equal name (jove-value (or token (jove-token))))))

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
  "Consume a semicolon or if allowed pretend one is there.
Return the value of the last sexp in BODY.  Though if unable to
eat a semicolon, flag next statement as junk."
  (unless (or (jove-eat jove-SEMI)
              (jove-can-insert-semicolon-p))
    (jove-set-error :cannot-insert-semi)))

(defun jove-null-expression (pos)
  "Create a null expression.
Used as a place holder is some type of nodes."
  (jove-node-make pos pos 'null-expression))

;;; Parse Error Handler

(defun jove-handle-error (node)
  "If `jove-error' and `jove-debug' message error and fontifiy NODE.
Return NODE.  Error message is printed to *Message* buffer.  And
region between `jove-start' and `jove-end' are
fontified with `js2-error' face."
  (when (jove-error)
    (when jove-debug
      (let ((error-message nil)
            (error-key (jove-error)))
        (cond
         ((and (eq :cannot-insert-semi error-key)
               (jove-is-contextual "await" (jove-prev-token)))
          (setq error-key :await-outside-async)))
        (setq error-message (or (gethash error-key jove-messages)
                                "unknown error"))
        ;; TODO: Push error to `jove--errors'.
        (message (jove-format-error (jove-start node)
                                (jove-end node)
                                error-message))
        (jove-set-face* node 'js2-error)))
    ;; When error set node property `:junk' to t.
    (jove-set-prop node :junk t)
    (jove-set-error nil))
  node)

;;; Helper Macros

(defmacro jove-expect (tt &rest recover)
  "Expect the current token to be of the type TT.
If the test passes consumed the token as a side effect, if the test fails
evaluate RECOVER if provided, otherwise throw an error."
  (declare (indent 1))
  `(if (eq ,tt (jove-tt (jove-token)))
       (jove-next)
     (if ,recover
         (progn ,@recover)
       (let ((expected (jove-tt-label ,tt))
          (found (if (jove-is jove-NAME)
                     (jove-value (jove-token))
                   (jove-tt-label (jove-tt (jove-token))))))
         (jove-signal (format "expected '%s' found '%s'" expected found))))))

(defmacro jove-parse-sequence (closing &rest body)
  "Loop while looking for CLOSING token type.
BODY is evaluated between `jove-COMMA' tokens, while allowing for
empty expressions and trailing commas.  NOTE: The CLOSING token
is not moved over."
  (declare (indent 1))
  `(progn
     ;; Execute body once before beginning the loop.
     (unless (memq (jove-tt (jove-token)) (list jove-COMMA ,closing))
       ,@body)
     (while (not (eq ,closing (jove-tt (jove-token))))
       (jove-expect jove-COMMA)
       (unless (or (jove-after-trailing-comma-p ,closing t)
                   (eq jove-COMMA (jove-token)))
         ,@body))))

;;; Expression Parsing Functions

(defun jove-parse-expression (&optional no-in)
  "Parse a full expression.
The boolean flag NO-IN forbids the 'in' operator."
  (let ((start-pos (jove-start (jove-token)))
        (expr (jove-parse-maybe-assign no-in)))
    (if (jove-is jove-COMMA)
        (let ((node (jove-node-make start-pos)))
          (jove-add-child node expr)
          (while (jove-eat jove-COMMA)
            (jove-add-child node (jove-parse-maybe-assign no-in)))
          (jove-finish node 'sequence-expression))
      expr)))

(defun jove-parse-maybe-assign (&optional no-in)
  "Maybe parse an assignment expression.
The boolean flag NO-IN forbids the 'in' operator."
  (if (and (jove-in-generator) (jove-is-contextual "yield"))
      (jove-parse-yield)
    (when (or (jove-is jove-PAREN-L)
              (jove-is jove-NAME))
      (jove-set-potential-arrow-at (jove-start (jove-token))))
    (let ((start-pos (jove-start (jove-token)))
          (left (jove-parse-maybe-conditional no-in)))
      ;; Find where afterLeftParse is used.
      (if (jove-tt-is-assign (jove-tt (jove-token)))
          (let ((node (jove-node-make start-pos)))
            (jove-set-prop node :op (jove-value (jove-token)))
            (jove-add-child node (if (jove-is jove-EQ)
                                 (jove-to-assignable left)
                               left))
            (jove-next)                     ; Move over the operator.
            (jove-add-child node (jove-parse-maybe-assign no-in))
            (jove-finish node 'assign-expression))
        left))))

(defun jove-parse-maybe-conditional (&optional no-in)
  "Maybe parse a ternary conditional expression.
The boolean flag NO-IN forbids the 'in' operator."
  (let ((start-pos (jove-start (jove-token)))
        (expr (jove-parse-expr-ops no-in)))
    (if (jove-is jove-QUESTION)
        (let ((node (jove-node-make start-pos)))
          (jove-set-prop node :mark (jove-start (jove-token)))
          (jove-next)                       ; Move over '?'
          (jove-add-children node
                         expr
                         (jove-parse-maybe-assign) ; no NO-IN here???
                         (progn
                           (jove-expect jove-COLON)
                           (jove-parse-maybe-assign no-in)))
          (jove-finish node 'conditional-expression))
      expr)))

(defun jove-parse-expr-ops (no-in)
  "Start the precedence parser.
If current expression is an ArrowFunctionExpression, just return
the expression.  The boolean flag NO-IN forbids the 'in' operator."
  (let ((start-pos (jove-start (jove-token)))
        (expr (jove-parse-maybe-unary nil)))
    (if (and (= start-pos (jove-start expr))
             (eq 'arrow-function-expression (jove-type expr)))
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
                       (jove-is-not jove-IN))
                   (jove-tt-binop (jove-tt (jove-token))))))
    (if (and prec (> prec min-prec))
        (let ((logical (or (jove-is jove-LOGICAL-OR)
                           (jove-is jove-LOGICAL-AND)))
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
  ;; Putting operator string value into the info slot.
  (let ((node (jove-node-make start-pos)))
    (jove-set-prop node :op op)
    (jove-add-children node left right)
    (jove-finish node (if logical 'logical-expression 'binary-expression))))

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
      (let ((node (jove-node-make))
            (update (jove-is jove-INC-DEC)))
        (jove-set-prop node :prefix t)
        (jove-set-prop node :op (jove-value (jove-token)))
        (jove-next)
        (jove-add-child node (jove-parse-maybe-unary t))
        (setq expr (jove-finish node (if update
                                          'update-expression
                                        'unary-expression)))
        (unless update (setq saw-unary t))))
     (t
      (setq expr (jove-parse-expr-subscripts))
      (while (and (jove-tt-postfix (jove-tt (jove-token)))
                  (not (jove-can-insert-semicolon-p)))
        (let ((node (jove-node-make start-pos)))
          (jove-set-prop node :postfix t)
          (jove-add-child node expr)
          (jove-set-prop node :op (jove-value (jove-token)))
          (jove-next)
          (setq expr (jove-finish node 'update-expression))))))
    (if (and (not saw-unary)
             (jove-is jove-STARSTAR))
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
        (maybe-async-arrow (and (eq 'identifier (jove-type base))
                                (string-equal "async" (jove-get-prop base :value))
                                (= (jove-end (jove-prev-token)) (jove-end base))
                                (not (jove-can-insert-semicolon-p)))))
    (catch 'node
      (while t
        (cond
         ;; Member Expression
         ((or (setq computed (jove-eat jove-BRACKET-L))
              (jove-eat jove-DOT))
          (let ((node (jove-node-make start-pos)))
            (when computed
              (jove-set-prop node :computed t))
            (jove-add-children node
                           base
                           (if computed
                               (jove-parse-expression)
                             (jove-parse-identifier t)))
            (if computed
                (jove-expect jove-BRACKET-R)
              (jove-set-face* (jove-prev-token) 'js2-object-property))
            (setq base (jove-finish node 'member-expression))))
         ;; Call Expression
         ((and (not no-calls)
               (jove-is jove-PAREN-L))
          (let ((id (jove-prev-token))   ; Save for possibly highlighting.
                (exprs (jove-parse-expr-list (prog1 (jove-node-make)
                                               (jove-next)) ; Move over '('
                                             jove-PAREN-R)))
            (jove-set-end exprs (jove-end (jove-prev-token)))
            ;; 'exprs' is not finished like most nodes.  It is almost
            ;; complete, only the 'type' slot remains to be set.
            (if (and maybe-async-arrow
                     (not (jove-can-insert-semicolon-p))
                     (jove-eat jove-ARROW))
                (let ((old-in-declaration (jove-in-declaration)))
                  (jove-set-in-declaration t)
                  (dolist (child (jove-children exprs))
                    (jove-to-assignable child))
                  (jove-set-in-declaration old-in-declaration)
                  (jove-set-type exprs 'parameters)
                  ;; Highlight 'async' as a keyword.
                  (jove-set-face* id 'font-lock-keyword-face)
                  (throw 'node (jove-parse-arrow-expr (jove-node-make start-pos)
                                                  exprs
                                                  t)))
              (let ((node (jove-node-make start-pos)))
                (jove-set-type exprs 'arguments)
                ;; Highlight 'id' as a call.
                (when (eq jove-NAME (jove-tt id))
                  (jove-set-face* id 'js2-function-call))
                (jove-add-children node base exprs)
                (jove-finish node 'call-expression)
                (setq base node)))))
         ;; Tagged Template Expression
         ((jove-is jove-BACKQUOTE)
          (jove-finish (jove-add-children (jove-node-make start-pos)
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
      (prog1 (jove-node-make (jove-start (jove-token))
                         (jove-end (jove-token))
                         'super)
        (jove-next)))
     ((eq jove-THIS type)
      (prog1 (jove-node-make (jove-start (jove-token))
                         (jove-end (jove-token))
                         'this-expression)
        (jove-next)))
     ((eq jove-NAME type)
      (let ((start-pos (jove-start (jove-token)))
            (id (jove-parse-identifier nil)))
        (cond
         ((and (string-equal "async" (jove-get-prop id :value))
               (not (jove-can-insert-semicolon-p))
               (jove-is jove-FUNCTION))
          ;; Highlight 'async' as keyword.
          (jove-set-face* (jove-prev-token) font-lock-keyword-face)
          (jove-next)                       ; Move over 'function'
          ;; Parse async function.
          (jove-parse-function (jove-node-make start-pos) nil nil t))
         ((and can-be-arrow
               (not (jove-can-insert-semicolon-p)))
          (cond
           ((jove-eat jove-ARROW)
            ;; Wrap the single parameter in a 'parameters' node.
            (let ((params (jove-node-make* id 'parameters)))
              ;; Highlight node 'id' as a parameter.
              (jove-set-face* id 'font-lock-variable-name-face)
              (jove-add-child params id)
              (jove-parse-arrow-expr (jove-node-make start-pos) params nil)))
           ((and (string-equal "async" (jove-get-prop id :value))
                 (jove-is jove-NAME))
           ;; Highlight 'async' as keyword.
           (jove-set-face* id font-lock-keyword-face)
           (setq id (jove-parse-identifier))
           ;; Highlight node 'id' as a parameter.
           (jove-set-face* id font-lock-variable-name-face)
           (if (or (jove-can-insert-semicolon-p)
                   (not (jove-eat jove-ARROW)))
               (jove-unexpected)
             (let ((params (jove-node-make (jove-start id) (jove-end id) 'parameters)))
               ;; Wrap the single parameter in a 'parameters' node.
               (jove-add-child params id)
               (jove-parse-arrow-expr (jove-node-make start-pos params t) params t))))
          (t           ; Still need to return ID if none of the above match.
           id)))
        (t
          id))))
     ((memq type (list jove-REGEXP jove-STRING jove-NUM jove-NULL jove-UNDEFINED jove-TRUE jove-FALSE))
      (prog1 (jove-node-make (jove-start (jove-token))
                         (jove-end (jove-token))
                         'literal)
        (jove-next)))
     ((eq jove-PAREN-L type)
      (jove-parse-paren-expr can-be-arrow))
     ((eq jove-BRACKET-L type)
      (jove-finish (jove-parse-expr-list (prog1 (jove-node-make)
                                   (jove-next)) ; Move over '['
                                 jove-BRACKET-R)
               'array-expression))
     ((eq jove-BRACE-L type)
      (jove-parse-object nil))
     ((eq jove-FUNCTION type)
      (jove-parse-function (prog1 (jove-node-make) (jove-next)) nil))
     ((eq jove-CLASS type)
      (jove-parse-class (jove-node-make) nil))
     ((eq jove-NEW type)
      (jove-parse-new))
     ((eq jove-BACKQUOTE type)
      (jove-parse-template))
     (t
      (jove-unexpected)))))

(defun jove-parse-paren-expr (&optional can-be-arrow)
  "Parse a parenthesized expression.
As of ECMAScript 6, possiblities are SequenceExpression or
ArrowExpression.  ArrowExpressions are allowed when the boolean
flag CAN-BE-ARROW is non-nil.  A sequence is wrapped in
ParenthesizedExpression.  Trailing commas and empty expressions
are allowed."
  (let ((node (jove-node-make))
        (start-pos (jove-start (jove-token)))
        (inner-end-pos nil)
        (inner-start-pos (progn (jove-next) (jove-start (jove-token)))))
    (jove-parse-sequence jove-PAREN-R
      (jove-add-child node (if (jove-is jove-ELLIPSIS)
                           (jove-parse-rest)
                         (jove-parse-maybe-assign nil))))
    (setq inner-end-pos (jove-start (jove-token)))
    (jove-expect jove-PAREN-R)
    ;; Return a ArrowFunctionExpression or ParenthesizedExpression.
    (if (and can-be-arrow
             (not (jove-can-insert-semicolon-p))
             (jove-is jove-ARROW))
        (let ((old-in-declaration (jove-in-declaration)))

          (jove-finish node 'parameters)
          (jove-set-in-declaration t)
          (jove-to-assignable (jove-children node))
          (jove-set-in-declaration old-in-declaration)
          
          (jove-next)                         ; Move over '=>'
          (jove-parse-arrow-expr (jove-node-make start-pos) node))
      ;; If more than one expression in 'expr-list', wrap them in a
      ;; SequenceExpression.
      (if (< 1 (length (jove-children node)))
          (progn
            (jove-set-start node inner-start-pos)
            (jove-set-end node inner-end-pos)
            (jove-finish* node 'sequence-expression))
        ;; If node is nil, nothing will be added to the paren expression.
        (setq node (car (jove-children node))))
      ;; Wrap in a ParenthesizedExpression.
      (jove-finish (jove-add-child (jove-node-make start-pos)
                           node)
               'parenthesized-expression))))

(defun jove-parse-new ()
  "Parse a new expression."
  (let ((node (jove-node-make))
        (meta (jove-parse-identifier t)))
    (if (jove-eat jove-DOT)
        (jove-finish (jove-add-children node
                                meta
                                (jove-parse-identifier t))
                 'meta-property)
      (let ((start-pos (jove-start (jove-token))))
        (jove-add-child node (jove-parse-subscripts (jove-parse-expr-atom)
                                            start-pos
                                            t))
        (when (jove-is jove-PAREN-L)
          (jove-add-child node (jove-finish (jove-parse-expr-list (prog1 (jove-node-make)
                                                        (jove-next))
                                                      jove-PAREN-R)
                                    'arguments)))
        (jove-finish node 'new-expression)))))

(defun jove-parse-template-element ()
  "Parse template element."
  (let ((node (jove-node-make)))
    ;; FIXME: If the template is empty it will skip over the closing
    ;; quasi.
    (jove-next)
    (when (jove-is jove-BACKQUOTE)
      (jove-set-prop node :tail t))
    (jove-finish node 'template-element)))

(defun jove-parse-template ()
  "Parse template literal."
  (let ((elt nil)
        (node (jove-node-make)))
    (jove-next)                             ; Move over '`'
    (jove-add-child node (setq elt (jove-parse-template-element)))
    (while (not (jove-get-prop elt :tail))
      (jove-expect jove-DOLLAR-BRACE-L)
      (jove-add-child node (jove-parse-expression))
      (jove-expect jove-BRACE-R)
      (jove-add-child node (setq elt (jove-parse-template-element))))
    (jove-next)                             ; Move over '`'
    (jove-finish node 'template-literal)))

(defun jove-parse-object (&optional is-pattern)
  "Parse an object literal or binding pattern.
Optionally if IS-PATTERN, parse as an ObjectPattern."
  (let ((start-pos nil)
        (prop nil)
        (is-async nil)
        (is-generator nil)
        (node (jove-node-make)))
    (jove-next)                             ; Move over '{'
    (jove-parse-sequence jove-BRACE-R
      (setq prop (jove-node-make))
      (when (not is-pattern)
        (setq is-generator (jove-eat jove-STAR)))
      (setq start-pos (jove-start (jove-token)))
      (jove-parse-property-name prop is-pattern)
        ;; FIXME: Clean up this mess.
      (when (and (not is-pattern)
                 (not is-generator)
                 (not (jove-get-prop prop :computed))
                 ;; FIXME: If car is nil this will error.
                 (eq 'identifier (jove-type (car (jove-children prop))))
                 (string-equal "async" (jove-get-prop (car (jove-children prop)) :value))
                 (not (jove-is jove-PAREN-L))
                 (not (jove-is jove-COLON)))
        (setq is-async t)
        ;; FIXME: Highligh face of 'async' as a keyword.
        (jove-set-children prop '())        ; Clear children.
        (jove-parse-property-name prop is-pattern))
      (jove-parse-property-value prop is-pattern is-generator is-async start-pos)
      (jove-add-child node (jove-finish prop 'property)))
    (jove-next)                             ; Move over '}'
    (jove-finish node (if is-pattern 'object-pattern 'object-expression))))

;; TODO: Need a better way to flag different node options.
(defun jove-parse-property-value (prop is-pattern is-generator is-async start-pos)
  "Parse object property value.
Add as a child to the node PROP.  Boolean flags IS-PATTERN,
IS-GENERATOR, and IS-ASYNC are used to parse the property value
according to context.  START-POS is the position at the beginning
of the property."
  (when (and (or is-generator is-async)
             (jove-is jove-COLON))
    (jove-unexpected))
  (cond
   ((jove-is jove-COLON)
    (jove-set-face* (jove-prev-token) 'js2-object-property)
    (jove-next)                             ; Move over ':'
    (jove-add-child prop (if is-pattern
                         (jove-parse-maybe-default start-pos)
                       (jove-parse-maybe-assign))))
   ((jove-is jove-PAREN-L)
    (when is-pattern
      (jove-unexpected))
    (jove-add-child prop (jove-parse-method is-generator is-async)))
   ((and (not (jove-get-prop prop :computed))
         (eq 'identifier (jove-type (car (jove-children prop))))
         ;; FIXME: Need a better way to access the key.
         (or (string-equal "get" (jove-get-prop (car (jove-children prop)) :value))
             (string-equal "set" (jove-get-prop (car (jove-children prop)) :value)))
         (jove-is-not jove-COMMA)
         (jove-is-not jove-BRACE-R))
    (when (or is-generator is-async is-pattern)
      (jove-unexpected))
    ;; Highlight the contextual 'get' or 'set' as a keyword.
    (jove-set-face* (car (jove-children prop)) 'font-lock-keyword-face)
    ;; FIXME: The identifier for the 'get' or 'set' shouldn't be
    ;; higlighted as a method.
    (jove-set-children prop '())
    (jove-parse-property-name prop)
    ;; Raise no warnings about getter or setter arguments.
    (jove-add-child prop (jove-parse-method)))
   ((and (not (jove-get-prop prop :computed))
         (eq 'identifier (jove-type (car (jove-children prop)))))
    (let ((id (jove-node-make (jove-start (jove-prev-token))
                          (jove-end (jove-prev-token))
                          'identifier)))
      (jove-set-prop id :value (jove-value (jove-prev-token)))
      (when (or is-pattern
            (jove-is jove-EQ)))
      (jove-add-child prop (jove-parse-maybe-default start-pos id))))
   (t
    (jove-unexpected)))
  ;; Maybe this is a good place to highlight the property name.
  ;; We should have all the information on if it is a normal
  ;; property, getter, setter, or method.
  )

(defun jove-parse-property-name (prop &optional is-pattern)
  "Parse object property name.
Add the parsed node to the node PROP.  The boolean IS-PATTERN
flags whether parsing an object pattern.  Add as a child to the
node PROP."
  (if (jove-eat jove-BRACKET-L)
      (progn
        (jove-set-prop prop :computed t)
        (jove-add-child prop (jove-parse-maybe-assign))
        (jove-expect jove-BRACKET-R))
    (if (or (jove-is jove-NUM)
            (jove-is jove-STRING))
        (jove-add-child prop (jove-parse-expr-atom))
      (when is-pattern
        (jove-set-face* (jove-token) font-lock-variable-name-face))
      (jove-add-child prop (jove-parse-identifier t)))))

(defun jove-parse-method (&optional is-generator is-async)
  "Parse an object or class method.
Boolean flags IS-GENERATOR and IS-ASYNC set the global variables
`jove-in-generator' and `jove--inasync'."
  (let ((node (jove-node-make))
        (prev-token (jove-prev-token))
        (old-in-function (jove-in-function))
        (old-in-generator (jove-in-generator))
        (old-in-async (jove-in-async))
        (old-in-declaration (jove-in-declaration))
        (params (prog1 (jove-node-make)
                  (jove-expect jove-PAREN-L))))

    ;; FIXME: Wouldn't previous token be the left paren?
    (when (eq jove-NAME (jove-tt prev-token))
      (jove-set-face* prev-token font-lock-function-name-face))

    (jove-set-in-function t)
    (jove-set-in-generator is-generator)
    (jove-set-in-async is-async)
    (jove-set-in-declaration t)

    (jove-add-child node (jove-finish (jove-parse-binding-list params
                                                   jove-PAREN-R)
                              'parameters))
    
    (jove-set-in-declaration old-in-declaration)

    (jove-parse-function-body node)

    (jove-set-in-function old-in-function)
    (jove-set-in-generator old-in-generator)
    (jove-set-in-async old-in-async)

    (jove-finish node 'function-expression)))

(defun jove-parse-arrow-expr (node params &optional is-async)
  "Parse arrow function using supplied NODE with given PARAMS.
Boolean flag IS-ASYNC sets the global variable `jove--in-asynce'."
  (let ((old-in-function (jove-in-function))
        (old-in-generator (jove-in-generator))
        (old-in-async (jove-in-async))
        (old-in-declaration (jove-in-declaration)))

    ;; NOTE: Arguments have already been highlighted.
    (jove-set-in-function t)
    (jove-set-in-generator nil)
    (jove-set-in-async is-async)
    (jove-set-in-declaration t)
    
    (when is-async
      (jove-set-prop node :async t))
    
    (dolist (param (jove-children params))
      (jove-to-assignable param))

    (jove-set-in-declaration old-in-declaration)
    
    (jove-add-child node params)

    (jove-parse-function-body node t)

    (jove-set-in-function old-in-function)
    (jove-set-in-generator old-in-generator)
    (jove-set-in-async old-in-async)

    (jove-finish node 'arrow-function-expression)))

(defun jove-parse-function-body (parent &optional is-arrow-func)
  "Parse function body and add as child to PARENT.
If IS-ARROW-FUNC is non-nil, possibly parse body as an expression,
unless the body is contained in a brace block."
  (if (and is-arrow-func
           (not (eq jove-BRACE-L (jove-tt (jove-token)))))
      (jove-add-child parent (jove-parse-maybe-assign))
    (jove-add-child parent (jove-parse-block))))

;; TODO: Combine `jove-parse-expr-list' and `jove-parse-binding-list'
(defun jove-parse-expr-list (node closing)
  "Parse a comma seperated list of expressions.
Return NODE with parsed expressions added as children.  Advance
parser over CLOSING, the token type which ends the list.  Always
allow empty expressions and trailing commas."
  (jove-parse-sequence closing
    (jove-add-child node (if (jove-is jove-ELLIPSIS)
                         (jove-parse-spread)
                       (jove-parse-maybe-assign))))
  (jove-next)                               ; Move over CLOSING.
  node)                                 ; Return NODE.

(defun jove-parse-identifier (&optional liberal)
  "Parse token as an identifier.
If LIBERAL is non-nil keywords are converted to identifiers."
  (let ((node (jove-node-make)))
    (cond
     ((jove-is jove-NAME)
      (when (and (jove-in-generator)
                 (string-equal "yield" (jove-value (jove-token))))
        (jove-warn (jove-start (jove-token)) (jove-end (jove-token))
               "'yield' used as an identifier inside a generator"))
      (when (and (jove-in-async)
                 (string-equal "await" (jove-value (jove-token))))
        (jove-warn (jove-start (jove-token)) (jove-end (jove-token))
               "'await' used as an identifier inside an async function"))
      (jove-set-prop node :value (jove-value (jove-token))))
     ((and liberal
           (jove-tt-keyword (jove-tt (jove-token))))
      (jove-set-prop node :value (jove-tt-label (jove-tt (jove-token)))))
     (t
      (jove-unexpected)))
    (jove-next)
    (jove-finish node 'identifier)))

(defun jove-parse-yield ()
  "Parse a yield expression."
  (let ((node (jove-node-make)))
    (jove-set-face* (jove-token) 'font-lock-keyword-face)
    (jove-next)
    (unless (or (or (jove-is jove-SEMI)
                    (jove-can-insert-semicolon-p))
                (and (jove-is-not jove-STAR)
                     (not (jove-tt-starts-expr (jove-token)))))
      (when (jove-is jove-STAR)
        (jove-set-prop node :delegate t)
        (jove-set-face* (jove-token) 'font-lock-keyword-face)
        (jove-next))                        ; Move over '*'
      (jove-add-child node (jove-parse-maybe-assign)))
    (jove-finish node 'yield-expression)))

(defun jove-parse-await ()
  "Parse an await expression."
  (let ((node (jove-node-make)))
    (jove-set-face* (jove-token) 'font-lock-keyword-face)
    (jove-next)
    (jove-add-child node (jove-parse-maybe-unary nil))
    (jove-finish node 'await-expression)))

;;; Left Value Functions

;; FIXME: Needs to work with new vector node.
(defun jove-to-assignable (object)
  "Convert existing expression OBJECT to assignable pattern.
Updates node type, does not perform check for valid lvalues."
  (if (consp object)                    ; Non-nil lists.
      (dolist (item object)
        (jove-to-assignable item))
    (when (jove-node-p object)
      (let ((type (jove-type object)))
        (cond
         ((and (jove-in-declaration)
               (eq 'identifier type))
          (jove-set-face* object 'font-lock-variable-name-face))
         ((eq 'object-expression type)
          (jove-set-type object 'object-pattern)
          (dolist (prop (jove-children object))
            ;; Second item in the list of children should be the
            ;; value of the property.
            (jove-to-assignable (cadr (jove-children prop)))))
         ((eq 'array-expression type)
          (jove-set-type object 'array-pattern)
          (dolist (expr (jove-children object))
            (if (eq 'spread-element (jove-type expr))
                (progn
                  (jove-set-type expr 'rest-element)
                  (jove-to-assignable (car (jove-children expr))))
              (jove-to-assignable expr))))
         ((eq 'assignment-expression type)
          (jove-set-type object 'assign-pattern)
          ;; First child is 'left' operand.
          (jove-to-assignable (car (jove-children object))))
         ((eq 'parenthesized-expression type)
          (jove-to-assignable (car (jove-children object))))))))
  object)                               ; Return the node.

(defun jove-parse-spread ()
  "Parse spread element."
  (jove-finish (jove-add-child (prog1 (jove-node-make)
                         (jove-next))
                       (jove-parse-maybe-assign nil))
           'spread-element))

(defun jove-parse-rest (&optional allow-non-identifier)
  "Parse rest element.
Optionally ALLOW-NON-IDENTIFIER arguments."
  (jove-finish (jove-add-child (prog1 (jove-node-make)
                         (jove-next))
                       (if allow-non-identifier
                           (if (jove-is jove-NAME)
                               (jove-parse-identifier)
                             (jove-unexpected))
                         (if (or (jove-is jove-NAME)
                                 (jove-is jove-BRACKET-L))
                             (jove-parse-binding-atom)
                           (jove-unexpected))))
           'rest-element))

(defun jove-parse-binding-atom ()
  "Parse assignable atom.
The boolean HIGHLIGHT flags to set variable name face."
  (cond
   ((jove-is jove-NAME)
    (when (jove-in-declaration)
      (jove-set-face* (jove-token) 'font-lock-variable-name-face))
    (jove-parse-identifier))
   ((jove-is jove-BRACKET-L)
    ;; `jove-parse-binding-list' returns a regular list. The items need to
    ;; be added as children to the node.
    (jove-finish (jove-parse-binding-list (prog1 (jove-node-make)
                                    (jove-next))
                                  jove-BRACKET-R)
             'array-pattern))
   ((jove-is jove-BRACE-L)
    (jove-parse-object t))
   (t
    (jove-unexpected))))

;; This function is used to parse function and method parameters.
(defun jove-parse-binding-list (node closing  &optional allow-non-identifier)
  "Parse assignable list.
Return NODE with parsed expressions added as children.  Move over
CLOSING, the token type which ends the list.  Always allow empty
expressions and trailing commas.  Optionally ALLOW-NON-IDENTIFIER
arguments to RestElement."
  (jove-parse-sequence closing
    (jove-add-child node (if (jove-is jove-ELLIPSIS)
                         (jove-parse-rest allow-non-identifier)
                       (jove-parse-maybe-default (jove-start (jove-token)) nil))))
  (jove-next)                               ; Move over CLOSING.
  node)                                 ; Return NODE.

(defun jove-parse-maybe-default (start-pos &optional left)
  "Parse assignment pattern around a given atom if possible.
Begin the AssignmentPattern at START-POS.  Optionally supply LEFT
operand.  The boolean HIGHLIGHT flags to set variable name face."
  (setq left (or left (jove-parse-binding-atom)))
  (if (jove-is jove-EQ)
      (let ((node (jove-node-make start-pos)))
        (jove-set-prop node :op (jove-value (jove-token)))
        (jove-next)                         ; Move over '='
        (jove-finish (jove-add-children node
                                left
                                (jove-parse-maybe-assign))
                 'assignment-pattern))
    left))

;;; Statement Parsing Functions

(defun jove-is-let ()
  "Return non-nil if looking at a 'let'."
  (and (jove-is-contextual "let")
       (let ((type (jove-tt (jove-peek))))
         (or (eq jove-BRACE-L type)
             (eq jove-BRACKET-L type)
             (eq jove-NAME type)))))

(defun jove-is-function ()
  "Return non-nil if looking at a function."
  (or (jove-is jove-FUNCTION)
      (jove-is-async-function)))

(defun jove-is-async-function ()
  "Return non-nil if looking at an 'async' function."
  (and (jove-is-contextual "async")
       (let ((next (jove-peek)))
         (and (not (jove-newline-before next))
              (eq jove-FUNCTION (jove-tt next))))))

(defun jove-parse-top-level (node)
  "Parse a program.
Add top level statements as children to NODE."
  (condition-case err
      (while (jove-is-not jove-EOF)
        (jove-add-child node (jove-parse-statement t)))
    (jove-parse-error
     (when jove-debug (message "%s" (cadr err)))))
  (jove-next)                             ; Move over EOF.
  (jove-finish node 'program))

(defun jove-parse-statement (&optional declaration)
  "Parse a single statement.
The boolean DECLARATION flags to permit a function or class
declaration."
  (let ((kind nil)
        (tt (jove-tt (jove-token)))
        (node (jove-node-make)))
    (when (jove-is-let)
      (setq tt jove-VAR)
      (setq kind 'let)
      (jove-set-face* (jove-token) 'font-lock-keyword-face))
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
     ((eq jove-BRACE-L tt) (jove-parse-block node))
     ((eq jove-SEMI tt) (jove-parse-empty-statement node))
     ((eq jove-IMPORT tt) (jove-parse-import node))
     ((eq jove-EXPORT tt) (jove-parse-export node))
     ((and declaration
           (jove-is-async-function))
      ;; Highlight 'async' as keyword.
      (jove-set-face* (jove-token) 'font-lock-keyword-face)
      (jove-next)
      (jove-parse-function-statement node t))
     (t
      (let ((maybe-name (jove-value (jove-token)))
            (expr (jove-parse-expression)))
        (if (and (eq jove-NAME tt)
                 (eq 'identifier (jove-type expr))
                 (jove-eat jove-COLON))
            (jove-parse-labeled-statement node maybe-name expr)
          (jove-parse-expression-statement node expr)))))
    ;; Handle error if one exists, either way return NODE.
    (if (jove-error)
        (jove-handle-error node)
      node)))

(defun jove-parse-break-continue-statement (node tt)
  "Return NODE as either a 'break' or 'continue' statement.
Differentiate between the two using the token type TT."
  (jove-next)
  (unless (or (jove-is jove-SEMI)
              (jove-can-insert-semicolon-p))
    (if (jove-is-not jove-NAME)
        (jove-unexpected)
      (jove-add-child node (jove-parse-identifier))))
  (jove-semicolon)
  (jove-finish node (if (eq jove-BREAK tt)
                    'break-statement
                  'continue-statement)))

(defun jove-parse-debugger-statement (node)
  "Return NODE as a 'debugger' statement."
  (jove-next)
  (jove-semicolon)
  (jove-finish node 'debugger-statement))

(defun jove-parse-do-statement (node)
  "Return NODE as a 'do' statement."
  (jove-next)
  (jove-add-child node (jove-parse-statement))
  (jove-expect jove-WHILE)
  (jove-add-child node (jove-parse-paren-expr))
  (jove-eat jove-SEMI)
  (jove-finish node 'do-while-statement))

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
      (let ((init (jove-node-make))
            (kind (if is-let 'let (intern (jove-value (jove-token))))))
        (jove-next)
        (jove-parse-var init t kind)
        (jove-finish init 'variable-declaration)
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
            ;; FIXME: `jove-to-assignable' should handed a list of expression?
            (jove-parse-for-in node (jove-to-assignable init))
          (jove-parse-for node init)))))))

(defun jove-parse-function-statement (node &optional is-async)
  "Return NODE as a 'function' statement.
The boolean IS-ASYNC flags an async function."
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
  (jove-finish node 'if-statement))

(defun jove-parse-return-statement (node)
  "Return NODE as a 'return' statement."
  (jove-next)
  (unless (or (jove-is jove-SEMI)
              (jove-can-insert-semicolon-p))
    (jove-add-child node (jove-parse-expression)))
  (jove-semicolon)
  (jove-finish node 'return-statement))

(defun jove-parse-switch-statement (node)
  "Return NODE as a 'switch' statement."
  (jove-next)
  (jove-add-child node (jove-parse-paren-expr))
  (let (current                         ; Current switch case.
        is-case)
    (jove-expect jove-BRACE-L)
    (while (jove-is-not jove-BRACE-R)
      (if (or (setq is-case (jove-is jove-CASE))
              (jove-is jove-DEFAULT))
          (progn
            (when current (jove-finish current 'switch-case))
            (jove-add-child node (setq current (jove-node-make)))
            (jove-next)
            (if is-case
                (jove-add-child current (jove-parse-expression))
              (jove-add-child current (jove-null-expression (jove-end (jove-token)))))
            (jove-expect jove-COLON))
        (if (not current)
            (jove-unexpected)
          (jove-add-child current (jove-parse-statement t)))))
    (when current (jove-finish current 'switch-case))
    (jove-next)                             ; Move over '}'
    (jove-finish node 'switch-statement)))

(defun jove-parse-throw-statement (node)
  "Return NODE as a 'throw' statement."
  (jove-next)
  ;; A newline between the 'throw' keyword and the expression
  ;; is illegal.  Though I assume if it isn't there it just
  ;; hasn't been typed yet, so fill in with a null expression.
  (jove-add-child node (if (jove-newline-before (jove-token))
                       (jove-null-expression (jove-end (jove-prev-token)))
                     (jove-parse-expression)))
  (jove-semicolon)
  (jove-finish node 'throw-statement))

(defun jove-parse-try-statement (node)
  "Return NODE as a 'try' statement."
  (jove-next)
  (jove-add-child node (jove-parse-block))
  (when (jove-is jove-CATCH)
    (let ((clause (jove-node-make)))
      (jove-next)
      (jove-expect jove-PAREN-L)
      (jove-add-child clause (jove-parse-binding-atom))
      (jove-expect jove-PAREN-R)
      (jove-add-child clause (jove-parse-block))
      (jove-add-child node (jove-finish clause 'catch-clause))))
  (when (jove-eat jove-FINALLY)
    (jove-add-child node (jove-parse-block)))
  (jove-finish node 'try-statement))

(defun jove-parse-var-statement (node kind)
  "Return NODE as a 'var' statement.
KIND should be a symbol of either 'var, 'let or 'const."
  (jove-next)
  (jove-parse-var node nil kind)
  (jove-semicolon)
  (jove-finish node 'variable-declaration))

(defun jove-parse-while-statement (node)
  "Return NODE as a 'while' statement."
  (jove-next)
  (jove-add-children node
                 (jove-parse-paren-expr)
                 (jove-parse-statement nil))
  (jove-finish node 'while-statement))

(defun jove-parse-with-statement (node)
  "Return NODE as a 'with' statement."
  (jove-next)
  (jove-add-children node
                 (jove-parse-paren-expr)
                 (jove-parse-statement nil))
  (jove-finish node 'with-statement))

(defun jove-parse-empty-statement (node)
  "Return NODE as a empty statement."
  ;; TODO: Double check the positions returned.
  (jove-next)
  (jove-finish node 'empty-statement))

(defun jove-parse-labeled-statement (node maybe-name expr)
  "Return NODE as a labeled statement."
  (jove-add-children node
                 expr
                 (jove-parse-statement t))
  (jove-finish node 'labeled-statement))

(defun jove-parse-expression-statement (node expression)
  "Return NODE as an expression statement.
EXPRESSION is supplied by `jove-parse-statement'."
  (jove-add-child node expression)
  (jove-semicolon)
  (jove-finish node 'expression-statement))

(defun jove-parse-block (&optional node)
  "Return NODE as a block of statements."
  (unless (jove-node-p node)
    (setq node (jove-node-make)))
  (jove-expect jove-BRACE-L)
  (while (not (jove-eat jove-BRACE-R))
    (jove-add-child node (jove-parse-statement t)))
  (jove-finish node 'block-statement))

(defun jove-parse-for (node initializer)
  "Return NODE as a regular 'for' statement.
INITIALIZER is supplied by `jove-parse-statement'."
  (jove-add-child node initializer)
  (jove-expect jove-SEMI)

  (jove-add-child node (if (jove-is jove-SEMI)
                       (jove-null-expression (jove-end (jove-token)))
                     (jove-parse-expression)))
  
  (jove-expect jove-SEMI)
  (jove-add-child node (if (jove-is jove-PAREN-R)
                       (jove-null-expression (jove-end (jove-token)))
                     (jove-parse-expression)))
  (jove-expect jove-PAREN-R)
  
  (jove-add-child node (jove-parse-statement))
  (jove-finish node 'for-statement))

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
    (jove-finish node type)))


(defun jove-parse-var (node is-for kind)
  "Return NODE as a variable declarator.
Boolean flag IS-FOR indicates declarations are inside a 'for'
statement initializer.  KIND should be a symbol of either 'var,
'let or 'const."
  ;; NOTE: Returned node is unfinished.
  (let ((collecting t)
        (decl nil)
        (old-in-declaration (jove-in-declaration)))

    (jove-set-in-declaration t)
    (jove-set-prop node :kind kind)

    (while collecting
      ;; Skip empty declarators, less errors while editting.
      (unless (jove-eat jove-COMMA)
        (setq decl (jove-node-make))
        (jove-add-child decl (jove-parse-binding-atom))
        (when (jove-is jove-EQ)
          (jove-next)
          ;; Add initializer as child if present.
          (jove-add-child decl (jove-parse-maybe-assign is-for)))
        (jove-add-child node (jove-finish decl 'variable-declarator))
        (unless (jove-eat jove-COMMA)
          (setq collecting nil))))

    (jove-set-in-declaration old-in-declaration)
    node))

(defun jove-parse-function (node &optional is-stat allow-expr-body is-async)
  "Parse a function using the supplied NODE.
Depending on IS-STAT parse as declaration or literal.  The boolean flag
ALLOW-EXPR-BODY permits an expression body if parsing an arrow function.
The boolean flag IS-ASYNC is used to set the global `jove-in-async'."
  (let ((old-in-function (jove-in-function))
        (old-in-generator (jove-in-generator))
        (old-in-async (jove-in-async))
        (old-in-declaration (jove-in-declaration)))

    (jove-set-in-function t)
    (when (and (not is-async)
               (jove-is jove-STAR))
      (jove-set-in-generator t)
      ;; Highlight '*' as a keyword.
      (jove-set-face* (jove-token) 'font-lock-keyword-face)
      (jove-next))                          ; Move over '*'
    (jove-set-in-async is-async)
    (jove-set-in-declaration t)

    (when is-async
      (jove-set-prop node :async t))

    (when (jove-is jove-NAME)
      (jove-set-face* (jove-token) 'font-lock-function-name-face)
      (jove-set-prop node :id (jove-value (jove-token)))
      (jove-next))

    (jove-parse-function-params node)

    (jove-set-in-declaration old-in-declaration)

    (jove-parse-function-body node allow-expr-body)

    (jove-set-in-function old-in-function)
    (jove-set-in-generator old-in-generator)
    (jove-set-in-async old-in-async)

    (jove-finish node (if is-stat
                           'function-statement
                         'function-expression))))

(defun jove-parse-function-params (parent)
  "Parse function parameters and add as a child to PARENT."
  (jove-add-child parent (jove-finish (jove-parse-binding-list (prog1 (jove-node-make)
                                                     (jove-expect jove-PAREN-L))
                                                   jove-PAREN-R)
                              'parameters)))

(defun jove-parse-class (node &optional is-statement)
  "Return NODE as 'class' declaration or literal.
IF boolean flag IS-STATEMENT is non-nil parse as declaration."
  ;; Deciding on whether to worry about discerning between
  ;; methods and constructor.
  (jove-next)
  ;; Parse identifier.
  (when (jove-is jove-NAME)
    (jove-set-face* (jove-token) 'font-lock-function-name-face)
    (jove-add-child node (jove-parse-identifier)))
  ;; Parse class super.
  (when (jove-is jove-EXTENDS)
    (jove-add-child node (jove-finish (jove-add-child (prog1 (jove-node-make)
                                            (jove-next))
                                          (jove-parse-expr-subscripts))
                              'class-super)))
  (let ((key nil)
        (key-value)
        (method nil)
        (body (jove-node-make))
        (is-generator nil)
        (is-async nil)
        (is-static nil)
        (is-maybe-static nil))
    
    (jove-expect jove-BRACE-L)
    
    (while (not (jove-eat jove-BRACE-R))
      (unless (jove-eat jove-SEMI)              ; Continue.
        (setq method (jove-node-make)
              is-generator nil
              is-async nil
              is-static nil
              is-maybe-static (jove-is-contextual "static"))

        (when (setq is-generator (jove-eat jove-STAR))
          ;; Highlight '*'.
          (jove-set-face* (jove-prev-token) 'font-lock-keyword-face))
        
        (jove-parse-property-name method)
        (jove-set-prop method :static
                   (setq is-static (and is-maybe-static
                                        (jove-is-not jove-PAREN-L)
                                        (not (jove-can-insert-semicolon-p)))))
        (when is-static
          ;; Get rid of 'static' as first child.
          (jove-set-children method '())
          ;; Highlight 'static'.
          (jove-set-face* (jove-prev-token) 'font-lock-keyword-face)
          ;; Don't care if is-generator was already assigned before.
          (setq is-generator (jove-eat jove-STAR))
          ;; Highlight '*'.
          (jove-set-face* (jove-prev-token) 'font-lock-function-name-face)
          ;; Reparse method key.
          (jove-parse-property-name method))

        (when (and (not is-generator)
                   (jove-is-not jove-PAREN-L)
                   (not (jove-get-prop method :computed))
                   (when (eq 'identifier
                             (jove-type (setq key
                                          (car (jove-children method)))))
                     (setq key-value (jove-get-prop key :value))))
          ;; Test for either 'async', 'get' or 'set'.
          (and (or (and (not (jove-can-insert-semicolon-p))
                        (string-equal "async" key-value)
                        (setq is-async t))
                   (and (not is-async)
                        (or (string-equal "get" key-value)
                            (string-equal "set" key-value))))
               (progn
                 (jove-set-face* key 'font-lock-keyword-face)
                 ;; Get rid of 'async', 'get' or 'set' as first child.
                 (jove-set-children method '())
                 ;; Reparse method key.
                 (jove-parse-property-name method))))

        (jove-add-child body (jove-finish (jove-add-child method
                                              (jove-parse-method is-generator
                                                             is-async))
                                  'method-definition))))
    (jove-add-child node (jove-finish body 'class-body))
    (jove-finish node (if is-statement 'class-declaration 'class-expression))))

(defun jove-parse-export (node)
  "Return NODE as 'export' declaration."
  (jove-next)
  (cond
   ((jove-eat jove-STAR)
    (jove-set-face* (jove-prev-token) 'font-lock-keyword-face)
    (jove-eat-contextual "from")
    (jove-set-face* (jove-prev-token) 'font-lock-keyword-face)
    (when (jove-is jove-STRING)
      (jove-add-child node (jove-parse-expr-atom)))
    (jove-finish-node node 'export-all-declaration))
   ((jove-eat jove-DEFAULT)
    (let (is-async)
      (cond
       ((or (jove-is jove-FUNCTION)
            (setq is-async (jove-is-async-function)))
        (let ((f-node (jove-node-make)))
          (jove-next)
          (when is-async (jove-next))
          (jove-add-child node (jove-parse-function f-node t nil is-async))))
       ((jove-is jove-CLASS)
        (jove-add-child node (jove-parse-class (jove-node-make) t)))
       (t
        (jove-add-child node (jove-parse-maybe-assign))
        (jove-semicolon))
       (jove-finish node 'export-default-declaration))))
   ((or (jove-is* jove-VAR jove-CONST jove-CLASS jove-FUNCTION)
        (jove-is-let)
        (jove-is-async-function))
    (jove-add-child node (jove-parse-statement t))
    (jove-finish node 'export-named-declaration))
   (t
    (when (jove-eat jove-BRACE-L)
      (let ((spec nil)
            (specs (jove-node-make)))       ; Specifiers
        (jove-parse-sequence jove-BRACE-R
          (setq spec (jove-node-make))
          (jove-add-child spec (jove-parse-identifier t))
          (when (jove-eat-contextual "as")
            (jove-set-face* (jove-prev-token) 'font-lock-keyword-face)
            (jove-add-child spec (jove-parse-identifier t)))
          (jove-finish spec 'export-specifier)
          (jove-add-child specs spec))
        (jove-next)                         ; Move over '}'
        (jove-add-child node (jove-finish specs 'export-specifiers))))
    (when (jove-eat-contextual "from")
      (jove-set-face* (jove-prev-token) 'font-lock-keyword-face)
      (when (jove-is jove-STRING)
        (jove-add-child node (jove-parse-expr-atom))))
    (jove-semicolon)
    (jove-finish node 'export-named-declaration))))

(defun jove-parse-import (node)
  "Return NODE as 'import' declaration."
  (jove-next)
  (if (jove-is jove-STRING)
      (jove-add-child node (jove-parse-expr-atom))
    (let ((specs (jove-parse-import-specifiers (jove-node-make))))
      (jove-finish specs 'import-specifiers)
      (jove-add-child node specs)
      (when (jove-eat-contextual "from")
        (jove-set-face* (jove-prev-token) 'font-lock-keyword-face)
        (when (jove-is jove-STRING)
          (jove-add-child node (jove-parse-expr-atom))))))
  (jove-semicolon)
  (jove-finish node 'import-declaration))

(defun jove-parse-import-specifiers (node)
  "Return NODE as a comma separated list of module imports."
  (catch 'specifiers
    (let (spec)
      (when (jove-is jove-NAME)
        ;; import defaultObject, {foo, bar as qux} from 'boo'
        (setq spec (jove-node-make))
        (jove-set-face* (jove-token) 'font-lock-variable-name-face)
        (jove-add-child spec (jove-parse-identifier))
        (jove-finish spec 'import-default-specifier)
        (jove-add-child node spec)
        (unless (jove-eat jove-COMMA)
          (throw 'specifiers node)))
      (when (jove-is jove-STAR)
        (jove-set-face* (jove-token) 'font-lock-keyword-face)
        (setq spec (jove-node-make))
        (jove-next)                         ; Move over '*'
        (when (jove-eat-contextual "as")
          (jove-set-face* (jove-prev-token) 'font-lock-keyword-face)
          (when (jove-is jove-NAME)
            (jove-set-face* (jove-token) 'font-lock-variable-name-face)
            (jove-add-child spec (jove-parse-identifier))))
        (jove-finish spec 'import-namespace-specifier)
        (jove-add-child node spec))
      (when (jove-eat jove-BRACE-L)
        (jove-parse-sequence jove-BRACE-R
          (setq spec (jove-node-make))
          (jove-add-child spec (jove-parse-identifier t))
          (if (jove-eat-contextual "as")
              (progn
                (jove-set-face* (jove-prev-token) 'font-lock-keyword-face)
                (when (jove-is jove-NAME)
                  (jove-set-face* (jove-token) 'font-lock-variable-name-face)
                  (jove-add-child spec (jove-parse-identifier))))
            (jove-set-face* (jove-prev-token) 'font-lock-variable-name-face))
          (jove-finish spec 'import-specifier)
          (jove-add-child node spec))
        (jove-next)))                       ; Move over '}'
    node))                              ; Return NODE.

(defun jove-parse ()
  "Run the Jove parser."
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (jove-config)
      (setq jove-ast (jove-node-make))
      (let ((start-pos (point))
            (start-time (float-time)))
        (jove-next)                         ; Load an initial token.
        (save-match-data
          (setq jove-ast (jove-parse-top-level jove-ast)))
        (when (and jove-verbose)
          (let ((time (/ (truncate (* (- (float-time) start-time)
                                      10000))
                         10000.0)))
            (message "Parser finished in %0.3fsec" time)))
        (jove-apply-fontifications start-pos (point))))))

(defun jove-find-node-at (node pos)
  "Return NODE or a child of NODE at found at POS.
Return nil if nothing is found."
  (when (and (<= (jove-start node) pos)
             (> (jove-end node) pos))
    (or (jove-find-child-at (jove-children node) pos)
        node)))

(defun jove-find-child-at (children pos)
  "Search for first child in CHILDREN to be within POS."
  (let ((child (car children)))
    (cond
     ((null child)
      nil)
     ((and (<= (jove-start child) pos)
           (> (jove-end child) pos))
      (jove-find-node-at child pos))
     (t
      (jove-find-child-at (cdr children) pos)))))

(provide 'jove-parser)

;;; jove-parser.el ends here
