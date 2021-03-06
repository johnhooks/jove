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

(defvar-local jove--in-function nil
  "Boolean flag to indicate the parser is in a function.")

(defvar-local jove--in-async nil
  "Boolean flag to indicate the parser is in an async function.")

(defvar-local jove--in-generator nil
  "Boolean flag to indicate the parser is in a generator function.")

(defvar-local jove--in-declaration nil
  "Boolean flag to indicate the parser is in a delcaration.")

(defvar-local jove--potential-arrow-at nil
  "A position in the buffer that may begin an fat arrow function.
The starting position of identifiers and parenthesized expressions are
recorded.")

(defvar-local jove--tmp nil
  "Temporary stack of previously lexed tokens.")

(defvar-local jove--flushed-p nil
  "AST has previously been flushed.")

(defvar-local jove--error nil
  "Current parse error.")

(defvar-local jove--peeked nil
  "Previously peeked token.")

;; Fontification

(defsubst jove-clear-face (start end)
  "Remove face properties from START to END."
  (remove-text-properties start end '(font-lock-face nil)))

(defun jove-apply-fontifications (start end &optional no-clear)
  "Apply fontifications from START to END.
Boolean NO-CLEAR flag prevents clearing faces before application."
  (with-silent-modifications
    (unless no-clear (jove-clear-face start end))
    (mapc #'(lambda (f)
              (apply #'put-text-property f))
          (nreverse jove--fontifications))  ; Allows applying over those previously pushed.
    (mapc #'(lambda (f)
              (put-text-property (nth 0 f) (nth 1 f) 'font-lock-face font-lock-warning-face))
          jove--warnings)
    (setq jove--fontifications nil
          jove--warnings nil)))     ; Probably shouldn't reset here.

(defun jove-set-face (start end face)
  "Queue region START to END for fontification using FACE."
  ;; This could was in js2-mode... don't understand its purpose.
  ;; Other than if someone put in a bad value.
  ;; (setq start (min (point-max) start)
  ;;       start (max (point-min) start)
  ;;       end (min (point-max) end)
  ;;       end (max (point-min) end))
  (push (list start end 'font-lock-face face) jove--fontifications))

(defsubst jove-set-face* (object face)
  "Queue region from OBJECT for fontification using FACE.
Both nodes and tokens use zero index for the start position and
the first index for the end position."
  (jove-set-face (nth 0 object) (nth 1 object) face))

;;; Initialization

(defun jove-config (&optional _state)
  "Initialize the parser.
If STATE not supplied create an initial state."
  (setq jove--in-function nil
        jove--in-async nil
        jove--in-generator nil
        jove--in-declaration nil
        jove--potential-arrow-at -1)

  ;; TODO: Need to figure out a way to make sure the two matchup.
  ;; (eq (jove-end jove-ast)                   ; If the end points do not match
  ;;     (jove-end (car jove--cache)))         ; complete a full reparse.
  
  (if (and jove-ast
           jove--cache)
      (progn
        (jove-config-lexer jove--cache)
        ;; (jove-next) ; This would mean that if we start from a cached
        ;; position all the previous token information is unavailable.
        ;; I reviewed the situations in which the previous token info
        ;; is used an there does not seem to be a situation in which
        ;; not having it avaiable at the initial token of a statement
        ;; would cause a problem. Though look into it more closely.

        ;; Also the face information is not being saved right now.
        )
    (jove-config-lexer)
    (setq jove-ast (jove-make-node))
    (jove-next)))                          ; Load initial token and cache.

;;; Token Movement Functions

(defun jove-next ()
  "Advance parser to next token."
  ;; This is just a temp measure until I figure out a better way.
  (setq jove--prev-start jove--start
        jove--prev-end jove--end
        jove--prev-tt jove--tt
        jove--prev-linum jove--linum)
  (jove-next-token)
  (when jove--face
    (if (listp jove--face)
        (dolist (f jove--face)
          (apply #'jove-set-face f))
      (jove-set-face jove--start jove--end jove--face))))

;; TODO: This function should not waste the tokens, they should be collected.
(defun jove-peek (&optional count)
  "Peek ahead either one or COUNT number of tokens.
Return a copy of the updated LEX-STATE."
  (save-excursion
    (let ((jove--start jove--start)
          (jove--end jove--end)
          (jove--tt jove--tt)
          (jove--value jove--value)
          (jove--face jove--face)
          (jove--linum jove--linum)
          (jove--expr-allowed jove--expr-allowed)
          (jove--newline-before jove--newline-before)
          (jove--ctx-stack jove--ctx-stack)
          (tmp jove--tmp))
      (if (numberp count)
          (progn
            (while (< 0 (setq count (1- count)))
              (if tmp
                  (jove-config-lexer (pop tmp)))
              (jove-next-token))
            (jove-next-token))
        (jove-next-token))
      (jove-make-token))))                  ; Return a token list.

;; Node

(defun jove-make-node (&optional start end type)
  "Return a node.
If START is not supplied, its will be set to the start position of the
current token.  Optionally supply any or all START, END, or TYPE."
  (list (or start jove--start)              ; 0
        end                             ; 1
        type                            ; 2
        nil                             ; 3 props
        nil                             ; 4 parent
        nil                             ; 5 children
        'jove-node))

(defun jove-make-node* (object &optional type) ; Can be token or node
  "Return a node.
Use OBJECT position data to set start and end location data.
Optionally supply node TYPE."
  (list (nth 0 object)                  ; 0
        (nth 1 object)                  ; 1
        type                            ; 2
        nil                             ; 3 props
        nil                             ; 4 parent
        nil                             ; 5 children
        'jove-node))

;; Data Getter and Setter Functions.

(defsubst jove-node-p (object)
  "Return t if OBJECT is a node."
  (and (list object)
       (eq 'jove-node (nth 6 object))))

(defsubst jove-start (list)
  "Return the 'start' slot of the LIST."
  (nth 0 list))
(defsubst jove-set-start (list value)
  "Set the 'start' slot of the LIST to VALUE."
  (setf (nth 0 list) value))

(defsubst jove-end (list)
  "Return the 'end' slot of the LIST."
  (nth 1 list))
(defsubst jove-set-end (list value)
  "Set the 'end' slot of the LIST to VALUE."
  (setf (nth 1 list) value))

(defsubst jove-type (list)
  "Return the 'type' slot of the LIST."
  (nth 2 list))
(defsubst jove-set-type (list value)
  "Set the 'type' slot of the LIST to VALUE."
  (setf (nth 2 list) value))

(defalias 'jove-tt 'jove-type)

(defsubst jove-props (node)
  "Return the 'props' slot of the NODE."
  (nth 3 node))
(defsubst jove-get-prop (node property)
  "Get NODE information PROPERTY from the alist 'node-props'.
Uses `assq' for PROPERTY lookup."
  (car (cdr (assq property (nth 3 node)))))
(defsubst jove-set-prop (node property value)
  "Set NODE information PROPERTY with VALUE into the alist 'node-props'."
  (setf (nth 3 node)
        (cons (list property value) (jove-props node))))

(defsubst jove-parent (node)
  "Return the 'parent' slot of the NODE."
  (nth 4 node))
(defsubst jove-set-parent (node parent)
  "Set the 'parent' slot of the NODE to PARENT."
  (setf (nth 4 node) parent))

(defsubst jove-children (node)
  "Return the 'children' slot of the NODE."
  (nth 5 node))
(defsubst jove-set-children (node value)
  "Set the 'children' slot of the NODE to VALUE."
  (setf (nth 5 node) value))
(defsubst jove-push-child (node child)
  "Push on top of NODE's 'children' slot CHILD."
  (setf (nth 5 node) (cons child (nth 5 node))))
(defun jove-pop-child (node)
  "Pop top child off the 'children' slot of NODE."
  (let ((children (nth 5 node)))
    (prog1 (car children)
      (setf (nth 5 node) (cdr children)))))

(defun jove-add-child (node child)
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
    (setf (nth 5 node) (append children (nth 5 node))))
  node)

(defun jove-finish (node &optional type)
  "Finish NODE of TYPE.
Uses position at the end of the previous token."
  ;; Reverse children.
  (setf (nth 5 node) (nreverse (nth 5 node)))
  (when type
    (jove-set-type node type))
  ;; The parser will have already advanced to the next token,
  ;; which is why the `jove--prev-end' is used.
  (jove-set-end node jove--prev-end)
  node)

(defun jove-finish* (node &optional type)
  "Finish NODE of TYPE.
This function is for the rare occasion a node does not use the
`jove-prev-token' location data for its end location.  All the
information has previously been set, all that is left is to is
set node 'type' slot and `nreverse' its children."
  (setf (nth 5 node) (nreverse (nth 5 node)))
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
  (let ((start (or start jove--start))
        (end (or end jove--end)))
    (jove-set-face start end 'js2-error)
    (signal (or type 'jove-parse-error)
            (list (jove-format-error start end message) start end))))

(defun jove-unexpected (&optional key)
  "Signal an unexpected token parse error.
If KEY is provided attempt to look up the message in `jove-messages'."
  (let ((message (gethash key jove-messages)))
    (jove-set-face jove--start jove--end 'js2-error)
    (jove-signal (or message
                (format "unexpected token: %s" (jove-tt-label jove--tt))))))

;; TODO: Remove.
(defsubst jove-is (tt)
  "Return non-nil if the current token is of the type TT."
  (eq tt jove--tt))

;; TODO: Remove.
(defsubst jove-is* (&rest tts)
  "Return non-nil if the current token is one of the types TTS."
  (memq jove--tt tts))

;; TODO Remove.
(defsubst jove-is-not (tt)
  "Return non-nil if the current token is not of the type TT."
  (not (eq tt jove--tt)))

(defun jove-eat (tt)
  "Return non-nil if the current token is of the type TT.
If the test passes consume the token as a side effect."
  (when (eq tt jove--tt)
    (jove-next)
    t))

(defun jove-eat* (tt face)
  "Return non-nil if the current token is of the type TT.
If the test passes consume the token as a side effect and queue
for fontificiation using FACE."
  (when (eq tt jove--tt)
    (jove-set-face jove--start jove--end face)
    (jove-next)
    t))

(defun jove-after-trailing-comma-p (tt &optional not-next)
  "Return non-nil if the current token is of the type TT.
Advance to next token, unless NOT-NEXT."
  ;; NOTE: Found in corn/src/parseutil.js
  (when (eq tt jove--tt)
    (unless not-next
      (jove-next))
    t))

;; TODO: Remove.
(defun jove-is-contextual (name)
  "Test whether current token is a contextual keyword NAME.
Optionally test against TOKEN if provided."
  (and (eq jove-NAME jove--tt)
       (string-equal name jove--value)))
;; TODO: Remove.
(defun jove-eat-contextual (name)
  "Consume contextual keyword NAME is possible."
  (and (jove-is-contextual name)
       (jove-eat jove-NAME)))

(defun jove-can-insert-semicolon-p ()
  "Test whether or not a semi-colon can be inserted."
  (or (memq jove--tt (list jove-EOB jove-BRACE-R))
      jove--newline-before))

(defun jove-semicolon ()
  "Consume a semicolon or if allowed pretend one is there.
Return the value of the last sexp in BODY.  Though if unable to
eat a semicolon, flag next statement as junk."
  (unless (or (jove-eat jove-SEMI)
              (jove-can-insert-semicolon-p))
    (setq jove--error :cannot-insert-semi)))

(defun jove-null-expression (pos)
  "Create a null expression.
Used as a place holder is some type of nodes."
  (jove-make-node pos pos 'null-expression))

;;; Parse Error Handler

(defun jove-handle-error (node)
  "If `jove-error' and `jove-debug' message error and fontifiy NODE.
Return NODE.  Error message is printed to *Message* buffer.  And
region between `jove-start' and `jove-end' are
fontified with `js2-error' face."
  (when jove--error
    (when jove-debug
      (let ((error-message nil)
            (error-key jove--error))
        (cond
         ((and (eq :cannot-insert-semi error-key)
               (eq jove-AWAIT jove--prev-tt))
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
    (setq jove--error nil))
  node)

;;; Helper Macros

(defmacro jove-expect (tt &rest recover)
  "Expect the current token to be of the type TT.
If the test passes consumed the token as a side effect, if the test fails
evaluate RECOVER if provided, otherwise throw an error."
  (declare (indent 1))
  `(if (eq ,tt jove--tt)
       (jove-next)
     (if ,recover
         (progn ,@recover)
       (let ((expected (jove-tt-label ,tt))
          (found (if (eq jove-NAME jove--tt)
                     jove--value
                   (jove-tt-label jove--tt))))
         (jove-signal (format "expected '%s' found '%s'" expected found))))))

(defmacro jove-parse-sequence (closing &rest body)
  "Loop while looking for CLOSING token type.
BODY is evaluated between `jove-COMMA' tokens, while allowing for
empty expressions and trailing commas.  NOTE: The CLOSING token
is not moved over."
  (declare (indent 1))
  `(progn
     ;; Execute body once before beginning the loop.
     (unless (memq jove--tt (list jove-COMMA ,closing))
       ,@body)
     (while (not (eq ,closing jove--tt))
       (jove-expect jove-COMMA)
       (unless (or (jove-after-trailing-comma-p ,closing t)
                   (eq jove-COMMA jove--tt))
         ,@body))))

;;; Expression Parsing Functions

(defun jove-parse-expression (&optional no-in)
  "Parse a full expression.
The boolean flag NO-IN forbids the 'in' operator."
  (let ((start-pos jove--start)
        (expr (jove-parse-maybe-assign no-in)))
    (if (eq jove-COMMA jove--tt)
        (let ((node (jove-make-node start-pos)))
          (jove-add-child node expr)
          (while (jove-eat jove-COMMA)
            (jove-add-child node (jove-parse-maybe-assign no-in)))
          (jove-finish node 'sequence-expression))
      expr)))

(defun jove-parse-maybe-assign (&optional no-in)
  "Maybe parse an assignment expression.
The boolean flag NO-IN forbids the 'in' operator."
  (if (and jove--in-generator
           (eq jove-YIELD jove--tt))
      (jove-parse-yield)
    (when (or (eq jove-PAREN-L jove--tt)
              (jove-tt-is-word jove--tt))
      (setq jove--potential-arrow-at jove--start))
    (let ((start-pos jove--start)
          (left (jove-parse-maybe-conditional no-in)))
      ;; Find where afterLeftParse is used.
      (if (jove-tt-is-assign jove--tt)
          (let ((node (jove-make-node start-pos)))
            (jove-set-prop node :op jove--value)
            (jove-add-child node (if (eq jove-EQ jove--tt)
                                 (jove-to-assignable left)
                               left))
            (jove-next)                     ; Move over the operator.
            (jove-add-child node (jove-parse-maybe-assign no-in))
            (jove-finish node 'assign-expression))
        left))))

(defun jove-parse-maybe-conditional (&optional no-in)
  "Maybe parse a ternary conditional expression.
The boolean flag NO-IN forbids the 'in' operator."
  (let ((start-pos jove--start)
        (expr (jove-parse-expr-ops no-in)))
    (if (eq jove-QUESTION jove--tt)
        (let ((node (jove-make-node start-pos)))
          (jove-set-prop node :mark jove--start)
          (jove-next)                       ; Move over '?'
          (jove-add-children node
                         expr
                         (jove-parse-maybe-assign) ; no NO-IN here???
                         (when (jove-eat jove-COLON)
                           (jove-parse-maybe-assign no-in)))
          (jove-finish node 'conditional-expression))
      expr)))

(defun jove-parse-expr-ops (no-in)
  "Start the precedence parser.
If current expression is an ArrowFunctionExpression, just return
the expression.  The boolean flag NO-IN forbids the 'in' operator."
  (let ((start-pos jove--start)
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
                       (not (eq jove-IN jove--tt)))
                   (jove-tt-binop jove--tt))))
    (if (and prec (> prec min-prec))
        (let ((logical (memq jove--tt (list jove-LOGICAL-OR jove-LOGICAL-AND)))
              (op (prog1 (jove-tt-label jove--tt)
                    (jove-next)
                    (setq start-pos jove--start)))
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
  (let ((node (jove-make-node start-pos)))
    (jove-set-prop node :op op)
    (jove-add-children node left right)
    (jove-finish node (if logical 'logical-expression 'binary-expression))))

;; TODO: Consider where the operator needs to be saved.
(defun jove-parse-maybe-unary (saw-unary)
  "Parse both prefix and postfix unary operators.
Unless SAW-UNARY, build a binary expression when '**' is encountered."
  (let ((expr nil)
        (start-pos jove--start))
    (cond
     ((and jove--in-async
           (eq jove-AWAIT jove--tt))
      (setq expr (jove-parse-await)))
     ((jove-tt-prefix jove--tt)
      (let ((node (jove-make-node))
            (update (eq jove-INC-DEC jove--tt)))
        (jove-set-prop node :prefix t)
        (jove-set-prop node :op (jove-tt-label jove--tt))
        (jove-next)
        (jove-add-child node (jove-parse-maybe-unary t))
        (setq expr (jove-finish node (if update
                                     'update-expression
                                   'unary-expression)))
        (unless update (setq saw-unary t))))
     (t
      (setq expr (jove-parse-expr-subscripts))
      (while (and (jove-tt-postfix jove--tt)
                  (not (jove-can-insert-semicolon-p)))
        (let ((node (jove-make-node start-pos)))
          (jove-set-prop node :postfix t)
          (jove-add-child node expr)
          (jove-set-prop node :op (jove-tt-label jove--tt))
          (jove-next)
          (setq expr (jove-finish node 'update-expression))))))
    (if (and (not saw-unary)
             (jove-eat jove-STARSTAR))
        (jove-build-binary start-pos
                       expr
                       (jove-parse-maybe-unary nil)
                       "**"
                       nil)
      expr)))

(defun jove-parse-expr-subscripts ()
  "Parse call, dot, and bracket notation subscript expressions."
  (let ((start-pos jove--start)
        (expr (jove-parse-expr-atom)))
    (jove-parse-subscripts expr start-pos)))

(defun jove-parse-subscripts (base start-pos &optional no-calls)
  "Possibly parse the subscripts of BASE.
All subsequent nodes wrap BASE and initialize at START-POS.
Optionally if NO-CALLS disallow the parsing of call expressions."
  (let ((computed nil)
        (maybe-async-arrow (and (eq 'identifier (jove-type base))
                                (string-equal "async" (jove-get-prop base :value))
                                (= jove--prev-end (jove-end base))
                                (not (jove-can-insert-semicolon-p)))))
    (catch 'node
      (while t
        (cond
         ;; Member Expression
         ((or (setq computed (jove-eat jove-BRACKET-L))
              (jove-eat jove-DOT))
          (let ((node (jove-make-node start-pos)))
            (when computed
              (jove-set-prop node :computed t))
            (jove-add-children node
                           base
                           (if computed
                               (jove-parse-expression)
                             (jove-parse-identifier t)))
            (if computed
                (jove-expect jove-BRACKET-R)
              (jove-set-face jove--prev-start jove--prev-end 'js2-object-property))
            (setq base (jove-finish node 'member-expression))))
         ;; Call Expression
         ((and (not no-calls)
               (eq jove-PAREN-L jove--tt))
          (let ((id-start jove--prev-start) ; Save for possibly highlighting.
                (id-end jove--prev-end)     ; TODO: Simplify.
                (id-tt jove--prev-tt)
                (exprs (jove-parse-expr-list (prog1 (jove-make-node)
                                               (jove-next)) ; Move over '('
                                             jove-PAREN-R)))
            (jove-set-end exprs jove--prev-end)
            ;; 'exprs' is not finished like most nodes.  It is almost
            ;; complete, only the 'type' slot remains to be set.
            (if (and maybe-async-arrow
                     (not (jove-can-insert-semicolon-p))
                     (jove-eat jove-ARROW))
                (progn
                  (let ((jove--in-declaration t))
                    (dolist (child (jove-children exprs))
                      (jove-to-assignable child)))
                  (jove-finish* exprs 'parameters)
                    ;; Highlight 'async' as a keyword.
                  (jove-set-face id-start id-end 'font-lock-keyword-face)
                  (throw 'node (jove-parse-arrow-expr (jove-make-node start-pos)
                                                  exprs
                                                  t)))
              (let ((node (jove-make-node start-pos)))
                (jove-finish* exprs 'arguments)
                ;; Highlight 'id' as a call.
                (when (jove-tt-is-word id-tt)
                  (jove-set-face id-start id-end 'js2-function-call))
                (jove-add-children node base exprs)
                (jove-finish node 'call-expression)
                (setq base node)))))
         ;; Tagged Template Expression
         ((eq jove-BACKQUOTE jove--tt)
          (jove-finish (jove-add-children (jove-make-node start-pos)
                                  base
                                  (jove-parse-template))
                        'tagged-template-expression))
         (t
          (throw 'node base)))))))

(defun jove-parse-expr-atom ()
  "Parse an atomic expression."
  (let ((tt jove--tt)
        (can-be-arrow (= jove--start jove--potential-arrow-at)))
    (cond
     ((eq jove-SUPER tt)
      (when (not jove--in-function)
        (jove-warn jove--start jove--end "'super' used outside of function or class"))
      (prog1 (jove-make-node jove--start jove--end 'super)
        (jove-next)))
     ((eq jove-THIS tt)
      (prog1 (jove-make-node jove--start jove--end 'this-expression)
        (jove-next)))
     ((jove-tt-is-word tt)
      (let ((start-pos jove--start)
            (id (jove-parse-identifier nil)))
        (cond
         ((and (eq jove-ASYNC jove--prev-tt)
               ;; (string-equal "async" (jove-get-prop id :value))
               (not (jove-can-insert-semicolon-p))
               (eq jove-FUNCTION jove--tt))
          ;; Highlight 'async' as keyword.
          (jove-set-face jove--prev-start jove--prev-end font-lock-keyword-face)
          (jove-next)                       ; Move over 'function'
          ;; Parse async function.
          (jove-parse-function (jove-make-node start-pos) nil nil t))
         ((and can-be-arrow
               (not (jove-can-insert-semicolon-p)))
          (cond
           ((jove-eat jove-ARROW)
            ;; Wrap the single parameter in a 'parameters' node.
            (let ((params (jove-make-node* id 'parameters)))
              ;; Highlight node 'id' as a parameter.
              (jove-set-face* id 'font-lock-variable-name-face)
              (jove-add-child params id)
              (jove-parse-arrow-expr (jove-make-node start-pos) params nil)))
           ((and (eq jove-ASYNC jove--prev-tt)
                 (jove-tt-is-word jove--tt))
            ;; Highlight 'async' as keyword.
            (jove-set-face jove--prev-start jove--prev-end 'font-lock-keyword-face)
            (setq id (jove-parse-identifier))
            ;; Highlight node 'id' as a parameter.
            (jove-set-face* id 'font-lock-variable-name-face)
            (if (or (jove-can-insert-semicolon-p)
                    (not (jove-eat jove-ARROW)))
                (jove-unexpected)
              (let ((params (jove-make-node (jove-start id) (jove-end id) 'parameters)))
                ;; Wrap the single parameter in a 'parameters' node.
                (jove-add-child params id)
                (jove-parse-arrow-expr (jove-make-node start-pos params t) params t))))
           (t           ; Return basic identifier.
            id)))
         (t
          id))))
     ((or (jove-tt-is-atom jove--tt)
          (memq tt (list jove-REGEX jove-STRING jove-NUM)))
      (prog1 (jove-make-node jove--start jove--end 'literal)
        (jove-next)))
     ((eq jove-PAREN-L tt)
      (jove-parse-paren-expr can-be-arrow))
     ((eq jove-BRACKET-L tt)
      (jove-finish (jove-parse-expr-list (prog1 (jove-make-node)
                                   (jove-next)) ; Move over '['
                                 jove-BRACKET-R)
               'array-expression))
     ((eq jove-BRACE-L tt)
      (jove-parse-object nil))
     ((eq jove-FUNCTION tt)
      (jove-parse-function (prog1 (jove-make-node) (jove-next)) nil))
     ((eq jove-CLASS tt)
      (jove-parse-class (jove-make-node) nil))
     ((eq jove-NEW tt)
      (jove-parse-new))
     ((eq jove-BACKQUOTE tt)
      (jove-parse-template))
     ((eq jove-JSX-TEXT tt)
      (jove-jsx-parse-text))
     ((eq jove-JSX-TAG-START tt)
      (jove-jsx-parse-element))
     (t
      (jove-unexpected)))))

(defun jove-parse-paren-expr (&optional can-be-arrow)
  "Parse a parenthesized expression.
As of ECMAScript 6, possiblities are SequenceExpression or
ArrowExpression.  ArrowExpressions are allowed when the boolean
flag CAN-BE-ARROW is non-nil.  A sequence is wrapped in
ParenthesizedExpression.  Trailing commas and empty expressions
are allowed."
  (let ((node (jove-make-node))
        (start-pos jove--start)
        (inner-end-pos nil)
        (inner-start-pos (progn (jove-next) jove--start)))
    (jove-parse-sequence jove-PAREN-R
      (jove-add-child node (if (eq jove-ELLIPSIS jove--tt)
                           (jove-parse-rest)
                         (jove-parse-maybe-assign nil))))
    (setq inner-end-pos jove--start)
    (jove-expect jove-PAREN-R)                  ; FIX: Don't think expect is necessary.
    ;; Return a ArrowFunctionExpression or ParenthesizedExpression.
    (if (and can-be-arrow
             (not (jove-can-insert-semicolon-p))
             (eq jove-ARROW jove--tt))
        (progn
          (jove-finish node 'parameters)
          (let ((jove--in-declaration t))
            (jove-to-assignable (jove-children node)))
          (jove-next)                         ; Move over '=>'
          (jove-parse-arrow-expr (jove-make-node start-pos) node))
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
      (jove-finish (jove-add-child (jove-make-node start-pos)
                           node)
               'parenthesized-expression))))

(defun jove-parse-new ()
  "Parse a new expression."
  (let ((node (jove-make-node))
        (meta (jove-parse-identifier t)))
    (if (jove-eat jove-DOT)
        (jove-finish (jove-add-children node
                                meta
                                (jove-parse-identifier t))
                 'meta-property)
      (let ((start-pos jove--start))
        (jove-add-child node (jove-parse-subscripts (jove-parse-expr-atom)
                                            start-pos
                                            t))
        (when (jove-is jove-PAREN-L)
          (jove-add-child node (jove-finish (jove-parse-expr-list (prog1 (jove-make-node)
                                                        (jove-next))
                                                      jove-PAREN-R)
                                    'arguments)))
        (jove-finish node 'new-expression)))))

(defun jove-parse-template-element ()
  "Parse template element."
  (let ((node (jove-make-node)))
    (jove-next)
    (when (eq jove-BACKQUOTE jove--tt)
      (jove-set-prop node :tail t))
    (jove-finish node 'template-element)))

(defun jove-parse-template ()
  "Parse template literal."
  ;; TODO: Fontify template literals from here.
  (let ((elt nil)                       ; Maintain ref to current element.
        (node (jove-make-node)))
    (jove-next)                             ; Move over '`'
    (jove-add-child node (setq elt (jove-parse-template-element)))
    (while (not (jove-get-prop elt :tail))
      (jove-expect jove-DOLLAR-BRACE-L)
      (if (eq jove-BRACE-R jove--tt)
          ;; Expression is empty.
          (jove-add-child node (prog1 (jove-make-node jove--prev-end
                                              jove--start
                                              'null-expression)
                             (jove-next)))
        ;; Otherwise parse an expression.
        (jove-add-child node (jove-parse-expression))
        (jove-expect jove-BRACE-R))
      (jove-add-child node (setq elt (jove-parse-template-element))))
    (jove-next)                             ; Move over '`'
    (jove-finish node 'template-literal)))

(defun jove-parse-object (&optional is-pattern)
  "Parse an object literal or binding pattern.
Optionally if IS-PATTERN, parse as an ObjectPattern."
  (let ((node (jove-make-node))
        (prop nil)
        (name nil)
        (value nil)
        (next nil)
        (is-async nil)
        (is-generator nil)
        (start-pos nil))
    (jove-next)                             ; Move over '{'
    (jove-parse-sequence jove-BRACE-R
      (setq prop (jove-make-node)
            is-async nil
            is-generator (and (not is-pattern) (jove-eat* jove-STAR 'font-lock-keyword-face))
            start-pos jove--start)
      (when (and (memq jove--tt (list jove-ASYNC jove-GET jove-SET))
                 (not (or is-pattern is-generator))
                 (jove-tt-is-word (jove-tt (setq next (jove-peek)))) ; Will not match keywords or atoms...
                 ;; Maybe cut this... because the only reason we should use 'async',
                 ;; 'get' and 'set' is with a non-computed property name, and the
                 ;; above check would eliminate all the situations in the below condition.
                 ;; (not (memq (jove-tt next) (list jove-COMMA jove-BRACE-R jove-PAREN-L jove-COLON)))
                 (not (nth 4 next)))    ; The 'newline-before' slot of the next token.
        (and (eq jove-ASYNC jove--tt)
             (setq is-async t))
        (jove-set-face jove--start jove--end 'font-lock-keyword-face)
        (jove-next))
      (setq name (jove-parse-property-name prop))
      (setq value (jove-parse-property-value prop is-pattern is-generator is-async start-pos))
      (when (not (jove-get-prop prop :computed))
        (jove-set-face* name (cond
                          ((memq (jove-type value)
                                 '(function-expression arrow-function-expression))
                           'font-lock-function-name-face)
                          (is-pattern
                           'font-lock-variable-name-face)
                          (t
                           'js2-object-property))))
      (jove-add-child node (jove-finish prop 'property)))
    (jove-next)                             ; Move over '}'
    (jove-finish node (if is-pattern 'object-pattern 'object-expression))))
;; If is-pattern the property name should be highlighted as a variable declaration,
;; If the 'async', 'get', 'set', or '*' are encountered the context needs to be
;; examined to determine if they are contextual keywords.

;; TODO: Need a better way to flag different node options.
(defun jove-parse-property-value (prop is-pattern is-generator is-async start-pos)
  "Parse object property value.
Add as a child to the node PROP.  Boolean flags IS-PATTERN,
IS-GENERATOR, and IS-ASYNC are used to parse the property value
according to context.  START-POS is the position at the beginning
of the property."
  (let (value)
    (when (and (or is-generator is-async)
               (eq jove-COLON jove--tt))
      (jove-unexpected))
    (cond
     ((jove-eat jove-COLON)
      (jove-add-child prop (setq value (if is-pattern
                                       (jove-parse-maybe-default start-pos)
                                     (jove-parse-maybe-assign)))))
     ((eq jove-PAREN-L jove--tt)
      (when is-pattern
        (jove-unexpected))
      (jove-add-child prop (setq value (jove-parse-method is-generator is-async))))
     ;; This seems super redundant to create a node for value when its identical
     ;; to the name.
     ((and (not (jove-get-prop prop :computed))
           (eq 'identifier (jove-type (car (jove-children prop)))))
      (let ((id (jove-make-node jove--prev-start
                            jove--prev-end
                            'identifier)))
        (jove-set-prop id :value jove--prev-value)
        (when (and is-pattern
                   (eq jove-EQ jove--tt))
          
          (jove-add-child prop (setq value (jove-parse-maybe-default start-pos id))))))
     (t
      (jove-unexpected)))
    value))

(defun jove-parse-property-name (prop)
  "Parse object property name.
Return the property name node.  Add child property name to PROP
as a side effect."
  (let (name)
    (if (jove-eat jove-BRACKET-L)
        (progn
          (jove-set-prop prop :computed t)
          (jove-add-child prop (setq name (jove-parse-maybe-assign)))
          (jove-expect jove-BRACKET-R))
      (jove-add-child prop (setq name (if (memq jove--tt (list jove-NUM jove-STRING))
                                      (jove-parse-expr-atom)
                                    (jove-parse-identifier t)))))
    name))

(defun jove-parse-method (&optional is-generator is-async)
  "Parse an object or class method.
Boolean flags IS-GENERATOR and IS-ASYNC lexically reset the
global variables `jove--in-generator' and `jove--in-async'."
  (let ((node (jove-make-node))
        (params (prog1 (jove-make-node)
                  (jove-expect jove-PAREN-L))))
    (let ((jove--in-function t)
          (jove--in-generator is-generator)
          (jove--in-async is-async))
      (let ((jove--in-declaration t))
        (jove-add-child node (jove-finish (jove-parse-binding-list params
                                                       jove-PAREN-R)
                                  'parameters)))
      (jove-parse-function-body node))
    (jove-finish node 'function-expression)))

(defun jove-parse-arrow-expr (node params &optional is-async)
  "Parse arrow function using supplied NODE with given PARAMS.
Boolean flag IS-ASYNC sets the global variable `jove--in-asynce'."
  (let ((jove--in-function t)
        (jove--in-generator nil)
        (jove--in-async is-async))
    ;; NOTE: Arguments have already been highlighted. (Is that true? Should happen
    ;; below in the `jove-to-assignable' function.
    (when is-async
      (jove-set-prop node :async t))
    (let ((jove--in-declaration t))
      (dolist (param (jove-children params))
        (jove-to-assignable param)))
    (jove-add-child node params)
    (jove-parse-function-body node t)
    (jove-finish node 'arrow-function-expression)))

(defun jove-parse-function-body (parent &optional is-arrow-func)
  "Parse function body and add as child to PARENT.
If IS-ARROW-FUNC is non-nil, possibly parse body as an expression,
unless the body is contained in a brace block."
  (if (and is-arrow-func
           (not (eq jove-BRACE-L jove--tt)))
      (jove-add-child parent (jove-parse-maybe-assign))
    (jove-add-child parent (jove-parse-block))))

;; TODO: Combine `jove-parse-expr-list' and `jove-parse-binding-list'
(defun jove-parse-expr-list (node closing)
  "Parse a comma seperated list of expressions.
Return NODE with parsed expressions added as children.  Advance
parser over CLOSING, the token type which ends the list.  Always
allow empty expressions and trailing commas."
  (jove-parse-sequence closing
    (jove-add-child node (if (eq jove-ELLIPSIS jove--tt)
                         (jove-parse-spread)
                       (jove-parse-maybe-assign))))
  (jove-next)                               ; Move over CLOSING.
  node)                                 ; Return NODE.

(defun jove-parse-identifier (&optional liberal)
  "Parse token as an identifier.
If LIBERAL is non-nil keywords are converted to identifiers."
  (let ((node (jove-make-node)))
    (cond
     ((jove-tt-is-word jove--tt)
      (when (and jove--in-generator
                 (eq jove-YIELD jove--tt))
        (jove-warn jove--start jove--end "'yield' used as an identifier inside a generator"))
      (when (and jove--in-async
                 (eq jove-AWAIT jove--tt))
        (jove-warn jove--start jove--end "'await' used as an identifier inside an async function"))
      (jove-set-prop node :value jove--value))
     ((and liberal
           (jove-tt-is-keyword jove--tt))
      (jove-set-prop node :value (jove-tt-label jove--tt)))
     (t
      (jove-unexpected)))
    (jove-next)
    (jove-finish node 'identifier)))

(defun jove-parse-yield ()
  "Parse a yield expression."
  (let ((node (jove-make-node)))
    (jove-set-face jove--start jove--end 'font-lock-keyword-face)
    (jove-next)
    (unless (or (or (eq jove-SEMI jove--tt)
                    (jove-can-insert-semicolon-p))
                (and (not (eq jove-STAR jove--tt))
                     (not (jove-tt-starts-expr jove--tt))))
      (when (eq jove-STAR jove--tt)
        (jove-set-prop node :delegate t)
        (jove-set-face jove--start jove--end 'font-lock-keyword-face)
        (jove-next))                        ; Move over '*'
      (jove-add-child node (jove-parse-maybe-assign)))
    (jove-finish node 'yield-expression)))

(defun jove-parse-await ()
  "Parse an await expression."
  (let ((node (jove-make-node)))
    (jove-set-face jove--start jove--end 'font-lock-keyword-face)
    (jove-next)
    (jove-add-child node (jove-parse-maybe-unary nil))
    (jove-finish node 'await-expression)))

;;; Left Value Functions

;; FIXME: Needs to work with new vector node.
(defun jove-to-assignable (object)
  "Convert existing expression OBJECT to assignable pattern.
Updates node type, does not perform check for valid lvalues."
  (if (and (consp object)               ; Non-nil lists.
           (not (jove-node-p object)))
      (dolist (item object)
        (jove-to-assignable item))
    (when (jove-node-p object)
      (let ((type (jove-type object)))
        (cond
         ((and jove--in-declaration
               (eq 'identifier type))
          (jove-set-face* object 'font-lock-variable-name-face))
         ((eq 'object-expression type)
          (jove-set-type object 'object-pattern)
          (dolist (prop (jove-children object))
            ;; Second item in the list of children should be the
            ;; value of the property.
            (jove-to-assignable (car (jove-children prop))))) ; cadr?
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
  (jove-finish (jove-add-child (prog1 (jove-make-node)
                         (jove-next))
                       (jove-parse-maybe-assign nil))
           'spread-element))

(defun jove-parse-rest (&optional allow-non-identifier)
  "Parse rest element.
Optionally ALLOW-NON-IDENTIFIER arguments."
  (jove-finish (jove-add-child (prog1 (jove-make-node)
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
   ((jove-tt-is-word jove--tt)
    (when jove--in-declaration
      (jove-set-face jove--start jove--end 'font-lock-variable-name-face))
    (jove-parse-identifier))
   ((jove-is jove-BRACKET-L)
    ;; `jove-parse-binding-list' returns a regular list. The items need to
    ;; be added as children to the node.
    (jove-finish (jove-parse-binding-list (prog1 (jove-make-node)
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
                       (jove-parse-maybe-default jove--start nil))))
  (jove-next)                               ; Move over CLOSING.
  node)                                 ; Return NODE.

(defun jove-parse-maybe-default (start-pos &optional left)
  "Parse assignment pattern around a given atom if possible.
Begin the AssignmentPattern at START-POS.  Optionally supply LEFT
operand.  The boolean HIGHLIGHT flags to set variable name face."
  (setq left (or left (jove-parse-binding-atom)))
  (if (jove-is jove-EQ)
      (let ((node (jove-make-node start-pos)))
        (jove-set-prop node :op (jove-tt-label jove--tt))
        (jove-next)                         ; Move over '='
        (jove-finish (jove-add-children node
                                left
                                (jove-parse-maybe-assign))
                 'assignment-pattern))
    left))

;;; Statement Parsing Functions

(defun jove-is-let ()
  "Return non-nil if looking at a 'let'."
  (and (eq jove-LET jove--tt)
       (let ((tt (jove-tt (jove-peek))))
         (or (memq tt (list jove-BRACE-L jove-BRACKET-L))
             (jove-tt-is-word tt)))))

(defun jove-is-function ()
  "Return non-nil if looking at a function."
  (or (jove-is jove-FUNCTION)
      (jove-is-async-function)))

(defun jove-is-async-function ()
  "Return non-nil if looking at an 'async' function."
  (and (eq jove-ASYNC jove--tt)
       (let ((next (jove-peek)))
         (and (not (nth 4 next))        ; The 'newline-before' slot.
              (eq jove-FUNCTION (jove-tt next))))))

;; It seems what would be most helpful would be to not have the top
;; level actually be a node. It should be a stack in which top level
;; statements are pushed and when a change happens flush to before the
;; change.

;; Or if it is a node, when there is a change, nreverse its children,
;; pop off all that are after the change point. Set the cache-end and
;; token cache to the same position. Then re-nreverse the program's children,
;; and initiate a reparse from the end of the last complete statement.

;; Pretty sure that if the child causes an error it should not be added to the
;; ast

(defun jove-parse-top-level (node)
  "Parse a program.
Add top level statements as children to NODE."
  (let (token
        child)
    (condition-case err
        (progn
          (while (not (eq jove-EOB jove--tt))
            (setq token (jove-copy-lexer-state)
                  child (jove-parse-statement t))
            ;; Want to have a timer to check how long
            ;; we have been parsing. If longer than 1/60
            ;; of a second stop and give allow for input.
            ;; if so cancel idle reparse otherwise keep
            ;; chunking. Apply fontifications along the way.
            (jove-set-prop child :token token)
            (jove-add-child node child))
          (setq jove-ast-complete-p t
                jove--flushed-p nil))
      (jove-parse-error
       (when jove-debug (message "%s" (cadr err))))))
  (jove-finish node 'program))

(defun jove-parse-statement (&optional declaration)
  "Parse a single statement.
The boolean DECLARATION flags to permit a function or class
declaration."
  (let ((kind nil)
        (tt jove--tt)
        (node (jove-make-node)))
    (when (jove-is-let)
      (setq tt jove-VAR)
      (setq kind 'let)
      (jove-set-face jove--start jove--end 'font-lock-keyword-face))
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
      (setq kind (or kind (intern jove--value)))
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
      (jove-set-face jove--start jove--end 'font-lock-keyword-face)
      (jove-next)
      (jove-parse-function-statement node t))
     (t
      (let ((expr (jove-parse-expression)))
        (if (and (jove-tt-is-word tt)
                 (eq 'identifier (jove-type expr))
                 (jove-eat jove-COLON))
            (jove-parse-labeled-statement node expr)
          (jove-parse-expression-statement node expr)))))
    ;; Handle error if one exists, either way return NODE.
    (if jove--error
        (jove-handle-error node)
      node)))

(defun jove-parse-break-continue-statement (node tt)
  "Return NODE as either a 'break' or 'continue' statement.
Differentiate between the two using the token type TT."
  (jove-next)
  (unless (or (jove-is jove-SEMI)
              (jove-can-insert-semicolon-p))
    (if (not (jove-tt-is-word jove--tt))
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
     ((eq jove-SEMI jove--tt)
      (jove-parse-for node nil))
     ((or is-let (memq jove--tt (list jove-VAR jove-CONST)))
      (let ((init (jove-make-node))
            (kind (if is-let 'let (intern jove--value))))
        ;; Probably a better way... maybe in `jove-is-let'.
        (when is-let (jove-set-face jove--start jove--end 'font-lock-keyword-face))
        (jove-next)
        (jove-parse-var init t kind)
        (jove-finish init 'variable-declaration)
        (if (memq jove--tt (list jove-IN jove-OF))
            ;; There are a few more tests in parseForStatement
            ;; need to look though more thoroughly.
            (jove-parse-for-in node init)
          (jove-parse-for node init))))
     (t
      (let ((init (jove-parse-expression t)))
        (if (memq jove--tt (list jove-IN jove-OF))
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
    (while (not (eq jove-BRACE-R jove--tt))
      (if (or (setq is-case (eq jove-CASE jove--tt))
              (jove-is jove-DEFAULT))
          (progn
            (when current (jove-finish current 'switch-case))
            (jove-add-child node (setq current (jove-make-node)))
            (jove-next)
            (if is-case
                (jove-add-child current (jove-parse-expression))
              (jove-add-child current (jove-null-expression jove--end)))
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
  (jove-add-child node (if jove--newline-before
                       (jove-null-expression jove--end)
                     (jove-parse-expression)))
  (jove-semicolon)
  (jove-finish node 'throw-statement))

(defun jove-parse-try-statement (node)
  "Return NODE as a 'try' statement."
  (jove-next)
  (jove-add-child node (jove-parse-block))
  (when (eq jove-CATCH jove--tt)
    (let ((clause (jove-make-node)))
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

(defun jove-parse-labeled-statement (node expr)
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
    (setq node (jove-make-node)))
  (jove-expect jove-BRACE-L)
  (while (not (jove-eat jove-BRACE-R))
    (jove-add-child node (jove-parse-statement t)))
  (jove-finish node 'block-statement))

(defun jove-parse-for (node initializer)
  "Return NODE as a regular 'for' statement.
INITIALIZER is supplied by `jove-parse-statement'."
  (jove-add-child node initializer)
  (jove-expect jove-SEMI)

  (jove-add-child node (if (eq jove-SEMI jove--tt)
                       (jove-null-expression jove--end)
                     (jove-parse-expression)))
  
  (jove-expect jove-SEMI)
  (jove-add-child node (if (eq jove-PAREN-R jove--tt)
                       (jove-null-expression jove--end)
                     (jove-parse-expression)))
  (jove-expect jove-PAREN-R)
  
  (jove-add-child node (jove-parse-statement))
  (jove-finish node 'for-statement))

(defun jove-parse-for-in (node initializer)
  "Return NODE as either a 'for in' or 'for of' statement.
INITIALIZER is supplied by `jove-parse-statement'."
  (let ((type (if (eq jove-IN jove--tt) 'for-in-statement 'for-of-statement)))
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
        (jove--in-declaration t))
    (jove-set-prop node :kind kind)
    (while collecting
      ;; Skip empty declarators, less errors while editting.
      (unless (jove-eat jove-COMMA)
        (setq decl (jove-make-node))
        (jove-add-child decl (jove-parse-binding-atom))
        (when (eq jove-EQ jove--tt)
          (jove-next)
          ;; Add initializer as child if present.
          (jove-add-child decl (jove-parse-maybe-assign is-for)))
        (jove-add-child node (jove-finish decl 'variable-declarator))
        (unless (jove-eat jove-COMMA)
          (setq collecting nil))))
    node))

(defun jove-parse-function (node &optional is-stat allow-expr-body is-async)
  "Parse a function using the supplied NODE.
Depending on IS-STAT parse as declaration or literal.  The boolean flag
ALLOW-EXPR-BODY permits an expression body if parsing an arrow function.
The boolean flag IS-ASYNC is used to set the global `jove-in-async'."
  (let ((jove--in-function t)
        (jove--in-generator (when (and (not is-async)
                                   (jove-eat* jove-STAR 'font-lock-keyword-face))
                          t))           ; Set `jove--in-generator' to t.
        (jove--in-async is-async))
    (when is-async
      (jove-set-prop node :async t))
    (when (jove-tt-is-word jove--tt)
      (jove-set-face jove--start jove--end 'font-lock-function-name-face)
      (jove-set-prop node :id jove--value)
      (jove-next))
    (let ((jove--in-declaration t))
      (jove-parse-function-params node))
    (jove-parse-function-body node allow-expr-body)
    (jove-finish node (if is-stat
                           'function-statement
                         'function-expression))))

(defun jove-parse-function-params (parent)
  "Parse function parameters and add as a child to PARENT."
  (jove-add-child parent (jove-finish (jove-parse-binding-list (prog1 (jove-make-node)
                                                     (jove-expect jove-PAREN-L))
                                                   jove-PAREN-R)
                              'parameters)))

;; FIX: This needs to parse correctly.
;; toggleForm = () => {
;;     this.setState({ showNewCardForm: !this.state.showNewCardForm });
;;   };
(defun jove-parse-class (node &optional is-statement)
  "Return NODE as 'class' declaration or literal.
IF boolean flag IS-STATEMENT is non-nil parse as declaration."
  ;; Deciding on whether to worry about discerning between
  ;; methods and constructor.
  (jove-next)
  ;; Parse identifier.
  (when (jove-tt-is-word jove--tt)
    (jove-set-face jove--start jove--end 'font-lock-function-name-face)
    (jove-add-child node (jove-parse-identifier)))
  ;; Parse class super.
  (when (eq jove-EXTENDS jove--tt)
    (jove-add-child node (jove-finish (jove-add-child (prog1 (jove-make-node)
                                            (jove-next))
                                          (jove-parse-expr-subscripts))
                              'class-super)))
  (let ((key nil)
        (key-value)
        (method nil)
        (method-name nil)
        (method-body nil)
        (body (jove-make-node))
        (is-generator nil)
        (is-async nil)
        (is-static nil)
        (is-maybe-static nil))
    
    (jove-expect jove-BRACE-L)
    
    (while (not (jove-eat jove-BRACE-R))
      (unless (jove-eat jove-SEMI)              ; Continue.
        (setq method (jove-make-node)
              is-generator nil
              is-async nil
              is-static nil
              is-maybe-static (eq jove-STATIC jove--tt)
              is-generator (jove-eat* jove-STAR 'font-lock-keyword-face) ; Highlight if '*'.
              method-name (jove-parse-property-name method))
        (jove-set-prop method :static
                   (setq is-static (and is-maybe-static
                                        (not (eq jove-PAREN-L jove--tt))
                                        (not (jove-can-insert-semicolon-p)))))
        (when is-static
          ;; Get rid of 'static' as first child.
          (jove-set-children method '())
          ;; Highlight 'static'.
          (jove-set-face jove--prev-start jove--prev-end 'font-lock-keyword-face)
          ;; Don't care if is-generator was already assigned before.
          (setq is-generator (jove-eat* jove-STAR 'font-lock-keyword-face)) ; Highlight if '*'.
          ;; Reparse method key.
          (setq method-name (jove-parse-property-name method)))

        (when (and (not is-generator)
                   (not (eq jove-PAREN-L jove--tt))
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
                 (setq method-name (jove-parse-property-name method)))))

        (setq method-body (if (jove-eat jove-EQ)
                              (prog1 (jove-parse-expression)
                                (jove-semicolon))
                            (jove-parse-method is-generator is-async)))
        
        (jove-add-child method method-body)
        
        (jove-finish method)

        (if (memq (jove-type method-body)
                  '(function-expression arrow-function-expression))
            (progn
              (jove-set-type method 'method-definition)
              (jove-set-face* method-name 'font-lock-function-name-face))
          (jove-set-type method 'property)
          (jove-set-face* method-name 'js2-object-property))

        (jove-add-child body method)))
    
    (jove-add-child node (jove-finish body 'class-body))
    (jove-finish node (if is-statement 'class-declaration 'class-expression))))

(defun jove-parse-export (node)
  "Return NODE as 'export' declaration."
  (jove-next)
  (cond
   ((jove-eat* jove-STAR 'font-lock-keyword-face)
    (jove-eat* jove-FROM 'font-lock-keyword-face)
    (when (eq jove-STRING jove--tt)
      (jove-add-child node (jove-parse-expr-atom)))
    (jove-finish node 'export-all-declaration))
   ((jove-eat* jove-DEFAULT 'font-lock-keyword-face)
    (let (is-async)
      (cond
       ((or (eq jove-FUNCTION jove--tt)
            (setq is-async (jove-is-async-function)))
        (let ((f-node (jove-make-node)))
          (jove-next)
          (when is-async (jove-next))
          (jove-add-child node (jove-parse-function f-node t nil is-async))))
       ((eq jove-CLASS jove--tt)
        (jove-add-child node (jove-parse-class (jove-make-node) t)))
       (t
        (jove-add-child node (jove-parse-maybe-assign))
        (jove-semicolon))
       (jove-finish node 'export-default-declaration))))
   ((or (memq jove--tt (list jove-VAR jove-CONST jove-CLASS jove-FUNCTION))
        (jove-is-let)
        (jove-is-async-function))
    (jove-add-child node (jove-parse-statement t))
    (jove-finish node 'export-named-declaration))
   (t
    (when (jove-eat jove-BRACE-L)
      (let ((spec nil)
            (specs (jove-make-node)))       ; Specifiers
        (jove-parse-sequence jove-BRACE-R
          (setq spec (jove-make-node))
          (jove-add-child spec (jove-parse-identifier t))
          (when (jove-eat jove-AS)
            (jove-set-face jove--prev-start jove--prev-end 'font-lock-keyword-face)
            (jove-add-child spec (jove-parse-identifier t)))
          (jove-finish spec 'export-specifier)
          (jove-add-child specs spec))
        (jove-next)                         ; Move over '}'
        (jove-add-child node (jove-finish specs 'export-specifiers))))
    (when (jove-eat jove-FROM)
      (jove-set-face jove--prev-start jove--prev-end 'font-lock-keyword-face)
      (when (eq jove-STRING jove--tt)
        (jove-add-child node (jove-parse-expr-atom))))
    (jove-semicolon)
    (jove-finish node 'export-named-declaration))))

(defun jove-parse-import (node)
  "Return NODE as 'import' declaration."
  (jove-next)
  (if (eq jove-STRING jove--tt)
      (jove-add-child node (jove-parse-expr-atom))
    (let ((specs (jove-parse-import-specifiers (jove-make-node))))
      (jove-finish specs 'import-specifiers)
      (jove-add-child node specs)
      (when (jove-eat jove-FROM)
        (jove-set-face jove--prev-start jove--prev-end 'font-lock-keyword-face)
        (when (eq jove-STRING jove--tt)
          (jove-add-child node (jove-parse-expr-atom))))))
  (jove-semicolon)
  (jove-finish node 'import-declaration))

(defun jove-parse-import-specifiers (node)
  "Return NODE as a comma separated list of module imports."
  (catch 'specifiers
    (let (spec)
      (when (eq jove-NAME jove--tt)
        ;; import defaultObject, {foo, bar as qux} from 'boo'
        (setq spec (jove-make-node))
        (jove-set-face jove--start jove--end 'font-lock-variable-name-face)
        (jove-add-child spec (jove-parse-identifier))
        (jove-finish spec 'import-default-specifier)
        (jove-add-child node spec)
        (unless (jove-eat jove-COMMA)
          (throw 'specifiers node)))
      (when (eq jove-STAR jove--tt)
        (jove-set-face jove--start jove--end 'font-lock-keyword-face)
        (setq spec (jove-make-node))
        (jove-next)                         ; Move over '*'
        (when (jove-eat jove-AS)
          (jove-set-face jove--prev-start jove--prev-end 'font-lock-keyword-face)
          (when (eq jove-NAME jove--tt)
            (jove-set-face jove--start jove--end 'font-lock-variable-name-face)
            (jove-add-child spec (jove-parse-identifier))))
        (jove-finish spec 'import-namespace-specifier)
        (jove-add-child node spec))
      (when (jove-eat jove-BRACE-L)
        (jove-parse-sequence jove-BRACE-R
          (setq spec (jove-make-node))
          (jove-add-child spec (jove-parse-identifier t))
          (if (jove-eat jove-AS)
              (progn
                (jove-set-face jove--start jove--end 'font-lock-keyword-face)
                (when (jove-tt-is-word jove--tt)
                  (jove-set-face jove--start jove--end 'font-lock-variable-name-face)
                  (jove-add-child spec (jove-parse-identifier))))
            (jove-set-face jove--prev-start jove--prev-end 'font-lock-variable-name-face))
          (jove-finish spec 'import-specifier)
          (jove-add-child node spec))
        (jove-next)))                       ; Move over '}'
    node))                              ; Return NODE.

(defun jove-jsx-parse-identitier ()
  "Parse JSX identifier."
  (let ((node (jove-make-node)))
    (cond
     ((eq jove-JSX-NAME jove--tt)
      (jove-set-prop node :value jove--value))
     ((jove-tt-is-keyword jove--tt)
      (jove-set-prop :value (jove-tt-label jove--tt)))
     (t
      (jove-unexpected)))
    (jove-next)
    (jove-finish node 'jsx-identifier)))

(defun jove-jsx-parse-namespaced-name ()
  "Parse JSX namespaced identifier."
  (let ((start-pos jove--start)
        (name (jove-jsx-parse-identitier)))
    (if (jove-eat jove-COLON)
        (jove-finish (jove-add-children (jove-make-node start-pos)
                                name
                                (jove-jsx-parse-identitier))
                 'jsx-namespaced-name)
      name)))

(defun jove-jsx-parse-element-name ()
  "Parse JSX element name."
  (let ((start-pos jove--start)
        (node (jove-jsx-parse-namespaced-name)))
    (while (jove-eat jove-DOT)
      (setq node (jove-finish (jove-add-children (jove-make-node start-pos)
                                         node
                                         (jove-jsx-parse-identitier))
                          'jsx-member-expression)))
    node))

(defun jove-jsx-parse-attribute-value ()
  "Parse JSX attribute value."
  (cond
   ((eq jove-BRACE-L jove--tt)
    (jove-jsx-parse-expression-container))
   ((or (eq jove-JSX-TAG-START jove--tt)
        (eq jove-STRING jove--tt))
    (jove-parse-expr-atom))
   (t
    (jove-unexpected :jsx-value-not-brace-expr-or-string))))

(defun jove-jsx-parse-empty-expression ()
  "Parse an empty JSX expression."
  ;; <tag>|...|</tag>
  (jove-make-node jove--prev-end jove--start 'jsx-empty-expression))

(defun jove-jsx-parse-expression-container ()
  "Parse a JSX expression container."
  (let ((node (jove-make-node)))
    (jove-next)                             ; Move over '{'
    (jove-add-child node (if (eq jove-BRACE-R jove--tt)
                         (jove-jsx-parse-empty-expression)
                       (jove-parse-expression)))
    (jove-expect jove-BRACE-R)
    (jove-finish node 'jsx-expression-container)))

(defun jove-jsx-parse-attribute ()
  "Parse a JSX attribute."
  (let ((node (jove-make-node)))
    (if (jove-eat jove-BRACE-L)
        ;; Allow invalid input.
        (cond
         ((eq jove-BRACE-R jove--tt)
          (jove-add-child node (jove-jsx-parse-empty-expression))
          (jove-expect jove-BRACE-R))
         ((jove-eat jove-ELLIPSIS)
          (jove-add-child node (unless (eq jove-BRACE-R jove--tt)
                             (jove-parse-maybe-assign)))
          (jove-expect jove-BRACE-R))
         (t
          (jove-unexpected)))
      (let ((name (jove-jsx-parse-namespaced-name)))
        (if (eq 'jsx-identifier (jove-type name))
            ;; Fontify all attributes the same
            (jove-set-face* name 'font-lock-variable-name-face)
          (jove-fontify-jsx-tag-name name))
        (jove-add-children node
                       name
                       (when (jove-eat jove-EQ)
                         (jove-jsx-parse-attribute-value)))))
    (jove-finish node 'jsx-attribute)))

(defun jove-jsx-parse-opening-element-at (start-pos)
  "Parse JSX opening tag."
  ;; Starting after '<'
  (let ((node (jove-make-node start-pos))
        (name (jove-jsx-parse-element-name)))
    (jove-fontify-jsx-tag-name name)
    (jove-add-child node name)
    (while (and (not (eq jove-SLASH jove--tt))
                (not (eq jove-JSX-TAG-END jove--tt)))
      (jove-add-child node (jove-jsx-parse-attribute)))
    (jove-set-prop node :self-closing (jove-eat jove-SLASH))
    (jove-expect jove-JSX-TAG-END)
    (jove-finish node 'jsx-opening-element)))

(defun jove-jsx-parse-closing-element-at (start-pos)
  "Parse JSX closing element."
  ;; Starting after '</'
  (let ((node (jove-make-node start-pos))
        (name (jove-jsx-parse-element-name)))
    (jove-fontify-jsx-tag-name name)
    (jove-add-child node name)
    (jove-expect jove-JSX-TAG-END)
    (jove-finish node 'jsx-closing-element)))

(defun jove-jsx-parse-element-at (start-pos)
  "Parse entire JSX element.
Inludes opening tag (starting after '<'), attributes, contents
and closing tag."
  (let ((node (jove-make-node start-pos))
        (opening (jove-jsx-parse-opening-element-at start-pos))
        (closing nil))
    (jove-add-child node opening)
    (when (not (jove-get-prop opening :self-closing))
      (catch 'contents
        (while t
          (cond
           ((eq jove-JSX-TAG-START jove--tt)
            (setq start-pos jove--start)
            (jove-next)
            (when (jove-eat jove-SLASH)
              (setq closing (jove-jsx-parse-closing-element-at start-pos))
              (throw 'contents nil))
            (jove-add-child node (jove-jsx-parse-element-at start-pos)))
           ((eq jove-JSX-TEXT jove--tt)
            ;; FIX: Why not just call `jove-jsx-parse-text'.
            (jove-add-child node (jove-parse-expr-atom)))
           ((eq jove-BRACE-L jove--tt)
            (jove-add-child node (jove-jsx-parse-expression-container)))
           (t
            (jove-unexpected)))))
      (jove-add-child node closing)
      (when (not (string= (jove-get-qualified-jsx-name (car (jove-children closing)))
                          (jove-get-qualified-jsx-name (car (jove-children opening)))))
        (jove-warn (jove-start closing)
               (jove-end closing)
               (concat "Expected corresponding JSX closing tag for <"
                       (jove-get-qualified-jsx-name (car (jove-children opening)))
                       ">"))))

    ;; TODO: Consider adding the ablity to parse invalid adjacent
    ;; JSX elements.
    
    ;; Think about... Because an expression was not allowed the lexer
    ;; did not update the token context to enter a JSX element. That
    ;; would need to be modified. So far the parser makes no modifications
    ;; to the lexer. Perhaps I should add a function to the lexer to
    ;; control updates to its state, rather than mucking around with
    ;; it here.
    
    (while (and (eq jove-RELATIONAL jove--tt)
                (string= "<" (jove-token-raw)))
      (jove-signal "Invalid adjacent JSX elements" :start jove--start :end jove--end))
    (jove-finish node 'jsx-element)))

(defun jove-jsx-parse-text ()
  "Return literal JSX text element."
  (prog1 (jove-make-node jove--start jove--end 'jsx-text)
    (jove-next)))

(defun jove-jsx-parse-element ()
  (let ((start-pos jove--start))
    (jove-next)                             ; Move over '<'
    (jove-jsx-parse-element-at start-pos)))

(defun jove-get-qualified-jsx-name (object)
  "Transform JSX element name to string."
  (let ((type (jove-type object)))
    (cond
     ((eq 'jsx-identifier type)
      (jove-get-prop object :value))
     ((eq 'jsx-namespaced-name type)
      (concat (jove-get-prop (car (jove-children object)) :value) ":"
              (jove-get-prop (car (cdr (jove-children object))) :value)))
     ((eq 'jsx-member-expression type)
      (concat (jove-get-qualified-jsx-name (car (jove-children object))) "."
              (jove-get-qualified-jsx-name (car (cdr (jove-children object)))))))))

(defun jove-fontify-jsx-tag-name (object &optional chainp namespacep)
  "Fontify JSX element name to string."
  (let ((type (jove-type object)))
    (cond
     ((eq 'jsx-identifier type)
      (jove-set-face* object (cond (chainp 'js2-object-property)
                               (namespacep 'font-lock-type-face)
                               (t 'font-lock-function-name-face))))
     ((eq 'jsx-namespaced-name type)
      (jove-fontify-jsx-tag-name (car (jove-children object)) nil t)
      (jove-fontify-jsx-tag-name (car (cdr (jove-children object)))))
     ((eq 'jsx-member-expression type)
      (jove-fontify-jsx-tag-name (car (jove-children object)) chainp)
      (jove-fontify-jsx-tag-name (car (cdr (jove-children object))) t)))))

(defun jove-parse ()
  "Run the Jove parser."
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (jove-config)
      (let ((start-pos (point))
            (start-time (float-time)))
        (save-match-data
          (setq jove-ast (jove-parse-top-level jove-ast)))
        (when jove-verbose
          (let ((time (/ (truncate (* (- (float-time) start-time)
                                      10000))
                         10000.0)))
            (message "Parser finished in %0.3fsec" time)))
        (jove-apply-fontifications start-pos (point))))))

;; TODO: Need function to find first node that starts at a position,
;; not to go as deep into the tree as possible.

;; works... though don't know how much more useful it is.
(defun jove-x-find-node-at (node pos)
  "Return NODE or a child of NODE at found at POS.
Return nil if nothing is found."
  (when (and (<= (jove-start node) pos)
             (> (jove-end node) pos))
    (jove-x-find-child-at (jove-children node) pos)))

(defun jove-x-find-child-at (children pos)
  "Search for first child in CHILDREN to be within POS."
  (let ((child (car children)))
    (cond
     ((null child)
      nil)
     ((and (<= (jove-start child) pos)
           (> (jove-end child) pos))
      (cons (jove-type child) (jove-x-find-node-at child pos)))
     (t
      (jove-x-find-child-at (cdr children) pos)))))

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

(defun jove-find (pos)
  (when jove-ast-complete-p
    (jove-find-node-at jove-ast pos)))

;; Process
;; Initial
;; - Attempt to create full AST.
;; - Add children in a way in which a partial ast is possible if there is an error.
;; - Create a token cache while parsing.
;; - make the process async'ish. pause every 1/30 of a second to wait for input.
;;   If input flush to the last statement before that position.

;; On a change
;; - Flush both ast and token cache.
;; - Set timer for reparse.

;; On idle reparse
;; - Read one token using the top state from the token cache.
;;   The parser is always one token ahead.
;; - Begin added statements to the ast.

;; What needs to happen but is not is a comparison of the
;; cache, the ast and the start of the change. Maybe after
;; finding the 'end' if the lexer cache doesn't match up
;; with the end of the ast node clear everything and start
;; over.

;; TODO: Figure out how to determine if an edit has been made
;; in a previously parsed statement, and if flushing the cache
;; results in being located at the end of the statement directly
;; above the current. If so attempt to parse the current statement
;; right away, keep track of the time, if taking too long 1/60 of
;; a second, ditch. Either way schedule a reparse. This would allow small
;; statements to be highlighting instantly.

;; What might actually be more robust would be to cache the token at the
;; beginning of each top level statement. It would be the easiest place
;; to know to make a copy. when a change is detected go to the last top
;; level statement and use the saved token...

;; Need some sort of protection that the children do not get double
;; reversed.
(defun jove-flush-ast (position)
  "Flush top level statements from `jove-ast' to POSITION."
  (let ((token nil)
        (children (if jove--flushed-p
                      (jove-children jove-ast)  ; Already reversed
                    (nreverse (jove-children jove-ast))))
        (looking t))
    (while looking
      (cond
       ((null children)
        (setq looking nil))
       ;; If POSITION is less that the end of the node, decrement
       ;; the list. 
       ((<= position (jove-start (car children)))
        (setq children (cdr children)))
       ;; If POSITION is greater than the end of the initial token
       ;; of the statement, finish. Decrement the list, set token to
       ;; the cached token, and stop looking
       ((> position (jove-end (jove-get-prop (car children) :token)))
        (setq token (jove-get-prop (car children) :token)
              children (cdr children)
              looking nil))
       ;; If we reach this, the edit must have been inside or directly
       ;; after the initial token.
       (t
        (setq children (cdr children)))))
    
    (setq jove--cache token                 ; Either nil or a token.
          jove--flushed-p t)
    (setf (nth 1 jove-ast) nil)             ; End
    (setf (nth 5 jove-ast) children)))      ; Children

(provide 'jove-parser)

;;; jove-parser.el ends here
