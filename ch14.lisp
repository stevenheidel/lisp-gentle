; 14.3
(defmacro set-nil (var)
  (list 'setf var nil))

; 14.4
(defmacro simple-rotatef (a b)
  `(let ((temp-a ,a)
         (temp-b ,b))
     (setf ,a temp-b)
     (setf ,b temp-a)))

; 14.5
(defmacro set-mutual (a b)
  `(progn
    (setf ,a ',b)
    (setf ,b ',a)))

; 14.6
(defmacro variable-chain (&rest variables)
  `(progn ,@(mapcar
              (lambda (var val) `(setf ,var ',val))
              variables (rest variables))))

; 14.11
(defstruct (node (:print-function print-node))
  (name nil)
  (inputs nil)
  (outputs nil))

(defun print-node (node stream depth)
  (declare (ignore depth))
  (format stream "#<Node ~A>" (node-name node)))

(defstruct (arc (:print-function print-arc))
  (from nil)
  (to nil)
  (label nil)
  (action nil))

(defun print-arc (arc stream depth)
  (declare (ignore depth))
  (format stream "#<ARC ~A / ~A / ~A>"
    (node-name (arc-from arc))
    (arc-label arc)
    (node-name (arc-to arc))))

(defvar *nodes*)
(defvar *arcs*)
(defvar *current-node*)

(defun initialize ()
  (setf *nodes* nil)
  (setf *arcs* nil)
  (setf *current-node* nil))

(defmacro defnode (name)
  `(add-node ',name))

(defun add-node (name)
  (let ((new-node (make-node :name name)))
    (setf *nodes* (nconc *nodes* (list new-node)))
    new-node))

(defun find-node (name)
  (or (find name *nodes* :key #'node-name)
      (error "No node named ~A exists." name)))

(defmacro defarc (from label to &optional action)
  `(add-arc ',from ',label ',to ',action))

(defun add-arc (from-name label to-name action)
  (let* ((from (find-node from-name))
         (to (find-node to-name))
         (new-arc (make-arc :from from
                            :label label
                            :to to
                            :action action)))
    (setf *arcs* (nconc *arcs* (list new-arc)))
    (setf (node-outputs from)
          (nconc (node-outputs from) (list new-arc)))
    (setf (node-inputs to)
          (nconc (node-inputs to) (list new-arc)))))

(defnode start)
(defnode have-5)
(defnode have-10)
(defnode have-15)
(defnode have-20)
(defnode end)

(defarc start   nickel      have-5  "Clunk!")
(defarc start   dime        have-10 "Clink!")
(defarc start   coin-return start   "Nothing to return.")
(defarc have-5  nickel      have-10 "Clunk!")
(defarc have-5  dime        have-15 "Clink!")
(defarc have-5  coin-return start   "Returned five cents.")
(defarc have-10 nickel      have-15 "Clunk!")
(defarc have-10 dime        have-20 "Clink!")
(defarc have-10 coin-return start   "Returned ten cents.")
(defarc have-15 nickel      have-20 "Clunk!")
(defarc have-15 dime        have-20 "Nickel change.")
(defarc have-15 gum-button  end     "Deliver gum.")
(defarc have-15 coin-return start   "Returned fifteen cents.")
(defarc have-20 nickel      have-20 "Nickel returned.")
(defarc have-20 dime        have-20 "Dime returned.")
(defarc have-20 gum-button  end     "Deliver gum, nickel change.")
(defarc have-20 mint-button end     "Deliver mints.")
(defarc have-20 coin-return start   "Returned twenty cents.")

; (a)
(defun compile-arc (arc)
  `((equal this-input ',(arc-label arc))
    (format t "~&~A" ,(arc-action arc))
    (,(node-name (arc-to arc)) (rest input-syms))))

; (b)
(defun compile-node (node)
  `(defun ,(node-name node) (input-syms &aux (this-input (first input-syms)))
     (cond ((null input-syms) ',(node-name node))
           ,@(mapcar #'compile-arc (node-outputs node))
           (t (error "No arc from ~A with label ~A."
                ',(node-name node) this-input)))))

; (c)
(defmacro compile-machine ()
  `(progn ,@(mapcar #'compile-node *nodes*)))
