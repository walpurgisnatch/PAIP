(defvar *state* nil
  "The current state: a list of conditions.")

(defvar *ops* nil
  "A list of available operators.")

(defstruct op
  "An operation"
  (action nil)
  (preconds nil)
  (add-list nil)
  (del-list nil))

(defun GPS (*state* goals *ops*)
    "General Problem Solver: achieve all goals using *ops*."
    (if (every #'achieve goals) 'solved))

(defun achieve (goal)
    "A goal is achieved if it already holds, or
    if there is an appropriate op for it that is applicable."
    (or (member goal *state*)
        (some #'apply-op
              (find-all goal *ops* :test #'appropriate-p))))

(defun achieve-all (goals)
    "Try to achieve each goal, then make sure they still hold."
    (and (every #'achieve goals) (subsetp goals *state*)))

(defun appropriate-p (goal op)
    "An op is appropriate to a goal if it is in its add list."
    (member goal (op-add-list op)))

(defun apply-op (op)
    "Print a message and update *state* if op is applicable."
    (when (every #'achieve (op-preconds op))
        (print (list 'executing (op-action op)))
        (setf *state* (set-difference *state* (op-del-list op)))
        (setf *state* (union *state* (op-add-list op)))
        t))

(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
    (if test-not
        (apply #'remove item sequence
               :test-not (complement test-not) keyword-args)
        (apply #'remove item sequence
               :test (complement test) keyword-args)))

(defmacro def-op (action preconds add del)
    `(make-op :action ,action
              :preconds ,preconds
              :add-list ,add
              :del-list ,del))

(defmacro def-ops (&body forms)
    `(list
      ,@(loop for f in forms collect `(def-op (nth 0 ,f) (nth 1 ,f) (nth 2 ,f) (when (nth 3 ,f) (nth 3 ,f))))))

(defparameter *school-ops*
  (def-ops
      '(drive-son-to-school (son-at-home car-works) (son-at-school) (son-at-home))
      '(shop-installs-battery (car-needs-battery shop-knows-problem shop-has-money) (car-works))
      '(tell-shop-problem (in-communication-with-shop) (shop-knows-problem))
      '(telephone-shop (know-phone-number) (in-communication-with-shop))
      '(look-up-number (have-phone-book) (know-phone-number))
      '(give-shop-money (have-money) (shop-has-money) (have-money))))
