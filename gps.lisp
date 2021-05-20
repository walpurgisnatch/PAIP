(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
    "Print debugging info if (DEBUG ID) has been specified."
    (when (member id *dbg-ids*)
        (fresh-line *debug-io*)
        (apply #'format *debug-io* format-string args)))

(defun debug (&rest ids)
    "Start dbg output on the given ids."
    (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug (&rest ids)
    "Stop dbg on the ids. With no ids, stop dbg altogether."
    (setf *dbg-ids* (if (null ids) nil
                        (set-difference *dbg-ids* ids))))

(defun dbg-indent (id indent format-string &rest args)
    "Print indented debugging info if (DEBUG ID) has been specified."
    (when (member id *dbg-ids*)
        (fresh-line *debug-io*)
        (dotimes (i indent) (princ "  " *debug-io*))
        (apply #'format *debug-io* format-string args)))

(defvar *ops* nil
  "A list of available operators.")

(defstruct op
  "An operation"
  (action nil)
  (preconds nil)
  (add-list nil)
  (del-list nil))

(defun GPS (state goals &optional (ops *ops*))
    "General Problem Solver: achieve all goals using *ops*."
    (let ((old-ops *ops*))
        (setf *ops* ops)
        (let ((result (remove-if #'atom (achieve-all
                                         (cons '(start) state)
                                         goals nil))))
            (setf *ops* old-ops)
            result)))

(defun executing-p (x)
    "Is x of the form: (executing ...)?"
    (starts-with x 'executing))

(defun starts-with (list x)
    "Is this a list whose first element is x?"
    (and (consp list) (eql (car list) x)))

(defun convert-op (op)
    "Make op conform to the (EXECUTING op) convention."
    (unless (some #'executing-p (op-add-list op))
        (push (list 'executing (op-action op)) (op-add-list op)))
    op)

(defun op (action &key preconds add-list del-list)
    "Make a new operator that obeys the (EXECUTING op) convention."
    (convert-op
     (make-op :action action :preconds preconds
              :add-list add-list :del-list del-list)))

(defun member-equal (item list)
    (member item list :test #'equal))

(defun achieve-all (state goals goal-stack)
    "Try to achieve each goal, then make sure they still hold."
    (let ((current-state state))
        (if (and (every #'(lambda (g)
                              (setf current-state
                                   (achieve current-state g goal-stack)))
                        goals)
                 (subsetp goals current-state :test #'equal))
            current-state)))

(defun achieve (state goal goal-stack)
    "A goal is achieved if it already holds, or
    if there is an appropriate op for it that is applicable."
    (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
    (cond ((member-equal goal state) state)
          ((member-equal goal goal-stack) nil)
          (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                   (find-all goal *ops* :test #'appropriate-p)))))

(defun apply-op (state goal op goal-stack)
    "Print a message and update *state* if op is applicable."
    (dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
    (let ((state2 (achieve-all state (op-preconds op)
                               (cons goal goal-stack))))
        (unless (null state2)
            ;; Return an updated state
            (dbg-indent :gps (length goal-stack) "Action: ~a" (op-action op))
            (append (remove-if #'(lambda (x)
                                     (member-equal x (op-del-list op)))
                               state2)
                    (op-add-list op)))))

(defun appropriate-p (goal op)
    "An op is appropriate to a goal if it is in its add list."
    (member-equal goal (op-add-list op)))

(defun use (oplist)
    (length (setf *ops* oplist)))

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
      '(give-shop-money (have-money) (shop-has-money) (have-money))
      '(ask-phone-number (in-communication-with-shop) (know-phone-number))))
