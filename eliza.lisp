;;;
;;; eliza.lisp
;;;

(defconstant fail nil "Indicates pat-match failure")

(defconstant no-bindings '((t . t))
  "Indicates pat-match success with no variables")

(defun simple-equal (x y)
    (if (or (atom x) (atom y))
        (eql x y)
        (and (simple-equal (car x) (car y))
             (simple-equal (cdr x) (cdr y)))))

(defun starts-with (list x)
    (and (consp list) (eql (car list) x)))

(defun segment-pattern-p (pattern)
    "Is this a segment matching pattern: ((?* var) . pat)"
    (and (consp pattern)
         (starts-with (car pattern) '?*)))

(defun variable-p (x)
    (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun get-binding (var bindings)
    "Find a (variable . value) pair in a binding list"
    (assoc var bindings))

(defun binding-val (binding)
    (cdr binding))

(defun lookup (var bindings)
    "Get the value part (for var) from a binding list."
    (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
    (cons (cons var val)
          (if (eq bindings no-bindings)
              nil
              bindings)))

(defun pat-match (pattern input &optional (bindings no-bindings))
    "Match pattern against input in the context of the bindings"
    (cond ((eq bindings fail) fail)
          ((variable-p pattern)
           (match-variable pattern input bindings))
          ((eql pattern input) bindings)
          ((segment-pattern-p pattern)
           (segment-match pattern input bindings))
          ((and (consp pattern) (consp input))
           (pat-match (cdr pattern) (cdr input)
                      (pat-match (car pattern) (car input)
                                 bindings)))
          (t fail)))

(defun match-variable (var input bindings)
    "Does VAR match input? Uses (or updates) and returns bindings."
    (let ((binding (get-binding var bindings)))
        (cond ((not binding) (extend-bindings var input bindings))
              ((equal input (binding-val binding)) bindings)
              (t fail))))


(defun segment-match (pattern input bindings &optional (start 0))
    "Match the segment pattern ((?* var) . pat) against input."
    (let ((var (second (car pattern)))
          (pat (cdr pattern)))
        (if (null pat)
            (match-variable var input bindings)
            ;; We assume that pat starts with a constant
            ;; In other words, a pattern can't have 2 consecutive vars
            (let ((pos (position (car pat) input
                                 :start start :test #'equal)))
                (if (null pos)
                    fail
                    (let ((b2 (pat-match
                               pat (subseq input pos)
                               (match-variable var (subseq input 0 pos)
                                               bindings))))
                        ;; if this filded, try another longer one
                        (if (eq b2 fail)
                            (segment-match pattern input bindings (+ pos 1))
                            b2)))))))

(defun rule-pattern (rule) (car rule))
(defun rule-responses (rule) (cdr rule))

(defun eliza ()
    "Respond to user input using pattern matching rules."
    (loop
      (print 'eliza>)
      (write (flatten (use-eliza-rules (read))) :pretty t)))

(defun random-elt (list)
    (elt list (random (length list))))

(defun use-eliza-rules (input)
    "Find some rule with which to transform the input."
    (some #'(lambda (rule)
                (let ((result (pat-match (rule-pattern rule) input)))
                    (if (not (eq result fail))
                    	(sublis (switch-viewpoint result)
                                (random-elt (rule-responses rule))))))
          *eliza-rules*))

(defun switch-viewpoint (words)
    "Change i to you and vice versa, and so on."
    (sublis '((i . you) (you . i) (me . you) (am . are))
            words))

(defun flatten (the-list)
    "Append together elements (or lists) in the list."
    (mappend #'mklist the-list))

(defun mklist (x)
    "Return x if it is a list, otherwise (x)."
    (if (listp x)
        x
        (list x)))

(defun mappend (fn the-list)
    "Apply fn to each element of list and append the results."
    (apply #'append (mapcar fn the-list)))

(defparameter *eliza-rules*
  '((((?* ?x) hello (?* ?y))
     (How do you do. Please state your problem.))
    (((?* ?x) I want (?* ?y))
     (What would it mean if you got ?y)
     (Why do you want ?y) (Suppose you got ?y soon))
    (((?* ?x) if (?* ?y))
     (Do you realy think its likely that ?y) (Do you wish that ?y)
     (What do you think about ?y) (Really-- if ?y))
    (((?* ?x) no (?* ?y))
     (Why not?) (You are being a bit negative))
    (((?* ?x) I was (?* ?y))
     (Were you really?) (Perhaps i already knew you were ?y))
    (((?* ?x) i feel (?* ?y))
     (Do you often feel ?y ?))
    (((?* ?x) i felt (?* ?y))
     (What other feelings do you have?))))
