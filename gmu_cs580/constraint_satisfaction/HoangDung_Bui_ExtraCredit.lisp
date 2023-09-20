;;; Hoang Dung Bui
;;; G01301478
;;; Constraint Satisfaction Project - extra-credit


;;;
;;; GENERAL FUNCTIONS
;;;
;;; These three functions might be of use to you.

(defun coin-toss (&optional (probability 0.5))
  "Returns a coin-toss of the given probability"
  (> (random 1.0) probability))

(defun best-position (predicate sequence &optional
                       (use-test #'(lambda (elt pos)
                             (declare (ignorable elt pos)) t)))
  "Returns the index of the best position in a sequence given
the predicate.  If the sequence is empty, returns nil.  The
predicate takes FOUR arguments: (1) the value of elt0.  (2)
the index of elt0 in the sequence.  (3) the value of elt1.
 (4) the index of elt1 in the sequence.  If elt0 is 'better'
than elt1, the predicate should return t, else it should
return nil.  use-test is an optional predicate which
takes two values, the (1) value of an elt and the (2)
position of the elt.  If it returns T then we should include
this element in the contest."
  (let (best best-i (cur 0))
    (map nil #'(lambda (elt)
         (when (funcall use-test elt cur)
           (when (or (not best-i)
                 (funcall predicate elt cur best best-i))
             (setf best elt)
             (setf best-i cur)))
         (incf cur))
     sequence)
    best-i))

(defun all-different-p (&rest vals)
  "Using EQUALP, returns t if each and every element in VALS is different"
  (mapl #'(lambda (lis)
        (mapcar #'(lambda (item)
            (when (equalp item (first lis))
              (return-from all-different-p nil)))
            (rest lis)))
    vals)
  t)



;;;
;;; THE DEFINITION OF A 'CONSTRAINT'
;;;

;;; A constraint is a structure with three parts:
;;; 1. A list of all the variables in your problem
;;; 2. A list of all the variables from #1 which are involved in the constraint
;;; 3. A predicate which, when passed a value-list, returns T if it doesn't
;;;    violate the constraint
;;;
;;; Originally I had constraints being just the predicate -- but it turns
;;; out that it's convenient to include the variable declarations in the
;;; constraint as well


(defstruct constraint
  "A constraint structure.  The only important item REALLY is the
predicate, which takes a value-list and returns T if the value-list
is consistent with the constraint.  variables is a list of variables
involved in the constriaint if that might be useful to you.
all-variables is a list of all variables in the problem, in order, also
if that's useful to you."

  all-variables
  variables
  predicate
  )



(defun value-of (variable)
  "Private function for new-constraint -- do not use this"
  (unless (second variable)
    (first variable)))

(defmacro new-constraint ((&rest vars) (&rest relevant-vars) &rest body)
  "Convenient format for building a constraint.  You specify ALL the
variables that exist in the entire problem, in order, in VARS.  Then
you specify the variables that are relevant to your constraint in
RELEVANT-VARS.  Then you make some body code.  Assume in your body code
that all the RELEVANT-VARs will have been checked to make sure they're
bound to a value already [you don't need to check that].  The body code
is then executed in a lexical environment with local variables of the
same symbol as each of the RELEVANT-VARS, and those local variables
are set to the values their equivalent RELEVANT-VARS are bound to."
  (let ((value-list (gensym))
    (variable (gensym)))
    `(make-constraint
      :all-variables ',vars
      :variables ',relevant-vars
      :predicate #'(lambda (,value-list)
             ;; destructuring-bind is like LET, but it binds
             ;; variables to certain locations within a list
             ;; or sublists.  I use it here to set local
             ;; variable names -- conveniently the same as
             ;; the variable names in VARS -- to the current values.
             ;; Nifty!
             (destructuring-bind ,vars
             ;; here we build our list of stuff to bind the variables to
             (mapcar #'(lambda (,variable) (value-of ,variable))
                 ,value-list)
               (declare (ignorable ,@vars))
               ;; here we test to see if all the relevant variables are bound --
               ;; if not, we automatically pass the constraint
               (or (not (every #'identity (list ,@relevant-vars)))
               ;; now we test the constraint
               ,@body))))))






;;; HEURISTICS FOR PICKING VARIABLES

;;;
;;; THE PICK-RANDOM-VARIABLE HEURISTIC
;;;
;;; okay, so it's not a heuristic.  It's our default

;;; Written for you

(defun pick-random-variable (value-list constraints)
  "Picks a random variable that's legal"
  (declare (ignorable constraints))
  (let ((len (length value-list)) var)
    (loop until (second (elt value-list (setf var (random len)))))
    var))


;;;
;;; THE PICK-FIRST-VARIABLE HEURISTIC
;;;
;;; Also not a heuristic.  :-)  Just picks the first available
;;; variable.  AMAZINGLY, the ordering of our variables is
;;; coincidentally set up such that this does a VERY GOOD JOB!
;;; I hadn't realized this and was just hung up on that for a
;;; very long time, sorry.

;;; Written for you

(defun pick-first-variable (value-list constraints)
  "Picks the first variable that's legal"
  (declare (ignorable constraints))
  (position-if #'rest value-list))



;;;
;;; THE MINIMUM REMAINING VALUES (MRV) AND DEGREE HEURISTICS
;;;
;;; You need to write a heuristic which picks according to MRV,
;;; using Degree to break ties, and picking randomly to break
;;; further ties beyond that.  Here are the functions that
;;; I had used, perhaps their templates might be of use to you.

(defun num-involved-constraints (variable-index constraints)
  "Returns the number of constraints which use the variable
of the given variable-index"
  ;; useful for the degree heuristic
  (let (var (count 0))
    ;; get the variable
    (setf var (elt (constraint-all-variables (elt constraints 0)) variable-index))
    (dolist (item constraints)
      (dolist (item1 (constraint-variables item))
	(if (equalp item1 var)
	    (setf count (+ count 1)))))
    count))


(defun legal-values (variable-index value-list constraints)
  "Returns the number of legal values which the variable-index
can hold, given the existing constraints"
  ;; useful for the mrv heuristic
  (let ((legal-count 0) (new-value-list nil))
    (dotimes (i (length (elt value-list variable-index)))
      (setf new-value-list (bind-variable value-list variable-index i))
      (if (consistent-p new-value-list constraints)
	  (setf legal-count (+ legal-count 1))))
    ;;legal-count))
    (length (elt value-list variable-index))))

(defun mrv-d (value-list constraints)
  "Picks a variable from among the NON-BOUND values in
value-list according to the Minimum Remaining Values
Heuristic, breaking ties with the Degree Heuristic"
  ;; best-position might be useful to you here.
  (let ((count 0) (mrv nil) (reverse-list nil) (min 0) (index-list nil) (max 0) pos inv1 inv2 var-index)
    (dotimes (i (length value-list))
      (setf count (legal-values i value-list constraints))
      (if (> count max)
	  (setf max count))
      ;; push the value into the mrv list
      (cond ((= 0 (length mrv))
	     (push count mrv))
	    ((< 0 (length mrv))
	     (setf reverse-list (reverse mrv))
	     (push count reverse-list)
	     (setf mrv (reverse reverse-list)))))
    (setf min max)
    ;; determine the minimum legal-values
    (dotimes (j (length mrv))
      (if (and (> min (elt mrv j)) (> (elt mrv j) 1))
	  (setf min (elt mrv j))))
    
    ;; determine the index of the minimum numbers
    (setf pos (position min mrv :test #'equal))
    (push pos index-list)
    (setf pos (position min mrv :test #'equal :from-end t))
    (if (not (= pos (elt index-list 0)))
	(push pos index-list))
    (setf var-index (elt index-list 0))
    ;;; check if there is more than 1 variables with same legal value using mrv
    (cond ((> (length index-list) 1)
	   (setf inv1 (num-involved-constraints (elt index-list 0) constraints))
	   (setf inv2 (num-involved-constraints (elt index-list 1) constraints))
	   (if (< inv1 inv2)
	       (setf var-index (elt index-list 1)))))   
    var-index))



;;;
;;; Two global variables to which we may set our variable-picking-function
;;; and value-ordering-functions.
;;;
;;; WARNING: If you re-enter or recompile a function which is being
;;; pointed to by these variables, then they will continue to point to
;;; the OLD VERSION OF THE FUNCTION (which doesn't get garbage-collected).
;;; Use setf to set them to the new function.
;;;

(defparameter *variable-picking-function*
  ;; the default here just picks the an available at random
  #'pick-random-variable

  "Set this to a function which takes a list of
lists of values, one per variable, plus set of constraints.  For example, if the
list looks like ((foo bar) (baz quux) (quuux)) then you have
THREE variables -- the first variable could be set to either
FOO or BAR, the second variable could be set to either BAZ
or QUUX, but the third variable has ALREADY been bound to QUUUX;
thus it appears as the *only choice* for the third variable.
Given this list, your function should return an index indicating
the variable you have chosen (in the example above, the index
would either be 0 or 1 -- never pick a variable that's already
been bound).  You're guaranteed to have at least one non-bound
variable in the list."
  )


(defparameter *value-ordering-function*
  ;; the default here just keeps the list order as it is
  #'(lambda (value-list variable-index constraints)
      (declare (ignorable variable-index constraints))
      value-list)
  ;; declare = to declare Lisp variables as "special" and behaves computationally as if it is not present (other than to affect the sematics), and is only allowed in certain contexts, such as after the variable list in a let, do, defun
  ;; An ignorable declaration specifies that for-value reference to the indicated binding might or might not occur within the scope of the declaration. Within the scope of such a declaration, it is not desirable for a compiler to issue a warning about the presence or absence of either a for value reference
  "Set this to a function which orders the values of a variable,
given a value list as defined in *variable-picking-function*,
plus the index number of the variable and some constraints.  The ordering should
be done such that the first item in the ordering is the first
thing we should try, and the last item is the last thing we
should try.  Example:
 (funcall *value-ordering-function* '((foo bar) (baz quux) (quux)) 1)
...might choose to prefer quux over baz, and thus return the list
 ((FOO BAR) (QUUX BAZ) (QUUX))
...This function can be destructive to the list."
  )



(defun complete-p (value-list)
  "Returns t if all values in value-list have been bound"
    ;; IMPLEMENT THIS
  (let ((check t))
    (dolist (item value-list)
      (if (not (= (length item) 1))
	  (setf check nil)))
    check))

(defun consistent-p (value-list constraints)
  "Returns t if, given the list of functions in constraints,
every value-list doesn't violate any of them"
  (let ((check t)) 
    (dolist (item constraints)
      (setf check (and check (funcall (constraint-predicate item) value-list))))
    check))

(defun bind-variable (value-list variable-index value-index)
  "Returns a new value list with the variable bound to the
given value -- that is, its list is reduced to just holding the value.
This should be a COPY of value-list: use copy-tree.  In other words,
it should NOT be destructive to the list."
  (let ((new-value-list nil) variable)
    (setf new-value-list (copy-tree value-list))
    (setf variable (elt (elt value-list variable-index) value-index))
    ;; swap the value at value-iyndex to the first element
    (rotatef (nth value-index (elt new-value-list variable-index)) (nth 0 (elt new-value-list variable-index)))
    (dolist (item (elt new-value-list variable-index))
	   (if (not (equalp item variable))
	       (delete item (elt new-value-list variable-index))))
    new-value-list))


(let (backtrack-node-count)
  (defun backtracking-search (variable-list value-list constraints)
    "Given a list of variables, a value-list for those variables, and
a list of constraint functions, prints out a solution and returns T or
NIL depending on whether or not a solutino was found.  Also indicates
how long it took and how many nodes were searched in the search tree."

    ;; as it turns out, variable-list is ONLY used to pretty-print
    ;; at the end here...
    (setf backtrack-node-count 0)
    (let (result)
      (time (setf result (recursive-backtracking value-list constraints)))
      (format t "~%~A nodes in search tree searched" backtrack-node-count)
      (if result
      (mapcar #'(lambda (var value) (format t "~%~a: ~a" var (first value)))
          variable-list result)
      (format t "No solution discovered"))
      (not (null result))))


  ;; you gotta write this one (note that it's inside the closure of
  ;; BACKTRACK-NODE-COUNT)

  (defun recursive-backtracking (value-list constraints)
    "Returns a list of values, one per variable, which satisfies the constraints.
value-list is a list of lists, one per variable.  Each sublist contains
all the values that the variable may take on.  If the sublist contains just a
single value, then the variable has been BOUND to that value already.  Each time
SATISFY-CONSTRAINTS is called, the first thing it should do is increment the
closure variable BACKTRACK-NODE-COUNT.  If nothing satisfies the constraints,
returns NIL."
    (when (= (mod (incf backtrack-node-count) 10000) 0)
      (format t "~%searching... ~a" backtrack-node-count))

    (let ((new-value-list value-list) var-index (val-list nil) result)
      ;; check the value-list is complete
      (when (complete-p new-value-list) (return-from recursive-backtracking new-value-list))
      ;; pick a variable to work on
      (setf var-index (mrv-d new-value-list constraints))
      
      (setf backtrack-node-count (+ backtrack-node-count 1))
      ;; get the list of the variable's values
      (setf val-list (elt value-list var-index))
      ;; test for all values of the variable
      (dotimes (i (length val-list))
	;; get a binding of the variable
	(setf new-value-list (bind-variable value-list var-index i))
	
	;; if the binding variable is consistent
	(cond ((consistent-p new-value-list constraints)
	       (when (complete-p new-value-list) (return-from recursive-backtracking new-value-list))
	       (setf result (recursive-backtracking new-value-list constraints))
	       (when (not (null result))
		 (return-from recursive-backtracking result)))))
      (return-from recursive-backtracking nil)) ))    







;;; Cryptarithmetic problem as shown in text:
;;;
;;;[3 2 1  ]  <- carry variables: X3, X2, X1
;;;   T W O
;;; + T W O
;;; -------
;;; F O U R
;;;
;;; Because T is a constant (true), we can't use it as a variable name.
;;; So we use S instead.

;;; RUN AS:
;;; (backtracking-search *cryptarithmetic-variables* *cryptarithmetic-values* *cryptarithmetic-constraints*)

(defparameter *cryptarithmetic-variables*
  '(F S U W R O X3 X2 X1))

(defparameter *cryptarithmetic-values*
  '(;; now the value lists, one per variable name
    (1 2 3 4 5 6 7 8 9)  ;; F: 1 through 9 only
    (1 2 3 4 5 6 7 8 9)  ;; S: 1 through 9 only
    (0 1 2 3 4 5 6 7 8 9) ;; U
    (0 1 2 3 4 5 6 7 8 9) ;; W
    (0 1 2 3 4 5 6 7 8 9) ;; R
    (0 1 2 3 4 5 6 7 8 9) ;; O
    (0 1 2 3 4 5 6 7 8 9) ;; X3
    (0 1 2 3 4 5 6 7 8 9) ;; X2
    (0 1 2 3 4 5 6 7 8 9))) ;; X1

(defparameter *cryptarithmetic-constraints*
  (list

   ;; ----  F S U W R and O must be all different  -----
   (new-constraint (F S U W R O X3 X2 X1)
           (F S U W R O)           ;; variables we need to make sure are bound
           ;; since there are only 15 comparisons, we might
           ;; as well do all of them here rather than any fancier
           ;; stuff...
           (and (/= F S) (/= F U) (/= F W) (/= F R) (/= F O)
            (/= S U) (/= S W) (/= S R) (/= S O)
            (/= U W) (/= U R) (/= U O)
            (/= W R) (/= W O)
            (/= R O)))
           ;;; (all-different-p F S U W R O))  ;; actually this is SLOWER.  But I'm showing it here for you to see.

   ;; ----- F must be the same as X3 -----
   (new-constraint (F S U W R O X3 X2 X1)
           (F X3)                  ;; variables we need to make sure are bound
           (= F X3))

   ;; ----- X2 + S + S must have its first digit as X3 and its second digit as O
   (new-constraint (F S U W R O X3 X2 X1)
           (S O X3 X2)             ;; variables we need to make sure are bound
           (and (= (mod (+ S S X2) 10) O)
            (= (truncate (+ S S X2) 10) X3)))

   ;; ----- X1 + W + W must have its first digit as X2 and its second digit as U
   (new-constraint (F S U W R O X3 X2 X1)
           (W U X1 X2)             ;; variables we need to make sure are bound
           (and (= (mod (+ W W X1) 10) U)
            (= (truncate (+ W W X1) 10) X2)))

   ;; ----- O + O must have its first digit as X1 and its second digit as R
   (new-constraint (F S U W R O X3 X2 X1)
           (R O X1)                ;; variables we need to make sure are bound
           (and (= (mod (+ O O) 10) R)
            (= (truncate (+ O O) 10) X1)))))





;;; Australia problem
;;; Map-coloring for regions of australia, a brutally simple problem
;;; as described in the text.
;;;
;;; Because T is a constant, once again we can't use T for tasmania, and instead use TA
;;;

;;; RUN AS:
;;; (backtracking-search *australia-variables* *australia-values* *australia-constraints*)

(defparameter *australia-variables*
  ;; we use TA rather than T for Tasmania because T is not permitted to be a variable
  '(wa nt sa q nsw v ta))

(defparameter *australia-values*
  '(;; now the value lists, one per variable name
    (red green blue)  ;; wa
    (red green blue)  ;; nt
    (red green blue)  ;; sa
    (red green blue)  ;; q
    (red green blue)  ;; nsw
    (red green blue)  ;; v
    (red green blue))) ;; ta

(defparameter *australia-constraints*
  (list

   ;; ---- WA and NT not the same
   (new-constraint (wa nt sa q nsw v ta)
           (wa nt)               ;; variables we need to make sure are bound
           (not (equal wa nt)))

   ;; ---- WA and SA not the same
   (new-constraint (wa nt sa q nsw v ta)
           (wa sa)               ;; variables we need to make sure are bound
           (not (equal wa sa)))

   ;; ---- NT and SA not the same
   (new-constraint (wa nt sa q nsw v ta)
           (nt sa)               ;; variables we need to make sure are bound
           (not (equal nt sa)))

   ;; ---- NT and Q not the same
   (new-constraint (wa nt sa q nsw v ta)
           (nt q)               ;; variables we need to make sure are bound
           (not (equal nt q)))

   ;; ---- WA and NT not the same
   (new-constraint (wa nt sa q nsw v ta)
           (wa nt)               ;; variables we need to make sure are bound
           (not (equal wa nt)))

   ;; ---- Q and SA not the same
   (new-constraint (wa nt sa q nsw v ta)
           (q sa)               ;; variables we need to make sure are bound
           (not (equal q sa)))

   ;; ---- Q and NSW not the same
   (new-constraint (wa nt sa q nsw v ta)
           (q nsw)               ;; variables we need to make sure are bound
           (not (equal q nsw)))

   ;; ---- NSW and SA not the same
   (new-constraint (wa nt sa q nsw v ta)
           (nsw sa)               ;; variables we need to make sure are bound
           (not (equal nsw sa)))

   ;; ---- NSW and V not the same
   (new-constraint (wa nt sa q nsw v ta)
           (nsw v)               ;; variables we need to make sure are bound
           (not (equal nsw v)))

   ;; ---- V and SA not the same
   (new-constraint (wa nt sa q nsw v ta)
           (v sa)               ;; variables we need to make sure are bound
           (not (equal v sa)))))





;;; 8-queens problem
;;; As described in the text.
;;; The setup for this problem is different from the others
;;; because I'm lazy and don't want to have to type 23 constraints
;;; definitions and 64 different value settings.  So I wrote
;;; some programs to spit out the values and the constraints for
;;; me.  Basically we assume that queen 0 (q0) exists only in
;;; column 0 of the chessboard, queen 1 (q1) is in column 2,
;;; etc.  The values are the 8 coordinate positions each one
;;; can take on.  The constraints are 23 tests to make sure that
;;; q0 can't capture q1, etc.   Okay, so it was probably easier
;;; to write out all 23 tests than to write the bizarre
;;; uncommented monstrosity below, but it was fun to figure out
;;; how to do.  :-)

;;; RUN AS:
;;; (backtracking-search *8-queens-variables* *8-queens-values* *8-queens-constraints*)


(defun cannot-capture-p (queen1 queen2)
  "Queen 1 cannot capture Queen 2 --
queens defined as lists of the form (x y),
we'll presume that the y's are different"
  (and (/= (second queen1) (second queen2))              ;; not on same row
       (/= (abs (- (first queen1) (first queen2)))
       (abs (- (second queen1) (second queen2))))))  ;; no diagonal

(defparameter *8-queens-variables*
  '(q0 q1 q2 q3 q4 q5 q6 q7))

(defparameter *8-queens-values*
  (let (board)
    (dotimes (x 8)
      (let (col)
    (dotimes (y 8)
      (push (list x y) col))
    (push (reverse col) board)))
    (reverse board)))

(defmacro generate-8-queens-constraints ()
  (cons 'list
    (apply #'append
           (maplist #'(lambda (remaining-queens)
                (let ((queen1 (first remaining-queens)))
                  (mapcar #'(lambda (queen2)
                      `(new-constraint ,*8-queens-variables*
                               (,queen1 ,queen2)
                               (cannot-capture-p ,queen1 ,queen2)))
                      (rest remaining-queens))))
            *8-queens-variables*))))


(defparameter *8-queens-constraints*
  (generate-8-queens-constraints))




;;; The Zebra Puzzle
;;;
;;; As shown on page 160 in text
;;; Additional constraints the text conveniently
;;; and quietly doesn't mention: things can't be
;;; in the same houses of course.
;;;
;;; There are a ton of variables here so I made my own
;;; new-constraint macro so I didn't have to type so much
;;;
;;; USE AS: (backtracking-search *zebra-variables* *zebra-values* *zebra-constraints*)

(defparameter *zebra-variables*
  '(englishman spaniard norwegian ukranian japanese
    red yellow blue ivory green
    dog fox snails horse zebra
    kools chesterfields winston lucky-strike parliaments
    orange-juice tea coffee milk water))

(defparameter *zebra-values*
  ;; all 25 variables take on the same values: (0 1 2 3 4)
  (make-list 25 :initial-element '(0 1 2 3 4)))

;;; so we're not writing (new-constraint (englishman ....... water) ... )
;;; over and over and over and over again, I make a new new-constraint
;;; macro:

(defmacro new-zebra-constraint ((&rest relevant-variables) &rest body)
  `(new-constraint ,*zebra-variables* ,relevant-variables ,@body))

(defparameter *zebra-constraints*
  (list
   ;; constraints guaranteeing differences
   (new-zebra-constraint (englishman spaniard norwegian ukranian japanese)
             (all-different-p englishman spaniard norwegian ukranian japanese))
   (new-zebra-constraint (red yellow blue ivory green)
             (all-different-p red yellow blue ivory green))
   (new-zebra-constraint (dog fox snails horse zebra)
             (all-different-p dog fox snails horse zebra))
   (new-zebra-constraint (kools chesterfields winston lucky-strike parliaments)
             (all-different-p kools chesterfields winston lucky-strike parliaments))
   (new-zebra-constraint (orange-juice tea coffee milk water)
             (all-different-p orange-juice tea coffee milk water))

   ;; constraints on page 160

   (new-zebra-constraint (englishman red)
             (= englishman red))

   (new-zebra-constraint (spaniard dog)
             (= spaniard dog))

   (new-zebra-constraint (norwegian)
             (= norwegian 0))

   (new-zebra-constraint (kools yellow)
             (= kools yellow))

   (new-zebra-constraint (chesterfields fox)
             (= (abs (- chesterfields fox)) 1))

   (new-zebra-constraint (norwegian blue)
             (= (abs (- norwegian blue)) 1))

   (new-zebra-constraint (winston snails)
             (= winston snails))

   (new-zebra-constraint (lucky-strike orange-juice)
             (= lucky-strike orange-juice))

   (new-zebra-constraint (ukranian tea)
             (= ukranian tea))

   (new-zebra-constraint (japanese parliaments)
             (= japanese parliaments))

   (new-zebra-constraint (kools horse)
             (= (abs (- kools horse)) 1))

   (new-zebra-constraint (coffee green)
             (= coffee green))

   (new-zebra-constraint (green ivory)
             (= (- green ivory) 1))

   (new-zebra-constraint (milk)
             (= milk 2))))


"
                           Report Extra Credit 2 
                         Project: Constraint Satisfaction Project 
                          Hoang Dung Bui
                          G001301478

In this project, a Constraint Satisfaction algorithm  was developed. There are four problems to run by the algorithm: Australia map, Cryptarithmetic, 8-queens, and Zebra. The main tasks of the programs are: calculate the variable to pick up, check the variables is complete, and consistent, build a new state by binding into a variable, backtracking-search and recursive-backtracking. The detail of each function were described below.

1. Program Explanation
- mrv-d function: This function determines which variable is better to perform first. The first criteria is the number of availabe values left for each variable. The less the values is, the higher the variable's priority is. This criteria is performed by function legal-values. The second criteria bases on the number of constraints that the variable is involved. There more involving of the variable, the higher priority it is. This priority is implemented by the function: num-involved-constraints.

- complete-p: this function check whether all variables are bounded. If yest, it will return true.

- consistent-p: this function is simply check whether the inputted value-list does violate any constraint. If it pass all the contraints, the function will return true. 

- bind-variable: it returns a new state by binding the unbound variable in a specific value. Its inputs are the value-list, the variable should be bounded and the index of the specific value will be bounded with the variable.
 
- recursive-backtracking: In this function, first it will check whether the input state is complete or not. If it is complete, the inputted state will be return, and it is te root of the recursive call. If the state is not complete, then program calls mrv-d function to pick-up the best variable, get the domain of the variable, and run through the domain by a dotimes loop. In the dotimes loop, there is a recursive call to the function itself, until it reach a complete and consistent state.


2. Experiment and Model's parameters 
As running the command: (backtracking-search *variables* *values* *constraints*) (substitute *variables* *values* *constraints* for the corresponding the given problems, the result as following:

2.1 For Australia map problems
Evaluation took:
  0.000 seconds of real time
  0.000417 seconds of total run time (0.000393 user, 0.000024 system)
  100.00% CPU
  1,197,644 processor cycles
  196,592 bytes consed
  

14 nodes in search tree searched
WA: RED
NT: GREEN
SA: BLUE
Q: RED
NSW: GREEN
V: RED
TA: RED

2.2 For Cryptarithmetic Problem
Evaluation took:
  1.182 seconds of real time
  1.186305 seconds of total run time (1.170120 user, 0.016185 system)
  [ Run times consist of 0.082 seconds GC time, and 1.105 seconds non-GC time. ]
  100.34% CPU
  3,419,627,071 processor cycles
  1,319,284,464 bytes consed
  
24504 nodes in search tree searched
F: 1
S: 7
U: 6
W: 3
R: 8
O: 4
X3: 1
X2: 0
X1: 0


2.3 For 8-queens Problem
Evaluation took:
  0.026 seconds of real time
  0.026157 seconds of total run time (0.026157 user, 0.000000 system)
  [ Run times consist of 0.005 seconds GC time, and 0.022 seconds non-GC time. ]
  100.00% CPU
  75,643,388 processor cycles
  18,611,728 bytes consed
  
226 nodes in search tree searched
Q0: (0 3)
Q1: (1 1)
Q2: (2 6)
Q3: (3 2)
Q4: (4 5)
Q5: (5 7)
Q6: (6 4)
Q7: (7 0)


2.4 For Zebra problem
Evaluation took:
  2.629 seconds of real time
  2.641928 seconds of total run time (2.616022 user, 0.025906 system)
  [ Run times consist of 0.238 seconds GC time, and 2.404 seconds non-GC time. ]
  100.49% CPU
  7,606,753,299 processor cycles
  4,110,650,704 bytes consed
  
14958 nodes in search tree searched
ENGLISHMAN: 2
SPANIARD: 3
NORWEGIAN: 0
UKRANIAN: 1
JAPANESE: 4
RED: 2
YELLOW: 0
BLUE: 1
IVORY: 3
GREEN: 4
DOG: 3
FOX: 0
SNAILS: 2
HORSE: 1
ZEBRA: 4
KOOLS: 0
CHESTERFIELDS: 1
WINSTON: 2
LUCKY-STRIKE: 3
PARLIAMENTS: 4
ORANGE-JUICE: 3
TEA: 1
COFFEE: 4
MILK: 2
WATER: 0


"
