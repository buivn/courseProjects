;;; Constraint Satisfaction Project
;;; 
;;; The object here is to write a simple backtracking constraint satisfaction
;;; search system.  We will not do forward checking, arc consistency, or
;;; backjumping: instead our only heuristics will be MRV and Degree [it's not
;;; a terribly hard project].  I provide you with four ready-made problem
;;; domains from the text: Australia 3-coloring (a *trivial* problem, good
;;; for initial testing), Cryptarithmetic (a quite hard problem), and
;;; 8-queens (a fairly easy problem), and the Zebra problem.
;;; 
;;; I generated the code below by writing the project myself and testing it,
;;; then ripping out certain parts for you to fill in.
;;;
;;; Data Structures:
;;;
;;; Variables are represented as symbols.  The full set of variables for a
;;; problem, called the VARIABLE-LIST, is all the variables in a list.  For
;;; example, the australia problem looks like:
;;;
;;;              '(wa nt sa q nsw v ta))
;;;
;;; Note in this example we're using "TA" rather than "T" because T cannot
;;; be used as a variable (it represents "true"!)
;;;
;;; The #1 data structure passed around in the code is called a VALUE-LIST.
;;; This is a list of lists, one per variable in the VARIABLE-LIST.  Each
;;; sublist contains all the values that that variable may currently take on.
;;; The order of these sublists is important: the first sublist corresponds
;;; to the first variable in your VARIABLE-LIST, and so on.  If a sublist
;;; contains just a single item, then that corresponding variable is considered
;;; to be bound to that value.  If there's more than one item, then the variable
;;; is not bound yet, and the sublist contains future possibilities left for that
;;; variable.  For example, if WA and NT had red and green left to them, and
;;; SA was set to blue, and Q and NSW and V all had red, green, and blue available,
;;; and TA was bound to red, then the current value-list would look like:
;;;
;;; '((red green) (red green) (blue) (red green blue) (red green blue) (red green blue) (red)) 
;;;
;;; At the beginning of the run you're provided a high-level value-list containing
;;; all possible values.  value-lists propagated through the tree get progressively
;;; more constrained -- you'll need to copy their parents to make the kids (use copy-tree,
;;; NOT copy-list!)
;;;
;;; The other big data structure is a list of constraints.  A constraint is a
;;; struct which most importantly contains a PREDICATE which takes a value-list
;;; and returns T if the value-list doesn't violate the constraint.  Constraints
;;; also contain slots holding the relevant variables for that constraint (which
;;; you may find comes in handy) and also all the variables for the problem (in
;;; essence, a copy of the VARIABLE-LIST -- but you'll find that handy too)
;;;
;;; Constraints are created using the NEW-CONSTRAINT macro.  For example if we had
;;; a problem with 3 variables A, B, and C, and our constraint said that variable B
;;; had to be 2 units away from variable C , we could write it as:
;;;
;;; (setf *my-constraint*  (new-constraint
;;;                                  (A B C)       ;; first list ALL the variables
;;;                                  (B C)         ;; then list the relevant variables
;;;                             (= 2 (abs (- B C)))))   ;; our code which returns T or NIL
;;;
;;; The NEW-CONSTRAINT macro will then build a constraints object for you, including
;;; a function which automatically checks that B and C have been bound, sets the variables
;;; to local variables so you can use them in your code like shown in the previous example,
;;; etc. etc.  Very convenient.
;;;
;;; I have provided you with three variable-selection functions.  The first (default)
;;; one picks a variable at random.  The second one you have to write -- the MRV and
;;; degree heuristic function.  The third one picks the first available variable in the
;;; list.  Amazingly, the third one (which is arbitrary) works EXTREMELY WELL.  Why is
;;; this?  Because the variables in the problems just SO HAPPEN to be arranged in the
;;; right order.  It's complete coincidence.  But it's worth checking out.
;;;
;;; I would like some comparison of the functions on your part as well.
;;; You should expect some runs to take a *very* long time with the random-selection
;;; function.  I don't expect you to run them to the bitter end if they're taking forever
;;; on your system.  However the MRV+Degree heuristic should run just fine on your system
;;; (taking no more than a minute or so) if it's compiled and working correctly.
;;;
;;; If you're into this you could try implementing forward checking or AC3.   But
;;; it's not a requirement.
;;;
;;; I strongly suggest you not only compile, but set up compiler optimizations.




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

  ;; IMPLEMENT THIS ONE
  )


(defun legal-values (variable-index value-list constraints)
  "Returns the number of legal values which the variable-index
can hold, given the existing constraints"
  ;; useful for the mrv heuristic

  ;; IMPLEMENT THIS ONE
  )

(defun mrv-d (value-list constraints)
  "Picks a variable from among the NON-BOUND values in
value-list according to the Minimum Remaining Values
Heuristic, breaking ties with the Degree Heuristic"
  ;; best-position might be useful to you here.

  ;; IMPLEMENT THIS ONE

  )



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
      (if (not (== (length item) 1))
	  (setf check nil)))
    check))

(defun consistent-p (value-list constraints)
  "Returns t if, given the list of functions in constraints,
every value-list doesn't violate any of them"
  ;; IMPLEMENT THIS
  )

(defun bind-variable (value-list variable-index value-index)
  "Returns a new value list with the variable bound to the
given value -- that is, its list is reduced to just holding the value.
This should be a COPY of value-list: use copy-tree.  In other words,
it should NOT be destructive to the list."
  )

(let (backtrack-node-count)
  (defun backtracking-search (variable-list value-list constraints)
    "Given a list of variables, a value-list for those variables, and
a list of constraint functions, prints out a solution and returns T or
NIL depending on whether or not a solutino was found.  Also indicates
how long it took and how many nodes were searched in the search tree."

    ;; I provide this one for you...

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

    ;; you need to write the rest.
    ))     ;; notice the second paren here, it closes (let (backtrack-node-count))







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
           (Q NSW)               ;; variables we need to make sure are bound
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
