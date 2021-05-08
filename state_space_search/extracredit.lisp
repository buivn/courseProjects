
;;; STATE SPACE SEARCH - Extra Credit
;;;
;;; Due MIDNIGHT, the evening of FRIDAY, APRIL 16


(load "utilities.lisp")
(load "queue.lisp")


(defun make-initial-state (initial-puzzle-situation)
    "Makes an initial state with a given puzzle situation.
    The puzzle situation is simply a list of 9 numbers.  So to
    create an initial state with the puzzle
    2 7 4
    9 8 3
    1 5 6
    ...you would call (make-initial-state '(2 7 4 9 8 3 1 5 6))"
    (cons (concatenate 'simple-vector initial-puzzle-situation 
            (list (position 16 initial-puzzle-situation))) nil))

(defun create-random-state (num-moves)
    "Generates a random state by starting with the
    canonical correct puzzle and making NUM-MOVES random moves.
    Since these are random moves, it could well undo previous
    moves, so the 'randomness' of the puzzle is <= num-moves"
    (let ((puzzle #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 16 15 14)))
        (dotimes (x num-moves)
            (let ((moves (elt *valid-moves* (empty-slot puzzle))))
                (setf puzzle (make-move (elt moves (random (length moves))) puzzle))))
        (build-state puzzle nil)))

(defmacro depth (state)
    "Returns the number of moves from the initial state 
    required to get to this STATE"
    `(1- (length ,state)))

(defmacro puzzle-from-state (state)
    "Returns the puzzle (an array of 10 integers) from STATE"
    `(car ,state))

(defmacro previous-state (state)
    "Returns the previous state that got us to this STATE"
    `(cdr ,state))

(defmacro empty-slot (puzzle)
    "Returns the position of the empty slot in PUZZLE"
    `(elt ,puzzle 16))

(defun swap (pos1 pos2 puzzle)
    "Returns a new puzzle with POS1 and POS2 swapped in original PUZZLE.  If
    POS1 or POS2 is empty, slot 9 is updated appropriately."
    (let ((tpos (elt puzzle pos1)) (puz (copy-seq puzzle)))
        (setf (elt puz pos1) (elt puz pos2))  ;; move pos2 into pos1's spot
        (setf (elt puz pos2) tpos)  ;; move pos1 into pos2's spot
        (if (= (elt puz pos1) 16) (setf (empty-slot puz) pos1)  ;; update if pos1 is 9
            (if (= (elt puz pos2) 16) (setf (empty-slot puz) pos2)))  ;; update if pos2 is 9
        puz))

(defparameter *valid-moves* 
    #((1 4) (0 2 5) (1 3 6) (2 7) (0 5 8) (1 4 6 9) (2 5 7 10) (3 6 11) (4 9 12) (5 8 10 13) (6 9 11 14) (7 10 15) (8 13) (9 12 14) (10 13 15) (11 14))
    "A vector, for each empty slot position, of all the valid moves that can be made.
    The moves are arranged in lists.")

(defmacro foreach-valid-move ((move puzzle) &rest body)
    "Iterates over each valid move in PUZZLE, setting
    MOVE to that move, then executing BODY.  Implicitly
    declares MOVE in a let, so you don't have to."
    `(dolist (,move (elt *valid-moves* (empty-slot ,puzzle)))
        ,@body))

(defun make-move (move puzzle)
    "Returns a new puzzle from original PUZZLE with a given MOVE made on it.
    If the move is illegal, nil is returned.  Note that this is a PUZZLE,
    NOT A STATE.  You'll need to build a state from it if you want to."
    (let ((moves (elt *valid-moves* (empty-slot puzzle))))
        (when (find move moves) (swap move (empty-slot puzzle) puzzle))))

(defmacro build-state (puzzle previous-state)
    "Builds a state from a new puzzle situation and a previous state"
    `(cons ,puzzle ,previous-state))

(defmacro foreach-position ((pos puzzle) &rest body)
    "Iterates over each position in PUZZLE, setting POS to the
    tile number at that position, then executing BODY. Implicitly
    declares POS in a let, so you don't have to."
    (let ((x (gensym)))
        `(let (,pos) (dotimes (,x 9) (setf ,pos (elt ,puzzle ,x))
            ,@body))))

(defun print-puzzle (puzzle)
    "Prints a puzzle in a pleasing fashion.  Returns the puzzle."
    (let (lis)
        (foreach-position (pos puzzle)
            (if (= pos 16) (push #\space lis) (push pos lis)))
        (apply #'format t "~%~A~A~A~%~A~A~A~%~A~A~A" (reverse lis)))
    puzzle)

(defun print-solution (goal-state)
    "Starting with the initial state and ending up with GOAL-STATE,
    prints a series of puzzle positions showing how to get 
    from one state to the other.  If goal-state is 'FAILED then
    simply prints out a failure message"
    ;; first let's define a recursive printer function
    (labels ((print-solution-h (state)
                (print-puzzle (puzzle-from-state state)) (terpri)
                (when (previous-state state) (print-solution-h (previous-state state)))))
        ;; now let's reverse our state list and call it on that
        (if (equalp goal-state 'failed) 
            (format t "~%Failed to find a solution")
            (progn
                (format t "~%Solution requires ~A moves:" (1- (length goal-state)))
                (print-solution-h (reverse goal-state))))))



(defun general-search (initial-state goal-test enqueueing-function &optional (maximum-iterations nil))
    "Starting at INITIAL-STATE, searches for a state which passes the GOAL-TEST
    function.  Uses a priority queue and a history list of previously-visited states.
    Enqueueing in the queue is done by the provided ENQUEUEING-FUNCTION.  Prints 
    out the number of iterations required to discover the goal state.  Returns the 
    discovered goal state, else returns the symbol 'FAILED if the entire search 
    space was searched and no goal state was found, or if MAXIMUM-ITERATIONS is 
    exceeded.  If maximum-iterations is set to nil, then there is no maximum number
    of iterations."
  
  (let ((state-queue nil) (history nil) (state nil) (iteration 0) (children nil) (check-iteration t) (in-history nil))
    ;; make a new empty state queue
    (setf state-queue (make-empty-queue))
    ;; add the Initial (queue and history) 
    (funcall enqueueing-function initial-state state-queue)
    (push (puzzle-from-state initial-state) history)
    ;; add maximum iteration to check
    (if (null maximum-iterations)
	(setf check-iteration nil))
    (loop   ;; the main loop
      (setf iteration (+ iteration 1))
      (if check-iteration
	  (when (= iteration maximum-iterations) (return 'FAILED)))
      ;; get a state from the queue
      (setf state (queue-front state-queue))
      ;; remove that state from the state queue
      (remove-front state-queue)
      ;; check whether the state is goal
      (cond ((funcall goal-test state)
	     (format t "~%The number of iteration: ~d ~%" iteration)
	     ;; print the solution
	     (print-solution state)
	     (return state)))
      ;; find the children of that state in the puzzle form
      (setf children (find-children state))
      (dolist (child children) ;; loop the whole children
	(setf in-history nil)
	;; check whether the  child state is in the history?
	(dolist (his history)
	  (cond ((equalp his (car child))
	      (setf in-history t))))
	(cond ((not in-history) ;; it not, add it to the state-queue and history
	       (funcall enqueueing-function child state-queue)
	       (push (puzzle-from-state child) history)
   
	))))
    )
;; hints: The history list ought to contain PUZZLES, not states.
;; However, the queue ought to contain STATES, which in this case consist of
;; conses consisting of the PUZZLE as the car, and the puzzle's parent's cons as the cdr.
;; You should use #'equalp to test for equality between puzzles in the history list.
;; You should also add the puzzle to the history list at exactly the same time
;; you add its corresponding state to the queue.

)

(defun goal-p (state)

  "Returns T if state is a goal state, else NIL.  Our goal test."
  (let (goal1)
    (setf goal1 (make-initial-state '(
				 1 2 3 4
				 5 6 7 8
				 9 10 11 12
				 13 14 15 16)))

    (equalp (car state) (car goal1))
  ))
                                   

(defun manhattan-enqueuer (state queue)
  "Enqueues by manhattan distance"
  ;; insert the state based on its f-function
  (enqueue-by-priority queue #'f-manhattan state)
)
	  
(defun f-manhattan (state)
  (let ((g 0) (h 0) (f 0))
    (setf g (depth state))
    (setf h (manhattan-distance-state state))
    ;; calculate the f-function (heuristic function)
    (setf f (+ g h))
    f))

(defun manhattan-distance-state (state)
  (let ((h 0))
    (dotimes (i (- (length (car state)) 1))
      (if (not (= (elt (car state) i) 16))
	  (setf h (+ h (manhattan-distance-one-tile i (- (elt (car state) i) 1))))))
    h))

(defun manhattan-distance-one-tile (start target)
  (let ((ver 0) (hor 0))
    (cond ((< start 4)   ;; if the start lie in row 1
	   (cond ((< target 4) ;;if the target lie in row 1
		  (setf ver 0)
		  (setf hor (abs (- (mod start 4) (mod target 4)))))
		 ((and (>= target 4) (< target 8))  ;;if the target lie in row 2
		  (setf ver 1)
		  (setf hor (abs (- (mod start 4) (mod target 4)))))
		 ((and (>= target 8) (< target 12))   ;;if the target lie in row 3
		  (setf ver 2)
		  (setf hor (abs (- (mod start 4) (mod target 4)))))
		 ((>= target 12)   ;;if the target lie in row 4
		  (setf ver 3)
		  (setf hor (abs (- (mod start 4) (mod target 4)))))))

	  ((and (>= start 4) (< start 8))  ;; if the start lie in row 2
	   (cond ((< target 4) ;;if the target lie in row 1
		  (setf ver 1)
		  (setf hor (abs (- (mod start 4) (mod target 4)))))
		 ((and (>= target 4) (< target 8))  ;;if the target lie in row 2
		  (setf ver 0)
		  (setf hor (abs (- (mod start 4) (mod target 4)))))
		 ((and (>= target 8) (< target 12))   ;;if the target lie in row 3
		  (setf ver 1)
		  (setf hor (abs (- (mod start 4) (mod target 4)))))
		 ((>= target 12)   ;;if the target lie in row 4
		  (setf ver 2)
		  (setf hor (abs (- (mod start 4) (mod target 4)))))))
 
	  ((and (>= start 8) (< start 12))  ;; if the start lie in row 3
	   (cond ((< target 4) ;;if the target lie in row 1
		  (setf ver 2)
		  (setf hor (abs (- (mod start 4) (mod target 4)))))
		 ((and (>= target 4) (< target 8))  ;;if the target lie in row 2
		  (setf ver 1)
		  (setf hor (abs (- (mod start 4) (mod target 4)))))
		 ((and (>= target 8) (< target 12))   ;;if the target lie in row 3
		  (setf ver 0)
		  (setf hor (abs (- (mod start 4) (mod target 4)))))
		 ((>= target 12)   ;;if the target lie in row 4
		  (setf ver 1)
		  (setf hor (abs (- (mod start 4) (mod target 4))))))) 

	  ((>= start 12)  ;; if the start lies in row 4
	   (cond ((< target 4) ;;if the target lie in row 1
		  (setf ver 3)
		  (setf hor (abs (- (mod start 4) (mod target 4)))))
		 ((and (>= target 4) (< target 8))  ;;if the target lie in row 2
		  (setf ver 2)
		  (setf hor (abs (- (mod start 4) (mod target 4)))))
		 ((and (>= target 8) (< target 12))   ;;if the target lie in row 3
		  (setf ver 1)
		  (setf hor (abs (- (mod start 4) (mod target 4)))))
		 ((>= target 12)   ;;if the target lie in row 4
		  (setf ver 0)
		  (setf hor (abs (- (mod start 4) (mod target 4))))))))
    (+ ver hor)))



(defun find-children (state)
  ;; This function find children of a states
  (let ((pos 0) children-state last-element children-number (children-list nil) (children nil) moves)
    ;; find the location of empty tile
    (setf last-element (- (length (car state)) 1))
    (setf pos (elt (car state) last-element))
    ;; save a list of valid move into the list moves
    (setf moves (elt *valid-moves* pos)) 
    (setf children-number (length (elt *valid-moves* pos)))
    (dotimes (i children-number)
      (setf children (swap (elt moves i) pos (car state)))
      (setf children-state (build-state children state))
      (push children-state children-list))
    ;; return a list of children states
    children-list))



(setf goal (make-initial-state '(
				 1 2 3 4
				 5 6 7 8
				 9 10 11 12
				 13 14 15 16)))

;;; solve in 16 moves 
(setf s41 (make-initial-state '(
			      1  2  3  4
			      5  6  7  8
			      9  11 16 15
				13 14 10 12)))

;;; solve in  moves 
(setf s42 (make-initial-state '(
			      2  1  3  4
			      5  11 7  8
			      9  6  16 15
			      13 14 10 12)))

;;; solve in 16 moves 
(setf s43 (make-initial-state '(
			      16 4  3  2
			      5  6  7  8
			      9  11 1 15
			      13 14 10 12)))

;;; The five test ex

;;; Solves in 4 moves:
(setf s31 (make-initial-state '(
			      9 2 3
			      1 4 6
			      7 5 8)))


;;; Solves in 8 moves:
(setf s32 (make-initial-state '(
2 4 3
1 5 6
9 7 8)))

;;; Solves in 16 moves:
(setf s33 (make-initial-state '(
2 3 9
5 4 8
1 6 7)))

;;; Solves in 24 moves:
(setf s34 (make-initial-state '(
1 8 9
3 2 4
6 5 7)))

;;; easy or hard to solve?  Why?
(setf s35 (make-initial-state '(
9 2 3
4 5 6
7 8 1)))



"

                                     Report's Project 4 - Extra Credit
                                 Hoang Dung Bui, G01301478

In this extra credit part, I extended the problem into 15 puzzle, with modification on on the examples (s41, s42, s43), function make-initial-state, create-random-state, empty-slot, swap, defparameter, print-puzzle, goal-p, manhattan-distance-state, and manhattan-distance-one-tile. 

In the first experiment, I still tested on A* algorithm with manhattan distance. The program structure is similar to the case for 8 puzzle, with modification for the 15 puzzle. There are three test cases:

1. Experiment on 15 puzzle
The results are described as following:

For example s41: it needs 298 iterations to reach the goal with 16 moves.

For Example s42: it start worse, and require 12 iterations (8 moves).

For Example 3: The result is much worse, with 246 iterations and 62 moves


2. Implement simple best-first search (f = h)
In general, the results are not optimal like the A* as following:

For example 1: it is the same result as A*, it needs 5 iterations to reach the goal (4 moves)

For Example 2: it start worse, and require 12 iterations (8 moves).

For Example 3: The result is much worse, with 246 iterations and 62 moves




There are four options to run the algorithm by dfs-enqueuer, bfs-enqueuer, manhattan-enqueuer, and num-out-enqueuer. The four main tasks of the functions are adding a new state into the queue. The way to add depends on the algorithm. To help the algoirthm works functionally, it was needed several functions such as find-children, f-num-out, manhattan-distance-one-tile, manhattan-distance-state, f-manhattan, and goal-p. The detail of each function were described below.

1. Program Explanation
- find-children: This function will look for the children of a given state. The function start by searching the tile number 9 (empty one), then determine the feasiable move for the position. For each valid move, it swap the tiles, and generate a new puzzle. the puzzles are then convert to state by build-state function, and then are return in the list form.
- f-num-out: This function calculate the f function to select the position to add the new state. It is based on the number of out place tile. A dotimes loop is used to run through the vector, and increase the value of the f-function if there are tiles out of their places.
- manhattan-distance-one-tile: This function calculate the manhattan distace of a tile to move to its correct position. With 8 puzzle, I broke the 8 number into three ranges, and determine the distances by the moves following theirs columns and rows.
- manhattan-distance-state: This function calculate the manhattan distance of the whole state. It is simply the sum of manhattan distances of all the tiles. 
- f-manhattan: This function is used to calculate the f-function with manhattan distance. It works as the same as the function f-num-out. It calculates both g and h values.
- goal-p: This function will check whether we reach the goal. It simply uses the equalp operator.

2. Implement simple best-first search (f = h)
In general, the results are not optimal like the A* as following:

For example 1: it is the same result as A*, it needs 5 iterations to reach the goal (4 moves)

For Example 2: it start worse, and require 12 iterations (8 moves).

For Example 3: The result is much worse, with 246 iterations and 62 moves


For Example 4: However, what a supprise, in this examples, it needs only 268 iterations to find the target, but it needs 62 moves to reach the target. This one can be explainable because best-first search just care about how good the heuristic look like.


For Example 5: It is still fail to find the goal within 60000 iterations

"
