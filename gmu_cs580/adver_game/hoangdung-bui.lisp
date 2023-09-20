;;;;                    ASSIGNMENT 5: Connect-Four Player


;(load "connectfour")
;(compile-file "connectfour.lisp")
;(load "connectfour")
;(load "hoangdung-bui")
;(compile-file "hoangdung-bui.lisp")
;(load "hoangdung-bui")

;; (defpackage :template
(defpackage :hoangdung-bui
  (:use "COMMON-LISP" "COMMON-LISP-USER")
  (:export "MAKE-COMPUTER-MOVE"))

(in-package :hoangdung-bui)

(defun alpha-beta (game current-depth max-depth
		   is-maxs-turn-p expand terminal-p evaluate
		   alpha beta)
 "Does alpha-beta search. 
is-maxs-turn-p takes one argument (a game) and returns true if it's max's turn in that game.  
expand takes one argument (a game) and gives all the immediate child games for this game.
terminal-p takes one argument (a game) and returns true if the game is over or not.
evaluate takes TWO arguements (a game and the is-maxs-turn-p function) and provides a quality
assessment of the game for 'max', from min-wins (-1000) to max-wins (1000)."
  (let ((alpha1 alpha) (beta1 beta) (children nil) (mm))
    (cond ((or (funcall terminal-p game) (>= current-depth max-depth))
	   ;;(print "check the first cond")
	   (return-from alpha-beta (funcall evaluate game #'is-maxs-turn-p)))
	  ;;; the second condition and statements
	  ;; if max turn
	  ((funcall is-maxs-turn-p game)
	   (setf children (funcall expand game))
	   (dolist (item children)
	     (setf mm (alpha-beta item (1+ current-depth) max-depth is-maxs-turn-p expand terminal-p evaluate alpha1 beta1))
	     (setf alpha1 (max alpha1 mm))
	     (if (>= alpha1 beta1)
		 (return-from alpha-beta beta1)))
	   (return-from alpha-beta alpha1))

	  ;; if min turn
	  ((not (funcall is-maxs-turn-p game))
	   (setf children (funcall expand game))
	   (dolist (item children)
	     (setf mm (alpha-beta item (1+ current-depth) max-depth is-maxs-turn-p expand terminal-p evaluate alpha1 beta1))
	     (setf beta1 (min beta1 mm))
	     (if (>= alpha1 beta1)
		 (return-from alpha-beta alpha1)))
	   (return-from alpha-beta beta1))
	  )))


(defun evaluate (game is-maxs-turn-p)
  "Returns an evaluation, between min-wins and max-wins inclusive, for the game.
is-maxs-turn-p is a function which, when called and passed the game, returns true
if it's max's turn to play."
  (let ((end (game-over game)))  ;; (game-over game) return 1 0 -1 or nil
    (if (null end) ;; game not over yet

	(progn
	  ;;; IMPLEMENT ME
	  ;; in this block, do your code which returns a heuristic
	  ;; evaluation of the system.  Feel free to create an outside function
	  ;; and call it if you don't want all the code here.
	  ;; by default we're returning 0 (draw).  That's obviously wrong.
	  (heuristic-evaluation game #'is-maxs-turn-p)
	  )


	(if (= 0 end)  ;; game is a draw
	    0

	    ;; else, the game is over but not a draw.  Return its value.
	    ;;
	    ;; this is a deep-math way of saying the following logic:
	    ;;
	    ;; if black, then if turn=black and ismaxsturn, then max-wins
	    ;; if black, then if turn=red and ismaxsturn, then min-wins
	    ;; if black, then if turn=black and !ismaxsturn, then min-wins
	    ;; if black, then if turn=red and !ismaxsturn, then max-wins
	    ;; if red, then if turn=black and ismaxsturn, then min-wins
	    ;; if red, then if turn=red and ismaxsturn, then max-wins
	    ;; if red, then if turn=black and !ismaxsturn, then max-wins
	    ;; if red, then if turn=red and !ismaxsturn, then min-wins
	    ;;
	    ;; keep in mind that black=1, red=-1, max-wins=1000, red-wins = -1000

	    (* end (turn game) max-wins (if (funcall is-maxs-turn-p game) 1 -1)))
	)
    )
  )

(defun heuristic-evaluation (game is-maxs-turn-p)
  ;; based on the number of open three consecutive checkers of black and red
  ;;; the return is the substract between the number of count-red and count-red
  (let ((count-red 0) (count-black 0)
	(width (width game))
	(height (height game))
	(board (board game))
	(count)
	(num-in-a-row)
	(max-row (- (num-in-a-row game) 1))
	(in-row)
	(row))
    ;; consider two row length: max-row - 2 and max-row - 1 
    (dotimes (j 2)
      (setf num-in-a-row (- max-row j))
      
      ;; black vertical
      (dotimes (column width)
	(setf count 0)
	(dotimes (row height)
	  (if (= empty (aref board column row)) (return)  ;; quick bail
	      (if (= black (aref board column row))
		  (if (>= (incf count) num-in-a-row)
		      (if (< (+ row 1) height)
			  ;; check empty square
			  (if (= empty (aref board column (+ row 1)))
			      ;;(incf count-black)
			      (setf count-black (+ count-black (* 6 num-in-a-row))))))
		  (setf count 0)))))

      ;; red vertical
      (dotimes (column width)
	(setf count 0)
	(dotimes (row height)
	  (if (= empty (aref board column row)) (return)  ;; quick bail
	      (if (= red (aref board column row))
		  (if (>= (incf count) num-in-a-row)
		      (if (< (+ row 1) height)
			  (if (= empty (aref board column (+ row 1)))
			      ;;(incf count-red))))
			      (setf count-red (+ count-red (* 6 num-in-a-row))))))
		  (setf count 0)))))

      ;; black horizontal
      (dotimes (row height)
	(setf count 0)
	(dotimes (column width)
	  (if (= black (aref board column row))
	      ;; check if it is 3 row and empty in two end?
	      (cond ((>= (incf count) num-in-a-row)
		     (if (< (+ column 1) width)
			 (if (= empty (aref board (+ column 1) row))
			     ;;(incf count-black)
			     (setf count-black (+ count-black (* 6 num-in-a-row)))))
		     (if (>= (- column num-in-a-row) 0)
			 (if (= empty (aref board (- column num-in-a-row) row))
			     ;; (incf count-black)
			     (setf count-black (+ count-black (* 6 num-in-a-row)))))))
	      ;; reset count there it is not black
	      (setf count 0))))

      ;; red horizontal
      (dotimes (row height)
	(setf count 0)
	(dotimes (column width)
	  (if (= red (aref board column row))
	      ;; check if it is 3 row and empty in two end?
	      (cond ((>= (incf count) num-in-a-row)
		     (if (< (+ column 1) width)
			 (if (= empty (aref board (+ column 1) row))
			     ;;(incf count-red)
			     (setf count-red (+ count-red (* 6 num-in-a-row)))))
			     
		     (if (>= (- column num-in-a-row) 0)
			 (if (= empty (aref board (- column num-in-a-row) row))
			     ;;(incf count-red)
			     (setf count-red (+ count-red (* 6 num-in-a-row)))))))
	      (setf count 0))))

      ;; diagonal - black /
      (dotimes (column (- width (1- num-in-a-row)))
	(dotimes (row (- height (1- num-in-a-row)))
	  (setf in-row nil)
	  (if (= empty (aref board column row)) (return) ;; quick bail
	      (cond ((= black (aref board column row))
		     (setf in-row t)
		     (dotimes (item (1- num-in-a-row))
		       (if (= black (aref board (+ (1+ item) column) (+ (1+ item) row)))
			   (setf in-row (and in-row t))
			   (setf in-row (and in-row nil))))
		     (cond (in-row   ;; check two ends of the row 
			    (if (and (>= (1- row) 0) (>= (1- column) 0))
				(if (= empty (aref board (1- column) (1- row)))
				    ;;(incf count-black)
				    (setf count-black (+ count-black (* 6 num-in-a-row)))))
			    (if (and (< (+ num-in-a-row column) width) (< (+ num-in-a-row row) height))
				(if (= empty (aref board (+ num-in-a-row column) (+ num-in-a-row row)))
				    ;;(incf count-black)
				    (setf count-black (+ count-black (* 6 num-in-a-row))))))))))))
      ;; (print count-black)
      ;; diagonal - red /
      (dotimes (column (- width (1- num-in-a-row)))
	(dotimes (row (- height (1- num-in-a-row)))
	  (setf in-row nil)
	  (if (= empty (aref board column row)) (return) ;; quick bail
	      (cond ((= red (aref board column row))
		     (setf in-row t)
		     (dotimes (item (1- num-in-a-row))
		       ;;(print item)
		       (if (= red (aref board (+ (1+ item) column) (+ (1+ item) row)))
			   (setf in-row (and in-row t))
			   (setf in-row (and in-row nil))))
		     (cond (in-row   ;; check two ends of the row
			    (if (and (>= (1- row) 0) (>= (1- column) 0))
				(if (= empty (aref board (1- column) (1- row)))
				    ;; (incf count-red)
				    (setf count-red (+ count-red (* 6 num-in-a-row)))
				    ))
			    (if (and (< (+ num-in-a-row column) width) (< (+ num-in-a-row row) height))
				(if (= empty (aref board (+ num-in-a-row column) (+ num-in-a-row row)))
				    ;;(incf count-red)
				    (setf count-red (+ count-red (* 6 num-in-a-row)))
				    )))))))))

      ;; diagonal - black \
      (dotimes (column (- width (1- num-in-a-row)))
	(dotimes (row-reverse (- height (1- num-in-a-row)))
	  (setf row (1- (- height row-reverse)))
	  (setf in-row nil)
	  (cond ((= black (aref board column row))
		 (setf in-row t)
		 (dotimes (item (1- num-in-a-row))
		   ;;(print item)
		   (if (= black (aref board (+ column (1+ item)) (- row (1+ item))))
		       (setf in-row (and in-row t))
		       (setf in-row (and in-row nil))))
		 (cond (in-row   ;; check two ends of the row
			;; check the left end

			(if (and (< (1+ row) height) (>= (1- column) 0))
			    (if (= empty (aref board (1- column) (1+ row)))
				;;(incf count-black)
				(setf count-black (+ count-black (* 6 num-in-a-row)))
				))
			;; check the right end

			(if (and (< (+ num-in-a-row column) width) (>= (- row num-in-a-row) 0))
			    (if (= empty (aref board (+ num-in-a-row column) (- row num-in-a-row)))
				;; (incf count-black)
				(setf count-black (+ count-black (* 6 num-in-a-row)))
				))))))))
      ;; diagonal - red \
      (dotimes (column (- width (1- num-in-a-row)))
	(dotimes (row-reverse (- height (1- num-in-a-row)))
	  (setf row (1- (- height row-reverse)))
	  (setf in-row nil)
	  (cond ((= red (aref board column row))
		 (setf in-row t)
		 (dotimes (item (1- num-in-a-row))
		   (if (= red (aref board (+ column (1+ item)) (- row (1+ item))))
		       (setf in-row (and in-row t))
		       (setf in-row (and in-row nil))))
		 (cond (in-row   ;; check two ends of the row
			;; check the left end
			(if (and (< (1+ row) height) (>= (1- column) 0))
			    (if (= empty (aref board (1- column) (1+ row)))
				;;(incf count-red)
				(setf count-red (+ count-red (* 6 num-in-a-row)))
				))
			;; check the right end
			(if (and (< (+ num-in-a-row column) width) (>= (- row num-in-a-row) 0))
			    (if (= empty (aref board (+ num-in-a-row column) (- row num-in-a-row)))
				;; (incf count-red)
				(setf count-red (+ count-red (* 6 num-in-a-row)))
				)))))))))
    ;;(if (funcall is-maxs-turn-p game)
	(- count-black count-red)))
	;;(- count-red count-black))))
    

(defun is-maxs-turn-p (game)
  (if (= (elt game 1) 1)
      (return-from is-maxs-turn-p t))
  (if (= (elt game 1) -1)
      (return-from is-maxs-turn-p nil)))
  


;; I've decided to make this function available to you

(defun make-computer-move (game depth)
  "Makes a move automatically by trying alpha-beta on all possible moves and then picking
the one which had the highest value for max., and making that move and returning the new game."

  (let* ((max (turn game)))
    (max-element (all-moves game)
		 (lambda (g)
		   (alpha-beta g 0 depth 
			       (lambda (gm) (= (turn gm) max)) ;; is-maxs-turn-p
			       (lambda (gm) (all-moves gm)) ;; expand
			       (lambda (gm) (game-over gm)) ;; terminal-p
			       #'evaluate ;; evaluate
			       min-wins
			       max-wins)))))


;; go back to cl-user
(in-package :cl-user)


"

                           Report for Project 5: Connect Four Player 
                                    Hoang Dung Bui
                                        G001301478

In this project, a Connect-Four Player game program  was developed. The main tasks of the programs are: calculate the heuristic evaluation for each state (game) and do the Alpha-beta Search. To calculate the heuristic value for each state, I wrote two functions: heuristic-evaluation and is-maxs-turn-p. The detail of each function were described below.

1. Program Explanation
- is-maxs-turn-p: This function simply goes to the third element of the list (game) and check the value. If the value is 1 (black), the function will return TRUE. Otherwise, it returns NIL.

- heuristic-evaluation: this function calculate the heuristic value of a input state. The value is calculated based on the substraction between the number of num-in-a-row consecutive black checkers and num-in-a-row consecutive red checkers. I only count the consecutive checkers which have empty squares in at least in one end of the row. The number of consecutive checkers are determined by row, column, up diagonal and down diagonal. The red and black checkers are processed by separated loops. The calculation for the diagonal consecutive checkers take most of the time of the program. For each checker color, the next num-in-a-row of checkers in diagonal direction will be checked. If there are more the same color checkers than the num-in-a-row, the algorithm checks there is any empty square at two ends. If yes, then the consecutive checker row will be counted.

- evaluate: this function returns the score for any state. If the state is the terminal, it returns the winner (black = 1000, red = -1000). It uses the heuristic-evaluation function to calculate the score for the non-terminal state. 

- alpha-beta: it is a recursive algorithm. At first, it checks whether the current state is terminal and it is the max-depth. If yes, it use the evaluate function to calculate the score for that state and return it. If not, it will determine all the children state of the current states and calculate their score by the recursive call to itself. It divide the program into two braches: the max turn and the min turn. If it is the max turn, it will return the max score from all the children. Otherwise, it returns the min score from its children. 


2. Experiment
2.1 Adjusting the computer-depth
If the computer-depth is set smaller than or equal to 5, the algorithm is mostly defeated by me. Sometimes, as set the computer goes first, it can win.

As the computer-depth is set larger than 5, the game is more intesting and I got difficulty to defeat the computer. If I go first, I have chance to win the game. Otherwise, mostly I lost.

As testing with computer-depth = 8, the processing time of the computer is much longer, however, it get more challenge to win the computer.

2.2 Adjusting the board's size
The search time is proportionaly with the board's size and the computer-depth.
As change the board size to 10x10, the computer-depth should not be over than 6. I tried comptuer-depth = 8, then it consumed nearly 30 seconds for each search.



"
