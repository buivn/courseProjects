;;;; ASSIGNMENT 5
;;;;
;;;; Connect-Four Player
;;;; Due Midnight, the evening of WEDNESDAY, APRIL 28.
;;;;
;;;;
;;;; You will provide the TA a single file (a revised version of this one).
;;;; You'll write the heuristic board evaluation function and the alpha-beta searcher
;;;; for a CONNECT FOUR game.  If you don't know how to play connect four, and can't
;;;; find stuff online, get ahold of me.  The version of connect four we'll be playing
;;;; is NON-TOROIDAL (non-"wraparound").
;;;;
;;;;
;;;;
;;;; HOW TO USE THIS TEMPLATE FILE
;;;;
;;;; This is the file you will fill out and provide to the TA.  It contains your project-specific
;;;; code.  Don't bother providing the connectfour.lisp file -- we have it, thanks.
;;;;
;;;; I'm providing you with one function the MAKE-COMPUTER-MOVE function.  This function 
;;;; takes a game state and generates all one-move-away states from there.  It then calls 
;;;; ALPHA-BETA on those states returns the new game state which got the highest alpha-beta 
;;;; score.  Piece of cake.
;;;;
;;;; You will write:
;;;;
;;;; 1. The ALPHA-BETA function.  This is a straightforward implementation of the
;;;;    function described in the lecture notes.  However to make your player better
;;;;    you MIGHT (or might not! -- it could make things worse) sort the expanded children
;;;;    based on who's best, so as to increase the chance that alpha-beta will cut off
;;;;    children early and improve your search time.  Look into the SORT function.
;;;;
;;;; 2. The EVALUATE function.  Or more specifically, you'll fill out one part of
;;;;    it (the progn block shown).  Here you'll provide an intelligent heuristic
;;;;    evaluator.  Right now the evaluator provided ("0") literally says that
;;;;    all unfinished games are draws.  That's pretty stupid -- surely you can
;;;;    do better than that!  You're welcome, and perhaps encouraged, to write your
;;;;    own separate function(s) and call it from within the EVALUATE function so as
;;;;    to keep the EVALUATE function code clean.  Keep whatever functions and
;;;;    auxillary code to within this file please.
;;;;
;;;; Your code should work for any width or height board, and for any number of
;;;; checkers in a row to win (at least 2).  These values can be extracted from
;;;; game board functions in the connectfour.lisp file.  Indeed, I'd strongly suggest
;;;; examining all of that file as it may contain some valuable functions and constants
;;;; that you might want to know about.
;;;;
;;;; Your code will not only be in this file but will be in its own package as well.
;;;; Your package name will be :firstname-lastname
;;;; For example, the package name for MY package is called :sean-luke
;;;; will change the text :TEMPLATE with your :firstname-lastname package name
;;;; in the TWO places it appears in this file.
;;;;
;;;; You will then name your file "firstname-lastname.lisp".  For example, I would
;;;; name my own file "sean-luke.lisp"
;;;;
;;;;
;;;; RUNNING THE CODE
;;;;
;;;; You'll want to compile your code first, like I do to my code in this example:
;;;;
;;;; (load "connectfour")
;;;; (compile-file "connectfour.lisp")
;;;; (load "connectfour")
;;;; (load "sean-luke")
;;;; (compile-file "sean-luke.lisp")
;;;; (load "sean-luke")
;;;;
;;;; Now we're ready to go.  If you want to just do a quick human-human game, you
;;;; don't need to compile and load your own file, just the connectfour file.
;;;;
;;;; You can run a simple human-against-human example like this:
;;;;
;;;; (play-human-human-game)
;;;;
;;;; You can run a simple human-against-computer example like this (replace sean-luke
;;;; with your own package name in this example of course)
;;;;
;;;; (play-human-computer-game #'sean-luke:make-computer-move t)  
;;;;
;;;; ...or if you wish the computer to go first,
;;;;
;;;; (play-human-computer-game #'sean-luke:make-computer-move nil)
;;;;
;;;; You can play a head-to-head game against two computer players (loading both of course)
;;;; like this:
;;;;
;;;; (play-game #'sean-luke:make-computer-move #'keith-sullivan:make-computer-move)
;;;;
;;;; Note that for all three of these functions (play-human-human-game, play-human-computer-game,
;;;; play-game) there are lots of optional keywords to change the width o the board, the height,
;;;; the number in a row that must be achieved, the search depth, whether boards are printed, etc.
;;;;
;;;;
;;;;
;;;; TESTING POLICY
;;;;
;;;; Plagiarism rules still apply (no code given, no code borrowed from anyone or any source
;;;; except the TA and the Professor, do not lay eyes on other people's code anywhere, including
;;;; online).  However they are softened in the following fashion:
;;;;
;;;;    - You are strongly encouraged to play against each other as long as neither of you
;;;;      looks at each others' code nor discusses your alpha-beta code.  Go on the forum and
;;;;      ask for partners.
;;;;
;;;;    - You are NOT permitted to discuss, in any way, the ALPHA-BETA function with anyone
;;;;      except ME and the TA.  That has to be done entirely on your own. 
;;;;
;;;;    - You ARE allowed to discuss, in general and HIGHLY ABSTRACT terms, your own approach
;;;;      to the EVALUATE function.  You are not allowed to show code or read another student's
;;;;      code.  But you can talk about your theory and general approach to doing board evaluation
;;;;      for connect four.  Let a thousand heuristics bloom!  You are also welcome to
;;;;      go online and get ideas about how to do the EVALUATE function as long as you do NOT
;;;;      read code.  I'm just as good as the next person -- perhaps even better -- at gathering
;;;;      every scrap of connect four code on the web and comparing your code against it.
;;;;
;;;; OTHER POLICIES
;;;;
;;;; - This code may have, and almost certainly has, bugs.  I'll post revisions as needed.
;;;;
;;;; - Near the end of the exam we will have a single-elimination tournament competition.
;;;;      The winner of the competition gets to play against the professor's "better" evaluator
;;;;      (assuming I've written one by then) and ALSO receives an improvement in his overall
;;;;      course grade.  Runners up may receive something smaller: at least a candy bar, perhaps
;;;;      a better incentive.
;;;;
;;;; - If your evaluator is too slow, you will be penalized with a cut in your search depth.
;;;;   Thus it's in your best interest to have as good an evaluator/depth combination as possible.
;;;;   Note that it's often the case that evaluators that are good on ODD depths are bad on EVEN
;;;;   depths (or vice versa!).  I won't say what depth will be used in the tournament.
;;;;
;;;; - To make your code run as fast as possible, I strongly suggest you look into Lisp compiler optimization.  SBCL's profiler might prove helpful too.
;;;;
;;;; Good luck!
;;;;
;;;;

(defpackage :template 
  (:use "COMMON-LISP" "COMMON-LISP-USER")
  (:export "MAKE-COMPUTER-MOVE"))

(in-package :template)

(defun alpha-beta (game current-depth max-depth
		   is-maxs-turn-p expand terminal-p evaluate
		   alpha beta)
 "Does alpha-beta search. 
is-maxs-turn-p takes one argument (a game) and returns true if it's max's turn in that game.  
expand takes one argument (a game) and gives all the immediate child games for this game.
terminal-p takes one argument (a game) and returns true if the game is over or not.
evaluate takes TWO arguements (a game and the is-maxs-turn-p function) and provides a quality
assessment of the game for 'max', from min-wins (-1000) to max-wins (1000)."

;;; IMPLEMENT ME
)



(defun evaluate (game is-maxs-turn-p)
  "Returns an evaluation, between min-wins and max-wins inclusive, for the game.
is-maxs-turn-p is a function which, when called and passed the game, returns true
if it's max's turn to play."
  (let ((end (game-over game)))
    (if (null end) ;; game not over yet

	(progn

	  ;;; IMPLEMENT ME
	  ;; in this block, do your code which returns a heuristic
	  ;; evaluation of the system.  Feel free to create an outside function
	  ;; and call it if you don't want all the code here.

	  0   ;; by default we're returning 0 (draw).  That's obviously wrong.

	  ;;; END IMPLEMENTATION

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

	    (* end (turn game) max-wins (if (funcall is-maxs-turn-p game) 1 -1))))))



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
