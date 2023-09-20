
;;; Project 3
;;;
;;; A SIMPLE GENETIC ALGORITHM OPERATING OVER FLOATING-POINT VECTORS
;;;
;;; DUE: MIDNIGHT THE EVENING OF THURSDAY, MARCH 25
;;;
;;; That's about 2 weeks from now.  You have to implement a number
;;; of functions, but none of them is particularly hard.  If you get
;;; really stuck, I am willing to give you help on a function or two
;;; in return for a reduced grade.


;;; Please don't deviate too much from this file.  Rearranging
;;; things into multiple files etc. may be convenient for you but it's MUCH
;;; tougher to grade.


#|
In this project you will do three things:

1. Implement a very simple abstract high-level evolutionary computation framework

2. Implement the functions necessary for a floating-point genetic algorithm

3. Test it on various objective functions and find good settings of parameters
   which work well on those functions


WHAT YOU MUST PROVIDE:

1. Completed code file which works and compiles and is reasonably well documented.

2. IN COMMENTS at the end of the code file, a short report which discusses interesting
   things you discovered as you were implementing the code, and the results of
   an experiment where you attempted to find parameter settings which generally
   produced the best results for various test objective functions.  Keep in mind
   that this algorithm is STOCHASTIC, so sometimes it produces good results and
   sometimes it produces bad ones, based partly on randomness.  So don't rely on
   a single run to determine how good some parameter settings are; perhaps you 
   might run some N > 50 times and take the average to get an idea of how well they
   do compared to others.

I have given you a bunch of objective test problems to try.  They are roughly in
increasing order of "challenge" to the genetic algorithm.  Some parameters you
can fool around with: the tournament size, the mutation rate, the crossover rate,
and the mutation variance.

I would fix the size of the individual, the number of individuals in the population,
and the number of generations to constants -- they're not part of the experiment
though you have to say what you set them to.  Suggested settings: an individual
of size 20, 50 individuals in the population, and oh, I dunno, how about 1000 
generations.  You'll find that as the individual size gets bigger the problem 
gets harder for the GA to solve.  Also as the generations get longer or the
population size gets bigger, the GA is being given more total resources to throw
at your problem so you'd expect better results.

SUBMISSION
Mail the TA your file, including the report. 

|#





;;; Some utility Functions and Macros that you might find to be useful (hint)

(defmacro while (test &rest body)
  "Repeatedly executes body as long as test returns true.  Then returns nil."
  `(loop while ,test do (progn ,@body)))

;;; Example usage
;;;
;;; (let ((x 0))
;;;    (while (< x 5)
;;;        (print x)
;;;        (incf x)))


(defun random? (&optional (prob 0.5))
  "Tosses a coin of prob probability of coming up heads,
then returns t if it's heads, else nil."
  (< (random 1.0) prob))

(defun generate-list (num function &optional no-duplicates)
  "Generates a list of size NUM, with each element created by
  (funcall FUNCTION).  If no-duplicates is t, then no duplicates
are permitted (FUNCTION is repeatedly called until a unique
new slot is created).  EQUALP is the default test used for duplicates."
  (let (bag)
    (while (< (length bag) num)
      (let ((candidate (funcall function)))
  (unless (and no-duplicates
         (member candidate bag :test #'equalp))
    (push candidate bag))))
    bag))

;; hope this works right
(defun gaussian-random (mean variance)
  "Generates a random number under a gaussian distribution with the
given mean and variance (using the Box-Muller-Marsaglia method)"
  (let (x y (w 0))
    (while (not (and (< 0 w) (< w 1)))
     (setf x (- (random 2.0) 1.0))
     (setf y (- (random 2.0) 1.0))
     (setf w (+ (* x x) (* y y))))
    (+ mean (* x (sqrt variance) (sqrt (* -2 (/ (log w) w)))))))






;;;;;; TOP-LEVEL EVOLUTIONARY COMPUTATION FUNCTIONS 


;;; TOURNAMENT SELECTION

;; is this a good setting?  Try tweaking it (any integer >= 2) and see
(defparameter *tournament-size* 7)
(defun tournament-select-one (population fitnesses)
  "Does one tournament selection and returns the selected individual."

  ;;; IMPLEMENT ME
  ;;;
  ;;; See Algorithm 32 of Essentials of Metaheuristics

  ;;; Hints:
  ;;; 1. My implementation is about 7 lines long
  ;;; 2. This would be a reasonable place to judiciously use SETF
  ;;; 3. You might want to do a loop, and keep track of both the
  ;;;    best individual you've found so far and also its fitness

)



(defun tournament-selector (num population fitnesses)
  "Does NUM tournament selections, and puts them all in a list, then returns the list"

  ;;; IMPLEMENT ME
  ;;;
  ;;; Hints:
  ;;; 1. This is a very short function.  My version is 1 line long.
  ;;; 2. Maybe one of the utility functions I provided might be of
  ;;;    benefit here

)



;; I'm nice and am providing this for you.  :-)
(defun simple-printer (pop fitnesses)
  "Determines the individual in pop with the best (highest) fitness, then
prints that fitness and individual in a pleasing manner."
  (let (best-ind best-fit)
    (mapcar #'(lambda (ind fit)
    (when (or (not best-ind)
        (< best-fit fit))
      (setq best-ind ind)
      (setq best-fit fit))) pop fitnesses)
    (format t "~%Best Individual of Generation...~%Fitness: ~a~%Individual:~a~%"
      best-fit best-ind)
    fitnesses))



(defun evolve (generations pop-size
         &key setup creator selector modifier evaluator printer)
  "Evolves for some number of GENERATIONS, creating a population of size
POP-SIZE, using various functions"

  ;;; IMPLEMENT ME
  ;;;
  ;; The functions passed in are as follows:
  ;;(SETUP)                     called at the beginning of evolution, to set up
  ;;                            global variables as necessary
  ;;(CREATOR)                   creates a random individual
  ;;(SELECTOR num pop fitneses) given a population and a list of corresponding fitnesses,
  ;;                            selects and returns NUM individuals as a list.
  ;;                            An individual may appear more than once in the list.
  ;;(MODIFIER ind1 ind2)        modifies individuals ind1 and ind2 by crossing them
  ;;                            over and mutating them.  Returns the two children
  ;;                            as a list: (child1 child2).  Nondestructive to
  ;;                            ind1 and ind2.
  ;;(PRINTER pop fitnesses)     prints the best individual in the population, plus
  ;;                            its fitness, and any other interesting statistics
  ;;                            you think interesting for that generation.
  ;;(EVALUATOR individual)      evaluates an individual, and returns its fitness.
  ;;Pop will be guaranteed to be a multiple of 2 in size.
  ;;
  ;; HIGHER FITNESSES ARE BETTER

  ;; your function should call PRINTER each generation, and also print out or the
  ;; best individual discovered over the whole run at the end, plus its fitness
  ;; and any other statistics you think might be nifty.

  ;;; HINTS: 
  ;;; 1. You could do this in many ways.  But I implemented it using
  ;;;    the following functions (among others)
  ;;;    FUNCALL FORMAT MAPCAR LAMBDA APPLY
  ;;; 2. My version of this function is about 20 lines long.
  ;;;    This sounds long but it's really pretty straightforward
  ;;;    code with no particular tricks.
  ;;; 3. Pay attention to the keyword arguments


)









;;;;;; FLOATING-POINT VECTOR GENETIC ALGORTITHM


;;; Here you will implement creator, modifier, and setup functions for
;;; individuals in the form of lists of floating-point values.  
;;; I have provided some objective functions which you can use as
;;; fitness evaluation functions.

;;; If you were really into this, you might try implementing an evolution
;;; strategy instead of a genetic algorithm and compare the two.
;;;
;;; If you were really REALLY into this, I have an extension of this
;;; project which also does genetic programming as well.  That is a much
;;; MUCH MUCH MUCH more difficult project.



(defparameter *float-vector-length* 20 
  "The length of the vector individuals")
(defparameter *float-min* -5.12 
  "The minimum legal value of a number in a vector") 
(defparameter *float-max* 5.12 
  "The maximum legal value of a number in a vector")

(defun float-vector-creator ()
  "Creates a floating-point-vector *float-vector-length* in size, filled with
UNIFORM random numbers in the range appropriate to the given problem"

  ;;; IMPLEMENT ME
  ;;;
  ;;; The numbers must be uniformly randomly chosen between *float-min* and
  ;;; *float-max*.  See the documentation for the RANDOM function.

  ;;; HINTS:
  ;;; 1. My version is 3 lines long.
  ;;; 2. Maybe a function I provided in the utilities might be handy.

)



;; I just made up these numbers, you'll probably need to tweak them
(defparameter *crossover-probability* 0.1
  "Per-gene probability of crossover in uniform crossover")
(defparameter *mutation-probability* 0.1
  "Per-gene probability of mutation in gaussian convolution") 
(defparameter *mutation-variance* 0.02
  "Per-gene mutation variance in gaussian convolution")




;; to impement FLOAT-VECTOR-MODIFIER, the following two functions are
;; strongly reccommended.


(defun uniform-crossover (ind1 ind2)
  "Performs uniform crossover on the two individuals, modifying them in place.
*crossover-probability* is the probability that any given allele will crossover.  
The individuals are guaranteed to be the same length.  Returns NIL."

  ;;; IMPLEMENT ME
  ;;;
  ;;; For crossover: use uniform crossover (Algorithm 25) in
  ;;;                Essentials of Metaheuristics

  ;;; HINTS:
  ;;; 1. DOTIMES, ELT, and ROTATEF
  ;;; 2. My version of this function is 2 lines long.


)


(defun gaussian-convolution (ind)
  "Performs gaussian convolution mutation on the individual, modifying it in place.
 Returns NIL."

  ;;; IMPLEMENT ME
  ;;;
  ;;; For mutation, see gaussian convolution (Algorithm 11) in
  ;;;                Essentials of Metaheuristics
  ;;; Keep in mind the legal minimum and maximum values for numbers.

  ;;; HINTS:
  ;;; 1. My version of this function is 6 lines long.
  ;;; 2. Maybe a function or three in the utility functions above 
  ;;;    might be handy
  ;;; 3. See also SETF


)









(defun float-vector-modifier (ind1 ind2)
  "Copies and modifies ind1 and ind2 by crossing them over with a uniform crossover,
then mutates the children.  *crossover-probability* is the probability that any
given allele will crossover.  *mutation-probability* is the probability that any
given allele in a child will mutate.  Mutation does gaussian convolution on the allele."

    ;;; IMPLEMENT ME
    ;;; It's pretty straightforward.
    ;;; This function should first COPY the two individuals, then
    ;;; CROSS THEM OVER, then mutate the result using gaussian covolution,
    ;;; then return BOTH children together as a list (child1 child2)
    ;;;
    ;;; HINTS:
    ;;; 1. My version of this function is 6 lines long but it's
    ;;;    really, really simple.
    ;;; 2. For copying lists:  See the Lisp Cheat Sheet 
    ;;;    (http://cs.gmu.edu/~sean/lisp/LispCheatSheet.txt)

)


;; you probably don't need to implement anything at all here
(defun float-vector-sum-setup ()
  "Does nothing.  Perhaps you might use this function to set
(ahem) various global variables which define the problem being evaluated
and the floating-point ranges involved, etc.  I dunno."
  )





;;; FITNESS EVALUATION FUNCTIONS

;;; I'm providing you with some classic objective functions.  See section 11.2.2 of
;;; Essentials of Metaheuristics for details on these functions.
;;;
;;; Many of these functions (sphere, rosenbrock, rastrigin, schwefel) are
;;; traditionally minimized rather than maximized.  We're assuming that higher
;;; values are "fitter" in this class, so I have taken the liberty of converting
;;; all the minimization functions into maximization functions by negating their
;;; outputs.  This means that you'll see a lot of negative values and that's fine;
;;; just remember that higher is always better.
;;; 
;;; These functions also traditionally operate with different bounds on the
;;; minimum and maximum values of the numbers in the individuals' vectors.  
;;; Let's assume that for all of these functions, these values can legally
;;; range from -5.12 to 5.12 inclusive.  One function (schwefel) normally goes from
;;; about -511 to +512, so if you look at the code you can see I'm multiplying
;;; the values by 100 to properly scale it so it now uses -5.12 to 5.12.


(defun sum-f (ind)
  "Performs the Sum objective function.  Assumes that ind is a list of floats"
  (reduce #'+ ind))

(defun step-f (ind)
  "Performs the Step objective function.  Assumes that ind is a list of floats"
  (+ (* 6 (length ind))
     (reduce #'+ (mapcar #'floor ind))))

(defun sphere-f (ind)
  "Performs the Sphere objective function.  Assumes that ind is a list of floats"
  (- (reduce #'+ (mapcar (lambda (x) (* x x)) ind))))

(defun rosenbrock-f (ind)
  "Performs the Rosenbrock objective function.  Assumes that ind is a list of floats"
  (- (reduce #'+ (mapcar (lambda (x x1)
         (+ (* (- 1 x) (- 1 x))
            (* 100 (- x1 (* x x)) (- x1 (* x x)))))
       ind (rest ind)))))

(defun rastrigin-f (ind)
  "Performs the Rastrigin objective function.  Assumes that ind is a list of floats"
  (- (+ (* 10 (length ind))
  (reduce #'+ (mapcar (lambda (x) (- (* x x) (* 10 (cos (* 2 pi x)))))
          ind)))))

(defun schwefel-f (ind)
  "Performs the Schwefel objective function.  Assumes that ind is a list of floats"
  (- (reduce #'+ (mapcar (lambda (x) (* (- x) (sin (sqrt (abs x)))))  
       (mapcar (lambda (x) (* x 100)) ind)))))




;;; an example way to fire up the GA.  If you've got it tuned right, it should quickly
;;; find individuals which are all very close to +5.12

#|
(evolve 50 1000
  :setup #'float-vector-sum-setup
  :creator #'float-vector-creator
  :selector #'tournament-selector
  :modifier #'float-vector-modifier
        :evaluator #'sum-f
  :printer #'simple-printer)
|#
