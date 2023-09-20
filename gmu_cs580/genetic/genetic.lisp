
;;; Project 3
;;;
;;; A SIMPLE GENETIC ALGORITHM OPERATING OVER FLOATING-POINT VECTORS
;;;
;;; DUE: MIDNIGHT THE EVENING OF THURSDAY, MARCH 25
;;;

;;; Some utility Functions and Macros that you might find to be useful (hint)

(defmacro while (test &rest body)
  "Repeatedly executes body as long as test returns true.  Then returns nil."
  `(loop while ,test do (progn ,@body)))

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
(defparameter *tournament-size* 10)

(defun tournament-select-one (population fitnesses)
  "Does one tournament selection and returns the selected individual."

  ;;; IMPLEMENT ME
  (let ((tournament-size *tournament-size*) best candidate)
    ;;; select the first candidate
    (setf best (elt population (random (length population))))
    ;;; make a loop of tournament-size -1
    (dotimes (i (- tournament-size 1))
      (setf candidate (elt population (random (length population))))
      (let ((fit-can (funcall fitnesses candidate)) (fit-best (funcall fitnesses best)))
	(if (> fit-can fit-best)
	    (setf best candidate)))) 
    best))


(defun tournament-selector (num population fitnesses)
  "Does NUM tournament selections, and puts them all in a list, then returns the list"
  (let ((output-list nil))
    (dotimes (x num)
	   (push (tournament-select-one population fitnesses) output-list))
    output-list))

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

  ;; setup local variables:
  (let ((population nil) (best-individual nil) (select-items nil) fitness-value (fitness-best -50) (num 2) (modified-vectors nil) (Q nil))
    
    ;; create the population - ;;(CREATOR) -  creates a random individual
    (setf population (generate-list pop-size creator t))
    ;(setf select-items (tournament-selector num population evaluator))
    ;(setf select-items (funcall selector num population evaluator))
    ;(print select-items)
    
    (dotimes (i generations)
      ;;; determine the best candidate
      
      (dolist (item population)
	(setf fitness-value (funcall evaluator item))
	(cond ((< fitness-best fitness-value)
	       (setf fitness-best fitness-value)
	       (setf best-individual item))))  ;;; end of dolist

      ;; prints the best individual in the population, plu  its fitness, 
      (format t "~%Best Individual of Generation ~d ~%Fitness: ~a~%Individual:~a~%"
      i fitness-best best-individual)
      
      (setf Q nil)
      (dotimes (j (/ pop-size 2))
	;;(SELECTOR num pop fitneses) given a population and a list of corresponding fitnesses,
	;; selects and returns NUM individuals as a list.
	;; An individual may appear more than once in the list.
	(setf select-items (funcall selector num population evaluator))
	(setf modified-vectors (funcall modifier (first select-items) (first (rest select-items))))
	(push (first modified-vectors) Q)
	(push (first (rest modified-vectors)) Q)) ;;; end of the second dotimes
      (setf population (copy-tree Q)))
    )) 

;;;;;; FLOATING-POINT VECTOR GENETIC ALGORTITHM

(defparameter *float-vector-length* 20 
  "The length of the vector individuals")
(defparameter *float-min* -5.12 
  "The minimum legal value of a number in a vector") 
(defparameter *float-max* 5.12 
  "The maximum legal value of a number in a vector")

(defun float-vector-creator ()
  "Creates a floating-point-vector *float-vector-length* in size, filled with
UNIFORM random numbers in the range appropriate to the given problem"
  (let ((float-vector nil) (i 0))
	(while (< i *float-vector-length*)
	       (push (+ *float-min*  (random (- *float-max* *float-min*))) float-vector)
	       (incf i))
	float-vector))


;; I just made up these numbers, you'll probably need to tweak them
(defparameter *crossover-probability* 0.12
  "Per-gene probability of crossover in uniform crossover")
(defparameter *mutation-probability* 0.15
  "Per-gene probability of mutation in gaussian convolution") 
(defparameter *mutation-variance* 0.05
  "Per-gene mutation variance in gaussian convolution")



(defun uniform-crossover (ind1 ind2)
  "Performs uniform crossover on the two individuals, modifying them in place.
*crossover-probability* is the probability that any given allele will crossover.  
The individuals are guaranteed to be the same length.  Returns NIL."
    (if (= (length ind1) (length ind2))
	 (dotimes (i (length ind1))
	   (if (> *crossover-probability* (random 1.0))
	       (rotatef (elt ind1 i) (elt ind2 i))))
	(print "Uniform-crossover - difference in length between two vectors")))



(defun gaussian-convolution (ind)
  "Performs gaussian convolution mutation on the individual, modifying it in place.
 Returns NIL."
  (let ((n 12.0))
    (dotimes (i (length ind))
      (cond ((> *mutation-probability* (random 1.0))
	       (while (not (and (< *float-min* (+ (elt ind i) n)) (> *float-max* (+ (elt ind i) n))))
		      (setf n (gaussian-random 0 *mutation-variance*)))
		(setf (elt ind i) (+ (elt ind i) n)))))))


(defun float-vector-modifier (ind1 ind2)
  "Copies and modifies ind1 and ind2 by crossing them over with a uniform crossover,
then mutates the children.  *crossover-probability* is the probability that any
given allele will crossover.  *mutation-probability* is the probability that any
given allele in a child will mutate.  Mutation does gaussian convolution on the allele."
  (let ((ind1-copy (copy-tree ind1)) (ind2-copy (copy-tree ind2)))
    (uniform-crossover ind1-copy ind2-copy)
    (gaussian-convolution ind1-copy)
    (gaussian-convolution ind2-copy)
    (list ind1-copy ind2-copy)))


;; you probably don't need to implement anything at all here
(defun float-vector-sum-setup ()
  "Does nothing.  Perhaps you might use this function to set
(ahem) various global variables which define the problem being evaluated
and the floating-point ranges involved, etc.  I dunno."
  )

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


#|
                           Report's Project 3
                        Hoang Dung Bui, G01301478

In this project, the genetic algorithm was developed. To run the algorithm, it was needed several functions such as float-vector-modifier, gaussian-convolution, uniform-crossover, float-vector-creator, evolve, tournament-selector, and tournament-select-one. The detail of each function were described below.

1. Program Explanation
- float-vector-modifier: This function combined the two functions: gaussian-convolution and uniform-crossover to modify the two parent vectors. It outputted two two children vectors.
 
- gaussian-convolution: it ran through the vector length, and used random function to generate a random number. If this number was smaller than *mutation-probability*, the mutation would be occurred. The amount of change was provided by a normal/gaussian distribution. The sum of current value and the change must be in the range (*float-min* ; *float-max*). It outputed nil.

- uniform-crossover: cross over two floating point vectors by uniform distribution. I used dotimes through the vector length, and used random function to generate a random number. If it was smaller than *crossover-probablity*, the swap would be occurred.

- float-vector-creator: I used the while function to generate a vector of length *float-vector-length*.

- tournament-selector: It generated a set of num individuals which were used as the parents for the genetic algorith. Usually, there were two individuals selected by this function. The dotimes loop was used to choose the individuals.

- tournament-select-one: the inputs were the population and fitness function. I used dotimes loop to select *tournament-size* individual, then select the best one as the output. To call the fitness function, the function funcall is used.

- evolve: This was the main function to perform the genetic algorithm. I used a dotimes loop to run through all the generations. In each generation, firstly all individuals were compared with the best. They would be swapped if any individual was better than the best. After that, the new population were generated by modified each parents pairs (mutation and recombination). I used another dotimes loop to perform task. The new population then substituted the old one. 

2. Experiment and Model's parameters 
The algorithm was tested and ran well on all the fitness functions: sum-f, step-f, sphere-f, rosenbrock-f, rastrigin-f, and schwefel-f. There were some interesting things as experimenting with them.
- Evaluator = sum-f, step-f, sphere-f
As testing many of parameter set, the ones following provided the best result: *crossover-probability* = 0.12, *mutation-probability* = 0.12, *mutation-variance* = 0.02, with the average highest fitness is 102.22. The worst case was 102.17.
 
Comments: As increasing the values of *crossover-probability*, *mutation-probability*, and *mutation-variance*, the genetic algorithm cannot reach the maxima value. The reason could be the large parameters make the algorithm swing too much, and usually jump over the optimal value. 

As reducing the parameters, it got the similar result. However, it sometimes outputed horrible result such as 97.07. It could get stuck in local optimal.

- Evaluator = rastrigin-f:
As testing many of parameter set, the ones following provided the best result:  generation = 100, *tournament-size* = 8, *crossover-probability* = 0.17, *mutation-probability* = 0.12, *mutation-variance* = 0.05, with the highest fitness is -0.2, the average fitness was around -2.61, and the worst case was -4.2
As changing the *tournament-size* from 10 to 55 does not affect much to the fitness result. Thus, small *tournament-size* will make the algorithm faster. 

- Evaluator = rosenbrock-f:
As testing many of parameter set, the ones following provided the best result:  generation = 1000, *tournament-size* = 10, *crossover-probability* = 0.12, *mutation-probability* = 0.15, *mutation-variance* = 0.05, the average fitness was around -16.6, and the worst case was -21.5. There was one time the fitness reaching with the highest fitness is -0.723.

There was problem with this fitness function. If the paramters  *crossover-probability*, *mutation-probability*, *mutation-variance* were set small, it took many generations for the algorithm founding the optimal point. If increasing the parameters' values, the algorithm could determine the optimal faster, but it swang too much, and could not get the global optima as the small cases.

|#
