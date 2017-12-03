

;;; Project 1: Build a simple evolutionary computation system.

;;; This is a nontrivial project to start with.  Do not discuss
;;; programming issues related to this project with other groups.
;;; If you have questions, contact me directly.  You may post to
;;; the Wiki questions of a general nature that you'd like people
;;; to respond to.

;;; Unquestionably the hardest code is the GP crossover operator and
;;; the PTC2 code.  For information about the Symbolic Regression
;;; and Artificial Ant problems, I have posted some chunks of my
;;; thesis to the web page to peruse.

;;; Please don't deviate much from this file -- I'm just looking for
;;; you to fill out these functions below and maybe add a bit more.  Rearranging
;;; things into multiple files etc. may be convenient for you but it's MUCH
;;; tougher to grade.

#|
In this project you will produce three things:

1. A high-level evolutionary computation framework

2. The representation, breeding functions, and evaluations to do a simple
GA for boolean vectors and for floating-point vectors, including a few test
functions on each.

3. The representation, breeding functions, and evaluations to do GP for
two problems:
A. Symbolic Regression
B. Artificial Ant

The high-level EC system will work as follows:

- Simple generational evolution
- GA-style Tournament selection
- A simple breeding function
- some simple statistics functions

I have provided the approximate function templates I myself used to complete
the task; with permission you can use these or go your own way, but otherwise
please try not to deviate much from this template.

The project is due approximately two and a half weeks from now or so.  Please 
try to get it in on time.

WHAT YOU MUST PROVIDE:

1. Completed code which works and compiles.  As simple as possible would be nice.

2. A short report describing the code you wrote and some experimentation with it.
Some things to try:
   -- different problems (in the vector section)
   -- different settings of mutation and crossover parameters
   -- different population sizes or run lengths
Try to get a handle on how problem difficulty changes when you tweak various
parameters.  Can you get mutation and crossover parameters which seem optimal for
a given problem for example?  (Obviously bigger population sizes are more optimal
but that's kinda cheating).  Note that this is a STOCHASTIC problem so you'll need
to run a number of times and get the mean best result in order to say anything of
consequence.  It'd be nice if the report were in LaTeX and it'd be good practice for
you as well, but it's not necessary.  I do not accept reports in Word.  Only send
me a PDF.

Make sure your code is compiled.  In most cases (such as SBCL), your code will be
automatically compiled.  But there exist systems (like LispWorks ("lisp" on 
mason.gmu.edu)) where the default is to interpret the code, and so you must 
compile your file first and then load that.

Information on compiling code and doing compiler optimizations can be found in the
"Speed" chapter of Graham.
|#


;;; Useful Functions and Macros

(defmacro swap (elt1 elt2)
  "Swaps elt1 and elt2, using SETF.  Returns nil."
  (let ((temp (gensym)))
    `(let ((,temp ,elt1))
       (setf ,elt1 ,elt2)
       (setf ,elt2 ,temp)
       nil)))

(defmacro while (test return-value &body body)
  "Repeatedly executes body as long as test returns true.
Then evaluates and returns return-value"
  `(progn (loop while ,test do (progn ,@body)) ,return-value))

(defun random-elt (sequence)
  "Returns a random element from sequence"
  (elt sequence (random (length sequence))))

(defun random? (&optional (prob 0.5))
  "Tosses a coin of prob probability of coming up heads,
then returns t if it's heads, else nil."
  (< (random 1.0) prob))

(defun generate-list (num function &optional no-duplicates)
  "Generates a list of size NUM, with each element created by
  (funcall FUNCTION).  If no-duplicates is t, then no duplicates
are permitted (FUNCTION is repeatedly called until a unique
new slot is created).  EQUALP is the test used for duplicates."
  (let (bag)
    (while (< (length bag) num) bag
      (let ((candidate (funcall function)))
	(unless (and no-duplicates
		          (member candidate bag :test #'equalp))
	    (push candidate bag))))))







;;;;;; TOP-LEVEL EVOLUTIONARY COMPUTATION FUNCTIONS 





;;; TOURNAMENT SELECTION

(defparameter *tournament-size* 7)
;Anthony
(defun tournament-select-one (population fitnesses)
  "Does one tournament selection and returns the selected individual."
  (let* ((r (random (length population))) (best-i r) (best-fit (elt fitnesses r)) next)
    (dotimes (i *tournament-size* t)
      (setf r (random (length population)))
      (setf next (elt fitnesses r))
      (if (> next best-fit) (progn (setf best-fit (elt fitnesses r)) (setf best-i r)) nil))
    (elt population best-i))
  ;;; IMPLEMENT ME  
  )


;Anthony
(defun tournament-selector (num population fitnesses)
  "Does NUM tournament selections, and puts them all in a list"
  (loop for i from 1 to num
        collect (tournament-select-one population fitnesses))
  ;;; IMPLEMENT ME
  )

;Anthony
;copied the lambda from simple-printer to make my life easier in the event we need to return the best individual
(defun find-best (pop fitnesses)
  (let (best-ind best-fit)
    (mapcar #'(lambda (ind fit)
		(when (or (not best-ind)
			    (< best-fit fit))
		    (setq best-ind ind)
		      (setq best-fit fit))) pop fitnesses)
    best-ind))

(defun simple-printer (pop fitnesses)  ;; I'm nice and am providing this for you.  :-)
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

;Anthony
(defun evolve (generations pop-size
			          &key setup creator selector modifier evaluator printer)
  "Evolves for some number of GENERATIONS, creating a population of size
POP-SIZE, using various functions"
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

    ;;; IMPLEMENT ME
    
    ;;for pop-size times:
    ;;~Create random individual and add to population P (CREATOR)
    ;;print best from initial population (given function above)
    (funcall setup)
    (let ((population (generate-list pop-size creator))
          (gen-num 0)
          fitnesses)
      (while (< gen-num generations) population
          (let (next-gen)
            (setf fitnesses (mapcar evaluator population))
            ;(print fitnesses) ;just a little debug statement
            (dotimes (i (/ pop-size 2) t)
              (let* ((parents (funcall selector 2 population fitnesses))
                    (children (funcall modifier (elt parents 0) (elt parents 1))))
                (setf next-gen (append next-gen children))))
            (funcall printer population fitnesses)
            (setf population next-gen)
            ;(print population)
            (setf gen-num (+ 1 gen-num))
            )
        )
      (setf fitnesses (mapcar evaluator population)) ;fitnesses for final population
      (funcall printer population fitnesses)
      (find-best population fitnesses) ;;We may not need to return the best in the examples below, but the algorithm does
      )

    ;;while (converging? num iterations?):
    ;(while
    ;;~for all individuals in population
    ;;~~assess and store fitness (EVALUATOR)
    ;;~~if best fitness so far, store
    ;;~Initialize empty next generation Q
    ;;~for pop-size/2 times:
    ;;~~select two random parents with replacement (tourney select twice; SELECTOR)
    ;;~~Create two children via crossover (MODIFIER)
    ;;~~randomly mutate children (also in MODIFIER- this step is included in the above)
    ;;~~add children to Q
    ;;~replace P with Q
    ;;~print best of generation P (given func above)
    
    ;;return Best of last generation P
  )










;;;;;; BOOLEAN VECTOR GENETIC ALGORTITHM






;;; Creator, Modifier, Evaluator, and Setup functions for the
;;; boolean vectors Problem.  Assume that you are evolving a bit vector
;;; *vector-length* long.  

;;; The default test function is Max-Ones.
;;;; Max-ones is defined in section 11.2.1 of "Essentials of Metaheuristics"
;;; by yours truly.  In that section are defined several other boolean functions
;;; which you should also try and report on.  Some plausible ones might
;;; include:

;;; :max-ones
;;; :trap
;;; :leading-ones
;;; :leading-ones-blocks



(defparameter *boolean-vector-length* 100)
(defparameter *boolean-problem* :leading-ones-blocks)
;; perhaps other problems might include... 

;Anthony
(defun boolean-vector-creator ()
  "Creates a boolean-vector *boolean-vector-length* in size, filled with
random Ts and NILs, or with random 1s and 0s, your option."
    ;;; IMPLEMENT ME
    (let (vec)
      (dotimes (i *boolean-vector-length* vec)
        (setf vec (cons (if (random?) 1 0) vec)) ;;a 0-1 option
        ;(setf vec (cons (random?) vec))
        )
      vec)
  )


(defparameter *boolean-crossover-probability* 0.2)
(defparameter *boolean-mutation-probability* 0.01)
;Anthony
(defun boolean-vector-modifier (ind1 ind2)
  "Copies and modifies ind1 and ind2 by crossing them over with a uniform crossover,
then mutates the children.  *crossover-probability* is the probability that any
given allele will crossover.  *mutation-probability* is the probability that any
given allele in a child will mutate.  Mutation simply flips the bit of the allele."
  (let ((child1 (copy-list ind1)) (child2 (copy-list ind2)) children)
    (setf children (dotimes (i *boolean-vector-length* (list child1 child2))
                     (if (random? *boolean-crossover-probability*) (swap (elt child1 i) (elt child2 i)) nil)))
    (dotimes (i *boolean-vector-length* children)
      ;(if (random? *boolean-mutation-probability*) (setf (elt (elt children 0) i) (not (elt (elt children 0) i)))) ;t/nil version
      (if (random? *boolean-mutation-probability*) (setf (elt (elt children 0) i) (if (= (elt (elt children 0) i) 1) 0 1)));1/0 version
      ;(if (random? *boolean-mutation-probability*) (setf (elt (elt children 1) i) (not (elt (elt children 1) i)))) ;t/nil version
      (if (random? *boolean-mutation-probability*) (setf (elt (elt children 1) i) (if (= (elt (elt children 1) i) 1) 0 1)));1/0 version
      )
  )
)

;Anthony
;;Just do whichever test function our parameter says to do
(defun boolean-vector-evaluator (ind1)
  "Evaluates an individual, which must be a boolean-vector, and returns
its fitness."
  (if (eql *boolean-problem* :max-ones) (max-ones-test ind1) 
  (if (eql *boolean-problem* :leading-ones) (leading-ones-test ind1) 
  (if (eql *boolean-problem* :leading-ones-blocks) (leading-ones-blocks-test ind1) nil)))
    ;;; IMPLEMENT ME
)

;Anthony
;;Made this to turn the evaluator into a switch statement basically
;;we can make more test functions and just change the *boolean-problem* keyword parameter (which is what I assume it's there for)
(defun max-ones-test (ind1)
  (let ((fitness 0))
    (dotimes (i *boolean-vector-length* fitness)
      ;(if (elt ind1 i) (setf fitness (+ 1 fitness)) nil) ;t/nil version
      (if (= 1 (elt ind1 i)) (setf fitness (+ 1 fitness)) nil) ;1/0 version
      )
    fitness)
)

;Anthony
(defun leading-ones-test (ind1)
  (let ((fitness 0))
    (dotimes (i *boolean-vector-length* fitness)
      ;(if (elt ind1 i) (setf fitness (+ 1 fitness)) (return)) ;t/nil version
      (if (= 1 (elt ind1 i)) (setf fitness (+ 1 fitness)) (return)) ;1/0 version
      )
    fitness)
)

(defparameter *lob-blocknum* 4)
;Anthony
(defun leading-ones-blocks-test (ind1)
  (let ((fitness 0))
    (dotimes (i *boolean-vector-length* fitness)
      ;(if (elt ind1 i) (setf fitness (+ 1 fitness)) (return)) ;t/nil version
      (if (= 1 (elt ind1 i)) (setf fitness (+ 1 fitness)) (return)) ;1/0 version
      )
    (floor fitness *lob-blocknum*)) ;leading-ones is just this but blocknum is set to 1
)


;Anthony- well, not really at the moment
(defun boolean-vector-sum-setup ()
  "Does nothing.  Perhaps you might use this to set up various
(ahem) global variables to define the problem being evaluated, I dunno."

 ;;Got nothing for this for now.
  )


;;; an example way to fire up the GA.  It should easily discover
;;; a 100% correct solution.
#|
(evolve 50 100
	:setup #'boolean-vector-sum-setup
	:creator #'boolean-vector-creator
	:selector #'tournament-selector
	:modifier #'boolean-vector-modifier
        :evaluator #'boolean-vector-evaluator
	:printer #'simple-printer)
|#







;;;;;; FLOATING-POINT VECTOR GENETIC ALGORTITHM




;;; Creator, Modifier, Evaluator, and Setup functions for the
;;; GA Max-ONES Problem.  Assume that you are evolving a vector
;;; of floating-point numbers *float-vector-length* long.  


;;; The default test function is Rastrigin.
;;;; Rastrigin is defined in section 11.2.2 of "Essentials of Metaheuristics"
;;; by yours truly.  In that section are defined several other floating-point functions
;;; which you should also try and report on.  Some plausible ones might
;;; include:

;;; :rastrigin
;;; :rosenbrock
;;; :griewank
;;; :schwefel

;;; If you were really into this, you might also try testing on
;;; rotated problems -- some of the problems above are linearly
;;; separable and rotation makes them harder and more interesting.
;;; See the end of Section 11.2.2.

;;; I have defined the minimum and maximum gene values for rastrigin
;;; as [-5.12, 5.12].  Other problems have other traditional min/max
;;; values, consult Section 11.2.2.



(defparameter *float-vector-length* 100)

(defparameter *float-problem* :rastrigin)
;(defparameter *float-problem* :rosenbrock)
(defparameter *float-min* -5.12)  ;; these will change based on the problem
(defparameter *float-max* 5.12)   ;; likewise

;aakarshika
(defun float-vector-creator ()
  "Creates a floating-point-vector *float-vector-length* in size, filled with
random numbers in the range appropriate to the given problem"
    ;;; IMPLEMENT ME
    ;;; you might as well use random uniform numbers from *float-vector-min*
    ;;; to *float-vector-max*.  
  #|(let ((population-vec (make-array *float-vector-length*)))
    (dotimes (i *float-vector-length*)
      (setf 
        (aref population-vec i) 
        (- (random (* 2 *float-max*)) *float-max*) ; (random 2*max)-max  ---> gets random val from -max to +max
      )
    )
  population-vec)|#
  (let (vec)
      (dotimes (i *float-vector-length* vec)
        (setf vec (cons (- (random (* 2 *float-max*)) *float-max*) vec))
        )
      vec)
)

;aakarshika
(defun gauz-mutate (vec min max p variance)
  "Adds gaussian noise to vec"
  (let ()
    (dotimes (i (length vec))
      (if (> p (random 1.0))
        (let ((noise (random variance)))
          (loop while (or 
                (> (+ (elt vec i) noise) max) 
                (< (+ (elt vec i) noise) min))
            do 
            (setf noise (random variance))
          )
          (setf (elt vec i) (+ (elt vec i) noise))
        )
      )
    )
  vec
  )
  
)

(defparameter *float-crossover-probability* 0.2)
(defparameter *float-mutation-probability* 0.1)   ;; I just made up this number
(defparameter *float-mutation-variance* 0.01)     ;; I just made up this number

;aakarshika
(defun float-vector-modifier (ind1 ind2)
  "Copies and modifies ind1 and ind2 by crossing them over with a uniform crossover,
then mutates the children.  *crossover-probability* is the probability that any
given allele will crossover.  *mutation-probability* is the probability that any
given allele in a child will mutate.  Mutation does gaussian convolution on the allele."

    ;;; IMPLEMENT ME
    ;;; Note: crossover is probably identical to the bit-vector crossover
    ;;; See "Gaussian Convolution" (Algorithm 11) in the book for mutation
  (let ((child1 (copy-list ind1)) (child2 (copy-list ind2)) children)
    ;(print "Doing crossover")
    (setf children (dotimes (i *float-vector-length* (list child1 child2))
                     (if (random? *float-crossover-probability*) (swap (elt child1 i) (elt child2 i)) nil)))
    ;(print "doing mutate")
    (setf (elt children 0) (gauz-mutate (elt children 0) *float-min* *float-max* *float-mutation-probability* *float-mutation-variance*))
    (setf (elt children 1) (gauz-mutate (elt children 1) *float-min* *float-max* *float-mutation-probability* *float-mutation-variance*))
    children
  )
)


;aakarshika
(defun rastrigin-eval(x)
  (let ((fitness 0) (n (length x)))
    (dotimes (i (length x))
      ;(print x)
      (setf fitness 
        (+ fitness 
          (-  (* (elt x i) (elt x i)) 
              (* 10 (cos (* 2 pi (elt x i))))
          )
        )
      )
    )
    (+ fitness (* 10 n))
  )
)

;aakarshika
(defun rosenbrock-eval(x)
  (let ((fitness 0) (n (length x)))
    (dotimes (i (- (length x) 1))
      (let ((xi (elt x i)) (xi2 (* (elt x i) (elt x i))))
        (setf fitness 
            (+ fitness 
              (* (- 1 xi) (- 1 xi)) 
              (* 100 (* (elt x (+ 1 i)) (* xi2 xi2)))
            )
        )
      )
    )
    fitness
  )
)



(defun float-vector-sum-evaluator (ind1)
  "Evaluates an individual, which must be a floating point vector, and returns
its fitness."

    ;;; IMPLEMENT ME
    ;(rastrigin-eval ind1)
    ;(rosenbrock-eval ind1)
    (if (eql *float-problem* :rastrigin) (rastrigin-eval ind1)
    (if (eql *float-problem* :rosenbrock) (rosenbrock-eval ind1) nil))
)




(defun float-vector-sum-setup ()
  "Does nothing.  Perhaps you might use this function to set
(ahem) various global variables which define the problem being evaluated
and the floating-point ranges involved, etc.  I dunno."
  )



;;; an example way to fire up the GA.  

#|
(evolve 50 100
	:setup #'float-vector-sum-setup
	:creator #'float-vector-creator
	:selector #'tournament-selector
	:modifier #'float-vector-modifier
        :evaluator #'float-vector-sum-evaluator
	:printer #'simple-printer)
|#













;;;; GP TREE CREATION CODE

;;; GP's tree individuals are considerably more complex to modify than
;;; simple vectors.  Get the GA system working right before tackling
;;; this part, trust me.



;; set up in the gp setup function -- for example, see
;; the code for gp-symbolic-regression-setup
(defparameter *nonterminal-set* nil)
(defparameter *terminal-set* nil)



;;; important hint: to use SETF to change the position of an element in a list
;;; or a tree, you need to pass into SETF an expression starting with the
;;; parent.  For example (setf (car parent) val) .... thus you HAVE to have
;;; access to the parent, not just the child.  This means that to store a
;;; "position", you have to store away the parent and the arg position of
;;; child, not the child itself.

(defun make-queue ()
  "Makes a random-queue"
  (make-array '(0) :adjustable t :fill-pointer t))
(defun enqueue (elt queue)
  "Enqueues an element in the random-queue"
  (progn (vector-push-extend elt queue) queue))
(defun queue-empty-p (queue)
  "Returns t if random-queue is empty"
  (= (length queue) 0))
(defun random-dequeue (queue)
  "Picks a random element in queue and removes and returns it.
Error generated if the queue is empty."
  (let ((index (random (length queue))))
    (swap (elt queue index) (elt queue (1- (length queue))))
    (vector-pop queue)))

(defun ptc2 (size)
  "If size=1, just returns a random terminal.  Else builds and
returns a tree by repeatedly extending the tree horizon with
nonterminals until the total number of nonterminals in the tree,
plus the number of unfilled slots in the horizon, is >= size.
Then fills the remaining slots in the horizon with terminals.
Terminals like X should be added to the tree
in function form (X) rather than just X."

(if (= size 1) (random-elt *terminal-set*)
  (let ((unfilled-positions (make-queue)) root (new-op (random-elt *nonterminal-set*)) (count 1) slot-path current-node)
    (setf root (append (list (first new-op)) (make-list (second new-op))))
    (dotimes (slot (second new-op))
      (enqueue (list '() (list (1+ slot))) unfilled-positions))
    (loop while (< (+ count (length unfilled-positions)) size)
          do (setf slot-path (random-dequeue unfilled-positions))
          do (setf new-op (random-elt *nonterminal-set*))
          do (incf count)
          do (setf current-node root)
          do (dolist (index (first slot-path))
               (setf current-node (elt current-node index)))
          do (setf (elt current-node (first (second slot-path))) (append (list (first new-op)) (make-list (second new-op))))
          do (dotimes (slot (second new-op)) 
               (enqueue (list (append (first slot-path) (second slot-path)) (list (1+ slot))) unfilled-positions)))
    (loop while (not (queue-empty-p unfilled-positions))
          do (setf slot-path (random-dequeue unfilled-positions))
          do (setf new-op (random-elt *terminal-set*))
          do (setf current-node root)
          do (dolist (index (first slot-path))
               (setf current-node (elt current-node index)))
          do (setf (elt current-node (first (second slot-path))) (list new-op)))
    root))

  #|
  The simple version of PTC2 you will implement is as follows:

  PTC2(size):
  if size = 1, return a random terminal
  else
     q <- make-queue
     root <- random nonterminal
     count <- 1
     enqueue into q each child argument slot of root
     Loop until count + size_of_q >= size
        remove a random argument slot s from q
        a <- random nonterminal
        count <- count + 1
        fill the slot s with a
        enqueue into q each child argument slot of a
     Loop until size_of_q = 0
        remove a random argument slot s from q
        a <- random terminal
        fill the slot s with a
     return root


  Note that "terminals" will all be considered to be
  zero-argument functions.  Thus PTC might generate the
  s-expression tree:

  (cos (+ (x) (- (x) (x)) (sin (x))))

  but it should NOT generate the tree:

  (cos (+ x (- x x) (sin x)))


  Note that this has some gotchas: the big gotcha is that you have to keep track of
  argument slots in lisp s-expressions, and not just pointers to the values presently
  in those slots.  If you are totally lost as to how to implement that, I can provide
  some hints, but you should try to figure it out on your own if you can.
  |#

  ;;; IMPLEMENT ME
)

(defparameter *size-limit* 20)
(defun gp-creator ()
  "Picks a random number from 1 to 20, then uses ptc2 to create
a tree of that size"

    ;;; IMPLEMENT ME
    (ptc2 (1+ (random *size-limit*))))



;;; GP TREE MODIFICATION CODE

(defun num-nodes (tree)
  "Returns the number of nodes in tree, including the root"
  (if (listp tree) 
    (reduce #'+ (mapcar #'num-nodes tree)) 
    1))


(defun nth-subtree-parent (tree n)
  "Given a tree, finds the nth node by depth-first search though
the tree, not including the root node of the tree (0-indexed). If the
nth node is NODE, let the parent node of NODE is PARENT,
and NODE is the ith child of PARENT (starting with 0),
then return a list of the form (PARENT i).  For example, in
the tree (a (b c d) (f (g (h) i) j)), let's say that (g (h) i)
is the chosen node.  Then we return ((f (g (h) i) j) 0).

If n is bigger than the number of nodes in the tree
 (not including the root), then we return n - nodes_in_tree
 (except for root)."

  ;;; this is best described with an example:
  ;    (dotimes (x 12)
  ;           (print (nth-subtree-parent
  ;                       '(a (b c) (d e (f (g h i j)) k))
  ;                        x)))
  ;;; result:
  ;((A (B C) (D E (F (G H I J)) K)) 0) 
  ;((B C) 0) 
  ;((A (B C) (D E (F (G H I J)) K)) 1) 
  ;((D E (F (G H I J)) K) 0) 
  ;((D E (F (G H I J)) K) 1) 
  ;((F (G H I J)) 0) 
  ;((G H I J) 0) 
  ;((G H I J) 1) 
  ;((G H I J) 2) 
  ;((D E (F (G H I J)) K) 2) 
  ;0
  ;1
  ;NIL

    ;;; IMPLEMENT ME
    (if (>= (1+ n) (num-nodes tree))
        (- (1+ n) (num-nodes tree))
      (let (val (iter 1))
        (loop while (>= n 0)
              do (if (= n 0)
                     (setf val (list tree (1- iter)))
                   (if (listp (elt tree iter))
                       (setf val (nth-subtree-parent (elt tree iter) (1- n)))))
              do (decf n (num-nodes (elt tree iter)))
              do (incf iter))
        val)))


(defparameter *mutation-size-limit* 10)
(defun gp-modifier (ind1 ind2)
  "Flips a coin.  If it's heads, then ind1 and ind2 are
crossed over using subtree crossover.  If it's tails, then
ind1 and ind2 are each mutated using subtree mutation, where
the size of the newly-generated subtrees is pickedc at random
from 1 to 10 inclusive.  Doesn't damage ind1 or ind2.  Returns
the two modified versions as a list."

    ;;; IMPLEMENT ME
    (if (random?)
         ;subtree crossover
        (let* ((tree1 (copy-tree ind1)) (tree2 (copy-tree ind2)) 
              (sub-index1 (random (num-nodes tree1))) (sub-index2 (random (num-nodes tree2)))
              (sub-path1 (nth-subtree-parent tree1 sub-index1)) (sub-path2 (nth-subtree-parent tree2 sub-index2))
              (sub1 (copy-tree (elt (first sub-path1) (second sub-path1)))) (sub2 (copy-tree (elt (first sub-path2) (second sub-path2)))))
          (setf (elt (first sub-path1) (second sub-path1)) sub2)
          (setf (elt (first sub-path2) (second sub-path2)) sub1)
          (list tree1 tree2))
       ;subtree mutation
      (let* ((tree1 (copy-tree ind1)) (tree2 (copy-tree ind2)) 
             (sub-index1 (random (num-nodes tree1))) (sub-index2 (random (num-nodes tree2)))
             (sub-path1 (nth-subtree-parent tree1 sub-index1)) (sub-path2 (nth-subtree-parent tree2 sub-index2)))
        (setf (elt (first sub-path1) (second sub-path1)) (ptc2 (1+ (random 10))))
        (setf (elt (first sub-path2) (second sub-path2)) (ptc2 (1+ (random 10))))
        (list tree1 tree2))))






;;; SYMBOLIC REGRESSION
;;; This problem domain is similar, more or less, to the GP example in
;;; the lecture notes.  Your goal is to make a symbolic expression which
;;; represents a mathematical function consisting of SIN COS, EXP,
;;; +, -, *, and % (a version of / which doesn't give an error on
;;; divide-by-zero).  And also the function X, which returns a current
;;; value for the X variable.
;;;
;;; In symbolic regression, we generate 20 (x,y) pairs produced at
;;; random which fit the expression y = x^4 + x^3 + x^2 + x.  You can
;;; make up another function is you like, but that's the one we're going
;;; with here.  Using just these data pairs, the GP system will attempt
;;; to ascertain the function.  It will generate possible expressions
;;; as its individuals, and the fitness of an expression is how closely,
;;; for each X value, it gives the correct corresponding Y value.
;;;
;;; This is called SYMBOLIC regression because we're actually learning
;;; the mathematical expression itself, including transcendental functions
;;; like SIN and COS and E^.  As opposed to statistical linear or
;;; quadratic curve-fitting regressions which just try to learn the
;;; linear or quadratic parameters etc.
;;;
;;; An example 100% optimal solution:
;;;
;;; (+ (* (x) (* (+ (x) (* (x) (x))) (x))) (* (+ (x) (cos (- (x) (x)))) (x)))



;;; GP SYMBOLIC REGRESSION SETUP
;;; (I provide this for you)

(defparameter *num-vals* 20)
(defparameter *vals* nil) ;; gets set in gp-setup

(defun gp-symbolic-regression-setup ()
  "Defines the function sets, and sets up vals"

  (setq *nonterminal-set* '((+ 2) (- 2) (* 2) (% 2) (sin 1) (cos 1) (exp 1)))
  (setq *terminal-set* '(x))

  (setq *vals* nil)
  (dotimes (v *num-vals*)
    (push (1- (random 2.0)) *vals*)))

(defun poly-to-learn (x) (+ (* x x x x) (* x x x) (* x x) x))

;; define the function set
(defparameter *x* nil) ;; to be set in gp-evaluator
(defun x () *x*)
(defun % (x y) (if (= y 0) 0 (/ x y)))  ;; "protected division"
;;; the rest of the functions are standard Lisp functions




;;; GP SYMBOLIC REGRESSION EVALUATION

(defun gp-symbolic-regression-evaluator (ind)
  "Evaluates an individual by setting *x* to each of the
elements in *vals* in turn, then running the individual and
get the output minus (poly-to-learn *x*).  Take the
absolute value of the this difference.  The sum of all such
absolute values over all *vals* is the 'raw-fitness' Z.  From
this we compute the individual's fitness as 1 / (1 + z) -- thus
large values of Z are low fitness.  Return the final
individual's fitness.  During evaluation, the expressions
evaluated may overflow or underflow, or produce NaN.  Handle all
such math errors by
returning most-positive-fixnum as the output of that expression."
  ;;; hint:
  ;;; (handler-case
  ;;;  ....
  ;;;  (error (condition)
  ;;;     (format t "~%Warning, ~a" condition) most-positive-fixnum))


  ;;; IMPLEMENT ME
  )


;;; Example run
#|
(evolve 50 500
	:setup #'gp-symbolic-regression-setup
	:creator #'gp-creator
	:selector #'tournament-selector
	:modifier #'gp-modifier
        :evaluator #'gp-symbolic-regression-evaluator
	:printer #'simple-printer)
|#





;;; GP ARTIFICIAL ANT CODE
;;; for this part you'll need to implement both the evaluator AND
;;; make up the function that form the function set.

;;; In the artificial ant problem domain, you'll be evolving an s-expression
;;; which moves an ant around a toroidal map shown below.  The ant starts out
;;; at (0,0), facing east (to the right).  The functions in
;;; the expression are:
;;; (if-food-ahead --then-- --else--)   If food is directly ahead of the ant,
;;;                                     then evaluate the THEN expression, else
;;;                                     evaluate the ELSE expression
;;; (progn2 --item1-- --item2--)        Evaluate item1, then evaluate item 2
;;; (progn3 --item1-- --item2-- --item3--)  Evaluate item1, then item2, then item3
;;; (move)                              Move forward one unit
;;;                                     If you pass over a food pellet, it is eaten
;;;                                     and removed from the map.
;;; (left)                              Turn left
;;; (right)                             Turn right
;;;
;;;
;;; the way a tree is evaluated is as follows.  The whole tree is evaluated,
;;; and the functions move the ant about.  If the ant has not made a total of
;;; 600 MOVE, LEFT, and RIGHT operations yet, then the tree is evaluated AGAIN
;;; moving the ant some more from its current position.  This goes on until
;;; 600 operations have been completed, at which time the ant will not move
;;; any more.  If 600 operations are completed in the middle of the evaluation
;;; of a tree, the simplest approach is to have the MOVE command simply
;;; "stop working" so the ant doesn't gobble up any more pellets accidentally.

;;; The fitness of the artificial ant is the number of pellets it ate in the
;;; 600-operation period.  Maps are reset between evaluations of different
;;; individuals of course.

;;; Here's an optimal ant program (there are many such):
;;  (progn3 (if-food-ahead (move) (progn2 (left) (progn2 (left)
;;             (if-food-ahead (move) (right))))) (move) (right)))

;;; This is a hard problem for GP and you may need to run many times before you
;;; get a 100% perfect answer.

;;; Note that in my thesis it says 400 moves and not 600.  We're going with
;;; 600 here.  It's easier.



;;; our ant's food trail map
(defparameter *map-strs* '(
".###............................"
"...#............................"
"...#.....................###...."
"...#....................#....#.."
"...#....................#....#.."
"...####.#####........##........."
"............#................#.."
"............#.......#..........."
"............#.......#..........."
"............#.......#........#.."
"....................#..........."
"............#..................."
"............#................#.."
"............#.......#..........."
"............#.......#.....###..."
".................#.....#........"
"................................"
"............#..................."
"............#...#.......#......."
"............#...#..........#...."
"............#...#..............."
"............#...#..............."
"............#.............#....."
"............#..........#........"
"...##..#####....#..............."
".#..............#..............."
".#..............#..............."
".#......#######................."
".#.....#........................"
".......#........................"
"..####.........................."
"................................"))

(defparameter *map-height* 32)
(defparameter *map-width* 32)


;;; some useful functions for you

(defun make-map (lis)
  "Makes a map out of a string-list such as *MAP-STRS*"
  (let ((map (make-array (list (length (first lis)) (length lis)))))
    (dotimes (y (length lis) map)
      (dotimes (x (length (elt lis y)))
  (setf (aref map x y)
              (cond ((equalp #\# (elt (elt lis y) x)) nil)
            (t t)))))))

(defun direction-to-arrow (dir)
  "Returns a character which represents a given direction -- might
be useful for showing the movement along a path perhaps..."
  (cond ((= dir *n*) #\^)
  ((= dir *s*) #\v)
  ((= dir *e*) #\>)
  (t #\<)))

(defun maparray (function array &optional (same-type nil))
  "Maps function over array in its major order.  If SAME-TYPE,
then the new array will have the same element type as the old
array; but if a function returns an invalid element then an error
may occur.  If SAME-TYPE is NIL, then the array will accommodate
any type."
  (let ((new-array (apply #'make-array (array-dimensions array)
          :element-type (if same-type
                    (array-element-type array)
                t)
            :adjustable (adjustable-array-p array)
              (if (vectorp array)
                  `(:fill-pointer ,(fill-pointer array))
              nil))))
    (dotimes (x (array-total-size array) new-array)
      (setf (row-major-aref new-array x)
          (funcall function (row-major-aref array x))))))

(defun print-map (map)
  "Prints a map, which must be a 2D array.  If a value in the map
is T (indicating a space), then a '.' is printed.  If a value in the map
is NIL (indicating a food pellet), then a '#' is printed.  If a value in
the map is anything else, then it is simply PRINTed.  This allows you to
consider spaces to be all sorts of stuff in case you'd like to print a
trail of spaces on the map for example.  Returns NIL."
  (let ((dim (array-dimensions map)))
    (dotimes (y (second dim) nil)
      (format t "~%")
      (dotimes (x (first dim))
  (format t "~a"
    (let ((v (aref map x y)))
        (cond ((equal v t) #\.)
        ((null v) #\#)
        (t v))))))))


;; The four directions.  For relative direction, you might
;; assume that the ant always PERCEIVES things as if it were
;; facing north.
(defconstant *n* 0)
(defconstant *e* 1)
(defconstant *s* 2)
(defconstant *w* 3)

(defmacro absolute-direction (relative-dir ant-dir)
  "If the ant is facing ANT-DIR, then converts the perceived
RELATIVE-DIR direction into an absolute ('true') direction
and returns that."
  `(mod (+ ,relative-dir ,ant-dir) 4))

(defmacro x-pos-at (x-pos absolute-dir &optional (steps 1))
  "Returns the new x position if one moved STEPS steps the absolute-dir
direction from the given x position.  Toroidal."
  `(mod (cond ((= (mod ,absolute-dir 2) *n*) ,x-pos)         ;; n or s
              ((= ,absolute-dir *e*) (+ ,x-pos ,steps))     ;; e
              (t (+ ,x-pos (- ,steps) *map-width*)))         ;; w
  *map-width*))

(defmacro y-pos-at (y-pos absolute-dir &optional (steps 1))
  "Returns the new y position if onee moved STEPS steps in the absolute-dir
direction from the given y position.  Toroidal."
  `(mod (cond ((= (mod ,absolute-dir 2) *e*) ,y-pos)        ;; e or w
              ((= ,absolute-dir *s*) (+ ,y-pos ,steps))     ;; s
              (t (+ ,y-pos (- ,steps) *map-height*)))       ;; n
  *map-height*))


(defparameter *current-move* 0 "The move # that the ant is at right now")
(defparameter *num-moves* 600 "How many moves the ant may make")
(defparameter *current-x-pos* 0 "The current X position of the ant")
(defparameter *current-y-pos* 0 "The current Y position of the ant")
(defparameter *current-ant-dir* *e* "The current direction the ant is facing")
(defparameter *eaten-pellets* 0 "How many pellets the ant has eaten so far")
(defparameter *map* (make-map *map-strs*) "The ant's map")




;;; the function set you have to implement


(defun food-found (x y)
(not (aref *map* x y)))

(defmacro if-food-ahead (then else)
  "If there is food directly ahead of the ant, then THEN is evaluated,
else ELSE is evaluated"
  ;; because this is an if/then statement, it MUST be implemented as a macro.

;;;`(if (food-found (x-pos-at *current-x-pos* *current-ant-dir*) (y-pos-at *current-y-pos* *current-ant-dir*)) ,then ,else))
;Not sure how to macro!
    ;;; IMPLEMENT ME


)

(defun progn2 (arg1 arg2)
    "Evaluates arg1 and arg2 in succession, then returns the value of arg2"
    (eval arg1)
    (eval arg2)
arg2)  ;; ...though in artificial ant, the return value isn't used ... 

(defun progn3 (arg1 arg2 arg3)
  "Evaluates arg1, arg2, and arg3 in succession, then returns the value of arg3"
   (eval arg1)
  (eval arg2)
  (eval arg3)
arg3)  ;; ...though in artificial ant, the return value isn't used ...

(defun move ()
  "If the move count does not exceed *num-moves*, increments the move count
and moves the ant forward, consuming any pellet under the new square where the
ant is now.  Perhaps it might be nice to leave a little trail in the map showing
where the ant had gone."
;current x,y
;if food-found x y --- eaten-pellets++
;update position and move

      ;;; IMPLEMENT ME
  )


(defun left ()
  "Increments the move count, and turns the ant left"
 (case *current-ant-dir*
    (0 (setf *current-ant-dir* *w*))
    (1 (setf *current-ant-dir* *n*))
    (3 (setf *current-ant-dir* *s*))
    (2 (setf *current-ant-dir* *e*)))
  (setf *current-move* (1+ *current-move*))

      ;;; IMPLEMENT ME
)

(defun right ()
  "Increments the move count, and turns the ant right"
(case *current-ant-dir*
    (0 (setf *current-ant-dir* *e*))
    (1 (setf *current-ant-dir* *s*))
    (3 (setf *current-ant-dir* *w*))
    (2 (setf *current-ant-dir* *n*)))
(setf *current-move* (1+ *current-move*))
      ;;; IMPLEMENT ME
)

(defparameter *nonterminal-set* nil)
(defparameter *terminal-set* nil)

;; I provide this for you
(defun gp-artificial-ant-setup ()
  "Sets up vals"
  (setq *nonterminal-set* '((if-food-ahead 2) (progn2 2) (progn3 3)))
  (setq *terminal-set* '(left right move))
  (setq *map* (make-map *map-strs*))
  (setq *current-move* 0)
  (setq *eaten-pellets* 0))


;; you'll need to implement this as well

(defun gp-artificial-ant-evaluator (ind)
  "Evaluates an individual by putting it in a fresh map and letting it run
for *num-moves* moves.  The fitness is the number of pellets eaten -- thus
more pellets, higher (better) fitness."

      ;;; IMPLEMENT ME
)

;; you might choose to write your own printer, which prints out the best
;; individual's map.  But it's not required.

#|
(evolve 50 500
  :setup #'gp-artificial-ant-setup
  :creator #'gp-creator
  :selector #'tournament-selector
  :modifier #'gp-modifier
        :evaluator #'gp-artificial-ant-evaluator
  :printer #'simple-printer)
|#

