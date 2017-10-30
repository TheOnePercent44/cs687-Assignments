
;; IMPLEMENT THIS FUNCTION

(defun sigmoid (u)
  "Sigmoid function applied to the number u"
	(/ 1 (+ 1 (exp (* -1 u))))
  )


;; output and correct-output are both column-vectors
;; IMPLEMENT THIS FUNCTION

(defun net-error (output correct-output)
  "Returns (as a scalar value) the error between the output and correct vectors"
  (* 0.5 (let ((r 0)) 
           (dotimes (i (length (- output 1)))
             (+ r (power (- (e output 1 (+ i 1)) (e correct-output 1 (+ i 1))) 2)))
           r))
  )

(defun power (m n)
  (let ((r 1))
    (dotimes (i n)
      (setf r (* r m)))
    r))


(defun net-build (data num-hidden-units alpha initial-bounds max-iterations modulo &optional print-all-errors)
  "Builds a neural network with num-hidden-units and the appropriate number
of input and output units based on the data.  Each element should be a random
value between -(INITIAL-BOUNDS) and +(INITIAL-BOUNDS).

Then performs the following loop MAX-ITERATIONS times, or until the error condition
is met (see below):

   1. For each data element in a randomized version of the data, perform
      backpropagation.
   2. Every modulo iterations,
          For every data element in the data, perform forward propagation and
          A.  If print-all-errors is true, then print the error for each element
          B.  At any rate, always print the worst error and the mean error
          C.  If the worst error is better (lower) than A-GOOD-MINIMUM-ERROR,
              quit all loops and prepare to exit the function --
              the error condition was met.

The function should return a list of two items: the final V matrix
and the final W matrix of the learned network."
  )


(defun simple-generalization (data num-hidden-units alpha initial-bounds max-iterations)
  "Given a set of data, trains a neural network on the first half
of the data, then tests generalization on the second half, returning
the average error among the samples in the second half.  Don't print any errors,
and use a modulo of MAX-ITERATIONS."
  )

(defun forward-propagate (datum v w)
  "Returns as a vector the output of the OUTPUT units when presented
the datum as input."
  )

(defun back-propagate (datum alpha v w)
  "Back-propagates a datum through the V and W matrices,
returning a list consisting of new, modified V and W matrices."
  ;; Consider using let*
  ;; let* is like let, except that it lets you initialize local
  ;; variables in the context of earlier local variables in the
  ;; same let* statement.
  )