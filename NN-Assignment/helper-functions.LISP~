
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