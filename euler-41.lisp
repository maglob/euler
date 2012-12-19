;; euler 41: What is the largest n-digit pandigital prime that exists?
(load "/Users/marko/prg/lisp/toolkit.lisp")

(defun pandigital? (n &aux (bitmask 0))
  (do* ((i 0 (1+ i))
       (n n (floor (/ n 10))))
       ((= n 0) (= bitmask (ash (1- (ash (the integer 1) i)) 1)))
     (setf bitmask (boole boole-ior bitmask (ash 1 (mod n 10))))))

(defun euler-41(&aux (max 0))
  (dotimes (n 9 max)
    (dolist (s (permutate (range (1+ n))))
      (let ((x (reduce (lambda (x y) (+ (* x 10) y)) s)))
	(if (prime? x)
	    (setf max (max max x)))))))

(defun euler-41-c()
  (reduce #'max 
	  (remove-if-not #'prime? 
			 (mapcar (lambda (z) (reduce (lambda (x y) (+ (* x 10) y)) 
						     z)) 
				 (reduce #'append 
					 (mapcar (lambda (n) (permutate (range n))) 
						 (range 9)))))))



