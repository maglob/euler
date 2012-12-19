;;;; What is the sum of both diagonals in a 1001 by 1001 spiral formed in the same way?

(defun diagsum(n)
  (if (= n 1)
      1
      (let ((m (* n n))
	    (ms (1- n)))
	(+ m (- m ms) (- m ms ms) (- m ms ms ms)
	   (diagsum (- n 2))))))

(defun euler-28(&optional (limit 1001))
  (diagsum limit))