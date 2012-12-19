;;;; Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

(defun power-digit-sum(n e)
  (if (zerop n)
      0
      (+ (expt (mod n 10) e) 
	 (power-digit-sum (floor (/ n 10)) e))))

(defun euler-30(&optional (e 5))
  (do ((i 10 (1+ i))
       (mx (* (expt 9 e) e))
       (sum 0))
       ((> i mx) sum)
       (if (= i (power-digit-sum i e))
	   (incf sum i))))
