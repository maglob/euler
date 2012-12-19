;; By listing the set of reduced proper fractions for d  1,000,000 in ascending order of size, find the numerator of the fraction immediately to the left of 3/7.

(defun euler-71 (&optional (limit 1000000))
  (let ((res 2/5))
    (dolist (i (range 8 limit) (numerator res))
      (let ((d i)
	    (nn (* 3 (ceiling (/ i 7)))))
	(do* ((n nn (1- n))
	      (r (/ n d) (/ n d)))
	     ((< r res))
	  (if (and (< r 3/7)
		   (= (gcd n d) 1))
	      (setf res r)))))))