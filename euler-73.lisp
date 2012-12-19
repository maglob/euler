;; How many fractions lie between 1/3 and 1/2 in the sorted set of reduced proper fractions for d  12,000?


;; from euler-71
(defun euler-73 (&optional (limit 12000) (low 1/3) (high 1/2))
  (let ((res 0))
    (dolist (d (range 2 limit) res)
      (let ((nn (* (numerator high) (ceiling (/ d (denominator high))))))
	(do* ((n nn (1- n))
	      (r (/ n d) (/ n d)))
	     ((<= r low))
	  (if (and (< r high)
		   (= (gcd n d) 1))
	      (incf res )))))))