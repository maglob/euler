(defun divisors(x &optional (d (floor (sqrt x))))
  (cond
	 ((= 1 d) (list 1))
	 ((zerop (mod x d)) (append (list d (/ x d))
				    (divisors x (1- d))))
	 (t (divisors x (1- d)))))


(defun div(x)
  (remove-duplicates (divisors x)))

(defun divsum(x)
  (apply #'+ (div x)))

(defun euler-21()
  (let ((sum 0))
    (dotimes (i 10000 sum) (if (> i 1)
			       (let ((a (divsum i)))
				 (if (and (= i (divsum a))
					  (/= i a))
				     (incf sum (print i))))))))
