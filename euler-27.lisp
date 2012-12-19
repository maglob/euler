;;;; n² + an + b, where |a| < 1000 and |b| < 1000
;;;; Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.

(defun primep(n)
  (cond ((= n 2) t)
	((or (< n 2) (zerop (mod n 2))) nil)
	(t (do ((i 3 (+ i 2)))
	       ((> (* i i) n) t)
	     (if (zerop (mod n i))
		 (return-from primep nil)))))))

(defun prime-run(fn)
  (do ((i 0 (1+ i)))
      ((not (primep (funcall fn i))) i)))
       
(defun euler-27(&optional (limit 1000))
  (do ((a (- limit) (1+ a))	 
       (r (list 0 0 0)))
      ((>= a limit) (* (cadr r) (caddr r)))
    (do ((b (- limit) (1+ b)))
	((>= b limit))
      (let ((run (prime-run #'(lambda(x)(+ (* x x) (* a x) b)))))
	(if (> run (car r))
	    (setq r (list run a b)))))))

