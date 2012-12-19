;; How many elements would be contained in the set of reduced proper fractions for d 1,000,000?

(load "~/Dropbox/prg/euler/toolkit.lisp")

(defun totient (n)
  "Algorithm from http://en.wikipedia.org/wiki/Euler's_totient_function"
  (let ((pf (histogram (prime-factors n))))
    (apply #'*
	   (mapcar (lambda (x) 
		     (let ((p (car x)) 
			   (k (cadr x))) 
		       (* (expt p k) (- 1 (/ p))))) pf))))

(defun euler-72 (&optional (limit 1000000))
  (reduce #'+ (mapcar #'totient (range 2 limit))))