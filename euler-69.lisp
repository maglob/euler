;; Find the value of n  1,000,000 for which n/φ(n) is a maximum.

(load "~/Dropbox/prg/euler/toolkit.lisp")

(defun totient (n)
  "Algorithm from http://en.wikipedia.org/wiki/Euler's_totient_function"
  (let ((pf (histogram (prime-factors n))))
    (apply #'*
	   (mapcar (lambda (x) 
		     (let ((p (car x)) 
			   (k (cadr x))) 
		       (* (expt p k) (- 1 (/ p))))) pf))))

(defun euler-69 (&optional (n 1000000))
  (values-list (reduce (lambda (a b) (if (> (cadr a) (cadr b)) a b)) 
		       (mapcar (lambda (x) (list x (/ x (totient x)))) 
			       (range 2 n)))))