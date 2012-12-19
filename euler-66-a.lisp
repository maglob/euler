;; Find the value of D  1000 in minimal solutions of x for which the largest value of x is obtained.

(load "~/Dropbox/prg/euler/toolkit.lisp")

(defun solution-p (x y d)
  "Predicate for solution for equation: x**2 - d*y**2 = 1"
  (= (- (* x x) (* d y y)) 1))

(defun solve-y (x d)
  "Integer solution for y in: x**2 - d*y**2 = 1"
  (let ((u (1- (* x x))))
    (if (zerop (mod u d))
	(let* ((y (/ u d))
	       (isqy (isqrt y)))
	  (if (and (> y 0) 
		   (= (- (* x x) (* d y) 1))
		   (= y (* isqy isqy)))
	      isqy)))))

(defun solve (d &optional (limit 1000))
  (if (/= (sqrt d) (isqrt d))  
      (dotimes (ii limit)
	(let* ((i (1+ ii))
	       (y (solve-y i d)))
	  (if y
	      (return-from solve (list i y)))))))
  
