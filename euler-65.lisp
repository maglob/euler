;; Find the sum of digits in the numerator of the 100th convergent of the continued fraction for e

(load "~/Dropbox/prg/euler/toolkit.lisp")

(defun e-cont-fract (n)
  "Return nth continous fractional of e"
  (case (mod n 3)
    ((1 0) 1)
    ((2) (* 2 (1+ (floor (/ n 3)))))))

(defun e-convergent (n &optional res)
  "Return nth convergent of continued fraction of e"
  (if (zerop n)
      (if res
	  (+ 2 (/ 1 res))
	  2)
      (e-convergent (1- n) 
		    (+ (e-cont-fract n) 
		       (if res 
			   (/ 1 res)
			   0)))))


(defun euler-65 (&optional (n 100))
  (apply #'+ (to-digits (numerator (car (nreverse (mapcar #'e-convergent 
							  (range (1- n)))))))))