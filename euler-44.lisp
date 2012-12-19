;; euler 44: Find the pair of pentagonal numbers, Pj and Pk, for which their sum and difference is pentagonal and D = |Pk  Pj| is minimised; what is the value of D?


(defun pentagon-number (n)
  "Pn =n(3n - 1)/ 2"
  (/ (* n 
	(1- (* 3 n)))
     2))

(defun solve (a b c &aux (d (- (* b b)
			       (* 4.0d0 a c))))
  "solve max root for 2nd degree equation"
  (if (and (/= a 0) (>= d 0))
      (let ((sd (sqrt d)))
	(values (/ (+ (- b) sd)
		   (+ a a))
		(/ (- (- b) sd)
		   (+ a a))))))
		      
      

(defun pentagon-number? (x)
  "3n^2 - n - 2x = 0"
  (dolist (n (multiple-value-list (solve 3 -1 (* x -2))))
    (if (and (> n 0)
	     (= n (floor n)))
	(return t))))

;; nex-pen += 3n - 2
(defun euler-44(&aux (d most-positive-fixnum))
  (do* ((high 1 (1+ high))
	(high-pen (pentagon-number high) (pentagon-number high)))
       ((> (- high-pen (pentagon-number (1- high))) d) d)
       ;((> high 10000000) d)
    (do* ((i (1- high) (1- i))
	  (i-pen (pentagon-number i) (pentagon-number i)))
	 ((or (< d (- high-pen i-pen))
	       (< i 1)))
      (if (and (pentagon-number? (- high-pen i-pen))
	       (pentagon-number? (+ high-pen i-pen)))
	  (progn
	    (setf d (min d (abs (- high-pen i-pen))))
	    (format t "~D ~D: ~D ~D, ~D~%" high i (+ high-pen i-pen) (- high-pen i-pen) d))))))