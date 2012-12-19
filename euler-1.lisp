(defun sum1(a b d)
  (do ((n a (+ n d))
       (sum 0 (+ sum n)))
      ((> n b) sum)))

(defun sum35(a b)
  (do ((n a (1+ n))
       (sum 0 (+ sum (cond ((eq 0 (mod n 3)) n)
			   ((eq 0 (mod n 5)) n)
			   (t 0)))))
      ((> n b) sum)))

(defun euler-1()
  (progn
    (print (sum35 1 999))
    (print (- (+ (sum1 0 999 3) (sum1 0 999 5)) (sum1 0 999 15)))
    (print (- (+ (* 3 333 (/ (+ 1 333) 2))
		 (* 5 199 (/ (+ 1 199) 2)))
	      (* 15 66 (/ (+ 1 66) 2))))
    (print (apply #'+ (loop for n from 1 below 1000 when (or (eq 0 (mod n 3)) (eq 0 (mod n 5))) collect n)))))