;; For the first one hundred natural numbers, find the total of the digital sums of the first one hundred decimal digits for all the irrational square roots.

(defun calc-x (p c)
  (dotimes (x 11)
    (if (> (* (+ (* 20 p) x) x) c)
	(return (1- x)))))
  
(defun sqrt-digits (n &optional (limit 100))
  (let ((res 0)
	(c n))
    (dotimes (i limit res)
      (let* ((p res)
	     (x (calc-x p c))
	     (y (* (+ (* 20 p) x) x)))		      
	(setf res (+ (* res 10) x))
	(setf c (- c y))
	(if (zerop c)
	    (return res)
	    (setf c (* 100 c)))))))

(defun euler-80 (&optional (limit 99))
  (apply #'+ (mapcar #'(lambda (x) (apply #'+ x)) 
		     (remove-if #'(lambda (x) (= 1 (length x))) 
				(mapcar #'to-digits (mapcar #'sqrt-digits (range limit)))))))