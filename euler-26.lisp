;;;; Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.

(defun invcycle(x)
  (if (< x 2)
      nil
      (do ((n 10 (* n 10))
	   (ht (make-hash-table)))
	  ((> (log n 10) 1000) (list nil n))
	(if (not (zerop (/ n x)))
	    (if (zerop (mod n x))
		(return-from invcycle nil)
		(let ((m (second (multiple-value-list (floor (/ n x))))))
		  (if (gethash m ht)
		      (return-from invcycle 
			(list (floor (/ n x)) (log (gethash m ht) 10) x))
		      (setf (gethash m ht) n))))))))

(defun euler-26(&optional (limit 1000))
  (let ((lst nil))
    (dotimes (i limit lst)
      (let ((n (invcycle (1+ i))))
	(if n
	    (setq lst (cons n lst)))))
    (car (sort lst #'(lambda(a b)(> (car a) (car b)))))
    ))

