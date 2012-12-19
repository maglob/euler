;; euler 35: Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

(defun palindromep(s &optional (start 0) (end (1- (length s))) )
  (if (> start end)
      t
      (and (eql (char s start) (char s end))
	   (palindromep s (1+ start) (1- end)))))

(defun euler-36 (&optional (limit 1000000) &aux (sum 0)) 
  (dotimes (n limit sum)
    (if (and (palindromep (format nil "~D" n))
	     (palindromep (format nil "~B" n)))
	;(format t "~d~%" n))))
	(setf sum (+ sum n)))))