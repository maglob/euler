;; Find the smallest positive integer, x, such taht 2x, 3x, 4x, 5x and 6x, contain the same digits
;; for example 125874 * 2 = 251748


(defun digit-signature-4 (n &optional (res 0))
  (if (= n 0)
      res
      (multiple-value-bind (d m) (floor (/ n 10))
	(digit-signature d
			 (+ res (expt 8 (* m 10)))))))

(defun digit-signature (n &optional (res 0))
  (declare (optimize (speed 3) (safety 0))
	   (fixnum n res))
  (if (= n 0)
      res
      (multiple-value-bind (d m) (floor n 10)
	(digit-signature d
			 (the fixnum (+ res (expt 16 m)))))))
	 
(defun digit-signature-3 (n)
  (do ((res 0)
       (i n (floor (/ i 10))))
      ((= i 0) res)
    (incf res  (expt 8 (mod i 10)))))

(defun euler-52-b (&optional (limit 5))
  (dotimes (n limit)
    (let ((end (ceiling (/ (expt 10 (1+ n)) 6))))
      (do* ((i (+ 8 (expt 10 n)) (+ i 9))
	    (r (digit-signature i) (digit-signature i)))
	   ((> i end) )
	(if (and 
		 (= r (digit-signature (* 6 i)))
		 (= r (digit-signature (* 5 i)))
		 (= r (digit-signature (* 4 i)))
		 (= r (digit-signature (* 3 1)))
		 (= r (digit-signature (* 2 i))))
	    (return-from euler-52 i))))))

(defun euler-52 (&optional (limit 1000000))
  (do* ((i 3 (+ i 3))
	(r (digit-signature (* i 2)) (digit-signature (* i 2))))
       ((>= i limit) )
    (if (and 
	 (= r (digit-signature (* 6 i)))
	 (= r (digit-signature (* 5 i)))
	 (= r (digit-signature (* 4 i)))
	 (= r (digit-signature (* 3 1))))
					;(= r (digit-signature (* 2 i))))
	(return-from euler-52 i))))
	  

			 