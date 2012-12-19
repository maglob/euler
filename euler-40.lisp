;; euler 40: if dn represents the nth digit of the fractional part, find the value of the following expression: d1 * d10 * d100 * d1000 * d10000 * d100000 * d1000000

(defun dn (n)
  (do* ((i 0 (1+ i))
	(j 1 (+ j (1+ (floor (log i 10))))))
;       ((> j n) (values n j i
       ((> j n) (- (char-code (char (format nil "~D" i)
			(- (1+ (floor (log i 10)))
			   (- j n))))
		   (char-code #\0)))))

(defun euler-40 ()
  (apply #'*
	 (mapcar #'dn 
		 '(1 10 100 1000 10000 100000 1000000))))


