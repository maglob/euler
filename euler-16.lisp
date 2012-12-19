;;;; What is the sum of the digits of the number 21000? 

(defun digitsum-1(x)
  (apply #'+ (map 'list #'(lambda (x) (- (char-code x) (char-code #\0))) 
		  (coerce (write-to-string x) 'list))))

(defun digitsum-2(x)
  (if (< x 1)
      0
      (+ (mod x 10) 
	 (digitsum-2 (floor (/ x 10))))))

(defun digitsum-3(x)
  (reduce #'+ (map 'vector 
		   #'digit-char-p 
		   (write-to-string x))))

(defun euler-16()
  (format t "~A ~A ~A ~%" 
	  (digitsum-1 (expt 2 1000)) 
	  (digitsum-2 (expt 2 1000))
	  (digitsum-3 (expt 2 1000))))