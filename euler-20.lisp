;;;; Find the sum of the digits in the number 100!

(defun factorial(x)
  (if (= x 1)
      1
      (* x (factorial (1- x)))))

(defun digitsum(x)
  (reduce #'+ (map 'vector 
		   #'digit-char-p 
		   (write-to-string x))))

(defun euler-20()
  (digitsum (factorial 100)))