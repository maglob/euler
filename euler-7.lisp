;;;; What is the 10001st prime number?

(defun range(n)
  (do ((i n (1- i))
       (lst nil (cons i lst)))
       ((<= i 0) lst)))

(defun primep(n)
  (cond ((null n) nil)
	((= n 1) nil)
	((= n 2) t)
	((= 0 (mod n 2)) nil)
	(t (let ((end (floor (sqrt n))))
	     (do ((i 3 (+ i 2)))
		 ((>  i end) t)
	       (if (= 0 (mod n i))
		   (return-from primep nil)))))))

(defun euler-7()
  (do ((i 0 (1+ i))
       (n 0 (if (primep i) 
		(1+ n) 
		n)))
      ((= n 10001) (1- i))))
