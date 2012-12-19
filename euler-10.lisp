;;;; Find the sum of all the primes below one million.

(defun primep(n)
  (let ((end (floor (sqrt n))))
    (cond ((= n 1) nil)
	  ((= n 2) t)
	  ((= 0 (mod n 2)) nil)
	  (t (do ((i 3 (+ i 2)))
	      ((>  i end) t)
	    (if (= 0 (mod n i))
		(return-from primep nil)))))))

(defun range(n)
  (do ((i n (1- i))
       (lst nil (cons i lst)))
       ((<= i 0) lst)))

(defun euler-10()
  (apply #'+ (remove-if-not #'primep (range 1000000))))

(defun euler-10b()
  (let ((s 0))
    (dotimes (i 1000000 s) (if (primep i) (setf s (+ s i))))))