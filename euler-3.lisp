;;;; What is the largest prime factor of the number 317584931803? 

(defun primep(n)
  (let ((end (floor (sqrt n))))
    (cond ((= n 1) t)
	  ((= 0 (mod n 2)) nil)
	  (t (do ((i 3 (+ i 2)))
	      ((>  i end) t)
	    (if (= 0 (mod n i))
		(return-from primep nil)))))))

(defun max-prime-factor(n)
  (do ((i (floor (sqrt n)) (1- i)))
      ((<= i 3) i)
    (if (and (= 0 (mod n i))
	     (primep i))
	(return-from max-prime-factor i))))

(defun euler-3()
  (max-prime-factor 317584931803))