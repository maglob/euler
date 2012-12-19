;; p(n)=number of ways to n coins can be separated into piles. Find the least value of n for which p(n) is divisible by one million.

(defvar cache)

(defun gen-pentagonal-number (n)
  "Generic pentagonal number"
  (let ((m (if (zerop (mod n 2))
	       (- (/ n 2))
	       (ceiling (/ n 2)))))
    (/ (* m (1- (* 3 m))) 2)))
      
(defun partition-count-euler (n)
  "See http://en.wikipedia.org/wiki/Partition_(number_theory)#Generating_function"
  (if (gethash n cache)
      (gethash n cache)
      (if (zerop n)
	  (setf (gethash n cache) 1)
	  (let ((sum 0))
	    (do* ((i 1 (1+ i))
		  (pn (gen-pentagonal-number i) (gen-pentagonal-number i)))
		 ((> pn n) (setf (gethash n cache) sum))
	      (if (zerop (mod (floor (/ (1- i) 2)) 2))
		  (incf sum (partition-count-euler (- n pn)))
		  (decf sum (partition-count-euler (- n pn)))))))))
	
(defun euler-78 (&optional (limit 60000))
  (setf cache (make-hash-table))
  (do ((i 9 (+ i 5)))
      ((> i limit) )
    (if (zerop (mod (partition-count-euler i) 1000000))
      (return i))))

  