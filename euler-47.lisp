;; euler 47: Find the first four consecutive integers to have four distinct primes factors. What is the first of these numbers?

(defun factors(n &aux (limit (isqrt n)))
  (if (> n 1)
      (do ((i 2 (1+ i)))
	  ((> i limit) (list n))
	(if (zerop (mod n i))
	    (return (cons i (factors (/ n i))))))))

(defun cnt-uniq-factors(n)
  (length (remove-duplicates (factors n))))

(defun euler-47 (&optional (cnt 4) &aux (n 0))
  (do ((i 2 (1+ i)))
      (nil)
    (if (= (cnt-uniq-factors i) cnt)
	(if (= (incf n) cnt)
	    (return (1+ (- i cnt))))
	(setf n 0))))