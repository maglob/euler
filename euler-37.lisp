;; euler 37: Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

(load "/Users/marko/prg/lisp/toolkit.lisp")

(defun truncatable-prime-rl? (n)
  (if (< n 10)
      (prime? n)
      (and (prime? n)
	   (truncatable-prime-rl? (floor (/ n 10))))))

(defun truncatable-prime-lr? (n)
  (if (< n 10)
      (prime? n)
      (and (prime? n)
	   (truncatable-prime-lr? (mod n (expt 10 (floor (log n 10))))))))

(defun euler-37 (&optional (limit 11))
  (do ((sum 0)
       (i 10 (1+ i)))
      ((<= limit 0) sum)
    (if (and (prime? i)
	     (truncatable-prime-rl? i)
	     (truncatable-prime-lr? i))
	(progn 
	  (incf sum i)
	  (decf limit)))))