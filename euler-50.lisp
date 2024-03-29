;; euler 50: Which prime, below one-million, can be written as the sum of the most consecutive primes?

(load "/Users/marko/prg/lisp/toolkit.lisp")

(defun sum-up-to-list (lst &aux (sum 0) res)
  (dolist (s lst (reverse res))
    (setf res (cons (incf sum s) res))))

(defun range-sum (array a b)
  (- (aref array b) 
     (if (> a 0)
	 (aref array (1- a))
	 0)))

(defun euler-50 (&optional (limit 1000000) &aux (res '(0 . 0)))
  (declare ((unsigned-byte 32) limit)
	   (optimize (speed 3) (safety 0)))
  (let* ((primes (remove-if-not #'prime? (range limit)))
	 (sa (coerce (sum-up-to-list primes) 'vector)))
    (declare (type (simple-array fixnum) sa))
    (do ((i 0 (1+ i)))
	((>= (aref sa i) limit) res)
      (declare ((unsigned-byte 32) i))
      (do ((j i (1+ j)))
	  ((> (the fixnum (range-sum sa i j)) limit) )
	(declare ((unsigned-byte 32)j))
	(let ((rs (range-sum sa i j))
	      (n (1+ (- j i))))
	  (declare ((unsigned-byte 32) rs n))
	  (if (and (prime? rs)
		   (> n (the fixnum (cdr res))))
	      (setf res (cons rs n))))))))
