;;;; How many circular primes are there below one million?

(load "/Users/marko/prg/lisp/toolkit.lisp")

(defun rotate-list(list)
  (let ((a (pop list)))
    (append list (list a))))

(defun rotate(n)
  (if (>= n 10)
      (let ((scale (expt 10 (floor (/ (log n) (log 10))))))
	(+ (* (mod n scale) 10) (floor (/ n scale))))
      n))

(defun circular-prime?(n)
  (if (prime? n)
      (let* ((list (to-digits n))
	     (len (length list)))
	(dotimes (i len t)
	  (if (not (prime? (to-number (setf list (rotate-list list)))))
	      (return-from circular-prime? nil))))))

(defun euler-35(&aux (count 0))
  (do ((i 0 (1+ i)))
      ((> i 999999) count)
    (if (circular-prime? i)
	(incf count))))