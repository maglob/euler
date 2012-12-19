;; euler 51: Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit, is part of an eight prime value family.

(load "/Users/marko/prg/lisp/toolkit.lisp")

(defun memoize (fn) 
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args) 
        (multiple-value-bind (val win) (gethash args cache)
          (if win 
              val
              (setf (gethash args cache) (apply fn args)))))))

(defun ndigits (n)
  (1+ (truncate (log n 10))))

(defun setdigit (n d v)
  "Set d'th digit of n to v"
  (let* ((m (expt 10 d))
	 (m10 (* m 10)))
    (+ (* (truncate (/ n m10)) m10)
       (* v m)
       (mod n m))))
	
	 
(defun gen-number-family (n mask &aux res)
  "Generae number family"
  (let ((nd (ndigits n)))
    (do ((d 9 (1- d))
	 (nv n n))
	((< d 0) res)
      (dotimes (i nd)
	(if (logbitp i mask)
	    (setf nv (setdigit nv i d))))
      (if (and (prime? nv)
	       (= (ndigits nv) nd))
	  (setf res (cons nv res))))))

(defun number-family-size (n mask &aux (res 0))
  "Size of number family"
  (let ((nd (ndigits n)))
    (do ((d 9 (1- d))
	 (nv n n))
	((< d 0) res)
      (dotimes (i nd)
	(if (logbitp i mask)
	    (setf nv (setdigit nv i d))))
      (if (and (funcall mz-prime? nv)
	       (or (/= d 0)
		   (= (ndigits nv) nd)))
	  (incf res)))))

(defun euler-51-b (&optional (limit 100) &aux (n 0) res)
  (do ((i 3 (+ 2 i)))
      ((> i limit) res)
    (if (prime? i)
	(do ((mask (1- (expt 2 (ndigits i))) (1- mask)))
	    ((<= mask 0) )
	  (let ((lst (gen-number-family i mask)))
	    (if (> (length lst) n)
		(progn (setf res lst)
		       (setf n (length lst))
		       (if (= n 8)
			   (return lst)))))))))


(defparameter mz-prime? (memoize #'prime?))

(defun euler-51 (&optional (limit 1000000))
  (do ((i 3 (+ 2 i)))
      ((> i limit) )
    (if (funcall mz-prime? i)
	(do ((mask (- (expt 2 (ndigits i)) 2) (1- mask)))
	    ((<= mask 0) )
	  (let ((size (number-family-size i mask)))
	    (if (= size 8)
		(return-from euler-51 (gen-number-family i mask))))))))


