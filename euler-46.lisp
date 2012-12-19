;; euler 46: 

(load "/Users/marko/prg/lisp/toolkit.lisp")

(defun memoize (fn) 
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args) 
	(multiple-value-bind (val win) (gethash args cache)
	  (if win 
	      val
	      (setf (gethash args cache) (apply fn args)))))))

(defun prime-nth (n)
  "Returns nth prime"
  (do((i 1 (1+ i)))
     (nil)
    (if (and (prime? i)
	     (zerop (decf n)))
	    (return i))))

(defun twice-square (n)
  (* 2 (* n n)))

(defun goldbach? (n)
  (do* ((i 1 (1+ i))
	(ts 2 (twice-square i)))
       ((>= ts n) nil)
    (if (prime? (- n ts))
	(return t))))

(defun euler-46 ()
  (do ((i 3 (+ i 2)))
      (nil)
    (if (and (not (prime? i))
	     (not (goldbach? i)))
	(return i))))


