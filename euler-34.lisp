;;;; Find the sum of all numbers which are equal to the sum of the factorial of their digits.

(load "/Users/marko/prg/lisp/toolkit.lisp")

(defun to-digits-fast(n result)
  (setf (fill-pointer result) 0)
  (do ((n n (floor (/ n 10))))
      ((= n 0) result)
    (vector-push (mod n 10) result)))
       
(defun euler-34(&aux (f (make-array 10)) (bound (* 7 (fract 9)))
		(buf (make-array 10 :fill-pointer 0)))
  (dotimes (i 10)
    (setf (elt f i) (fract i)))
  (do ((i 10 (1+ i))
       (sum 0))
      ((> i bound) sum)
    (if (= i
	   (reduce #'+ (to-digits-fast i buf) :key (lambda(x)(elt f x))))
	(progn
	  (format t "~a~%" i)
	  (setf sum (+ i sum))))))
