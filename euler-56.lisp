;; Considering natural numbers of the form, ab, where a, b  100, what is the maximum digital sum?

(defun digit-sum (n)
  (do ((sum 0)
       (n n (floor n 10)))
      ((zerop n) sum)
    (incf sum (mod n 10)))))

(defun euler-56 ()
  (let ((res 0))
    (dotimes (i 99 res)
      (dotimes (j 99)
	(setf res (max res (digit-sum (expt (1+ i) (1+ j)))))))))