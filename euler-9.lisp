;;;; There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product abc.

(defun find-abc()
  (do ((a 1 (1+ a)))
      ((>= a 333) a)
    (do ((b a (1+ b)))
	((>= b 500) b)
      (let ((c (- 1000 b a)))
	(if (= (* c c) (+ (* a a) (* b b)))
	    (return-from find-abc (list a b c)))))))

(defun euler-9()
  (apply #'* (find-abc)))