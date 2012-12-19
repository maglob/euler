;; In the first one-thousand expansions, how many fractions contain a numerator with more digits than denominator?

(defun my-sqrt-2 (n)
  (if (= n 1)
      (+ 1 (/ 1 2))
      (+ 1 (/ 1 (my-sqrt-2-helper (1- n))))))

(defun my-sqrt-2-helper (n)
  (if (= n 1)
      (+ 2 (/ 1 2))
      (+ 2 (/ 1 (my-sqrt-2-helper (1- n))))))

(defun euler-57 ()
  (let ((sum 0))
    (dotimes (i 1000 sum)
      (let ((f (my-sqrt-2 (1+ i))))
	(if (> (floor (log (numerator f) 10))
	       (floor (log (denominator f) 10)))
	    (incf sum))))))