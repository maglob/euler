;; euler 49: What 12-digit number do you form by concatenating the three terms in this sequence?

(load "/Users/marko/prg/lisp/toolkit.lisp")

(defun digit-list (n)
  (if (> n 0)
      (cons (mod n 10) (digit-list (floor (/ n 10))))))

(defun euler-49 ()
  (do ((i 1488 (1+ i)))
      ((> i 9999) )
    (let* ((i2 (+ i 3330))
	   (i3 (+ i 6660))
	   (l (sort (digit-list i) #'<))
	   (l2 (sort (digit-list i2) #'<))
	   (l3 (sort (digit-list i3) #'<)))
      (if (and (prime? i)
	       (prime? i2)
	       (prime? i3)
	       (equal l l2)
	       (equal l l3))
	  (return (+ (* i (expt 10 8))
		     (* i2 (expt 10 4))
		     i3))))))
			