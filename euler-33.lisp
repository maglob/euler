;;;; The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.

(load "/Users/marko/prg/lisp/toolkit.lisp")

(defun find-duplicate(list)
  (dolist (n list)
    (if (> (count n list) 1)
	(return-from find-duplicate n))))

(defun curious-fraction?(a b &aux (al (to-digits a)) (bl (to-digits b)))
  (let* ((n (find-duplicate (append al bl)))
	 (al2 (remove n al))
	 (bl2 (remove n bl)))
    (and n (/= n 0) al2 bl2 (/= (to-number bl2) 0)
	 (= (/ a b)
	    (/ (to-number al2)
	       (to-number bl2))))))


(defun euler-33(&aux (prod 1))
  (do ((i 10 (1+ i)))
      ((> i 99) (denominator prod))
    (do ((j 10 (1+ j)))
	 ((>= j i) t)
      (if (curious-fraction? j i)
	  (setf prod (* prod (/ j i)))))))
;	  (format t "found: ~A / ~A~%" j i)))))