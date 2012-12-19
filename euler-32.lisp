;;;; Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.

(load "/Users/marko/prg/lisp/toolkit.lisp")

(defun partition(list a b)
  (list (subseq list 0 a)
	(subseq list a b)
	(subseq list b)))

(defun pandigital?(plist)
  (= (* (to-number (nth 0 plist)) 
	(to-number (nth 1 plist)))
     (to-number (nth 2 plist))))

(defun part-list(list &aux result)
  (do ((i 1 (1+ i)))
      ((> i 5) result)
    (do ((j (1+ i) (1+ j)))
	((> j 6))
      (push (partition list i j)
	  result))))

(defun euler-32(&aux 
		(sum 0) 
		(ht (make-hash-table)))
  (dolist (p (permutate '(1 2 3 4 5 6 7 8 9)) sum)
    (dolist (cand (part-list p))
      (if (pandigital? cand)
	  (let ((n (to-number (nth 2 cand))))
	    (if (not (gethash n ht))
		(progn
		  (setf (gethash n ht) n)
		  (incf sum n))))))))


  