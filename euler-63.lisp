;; How many n-digit positive integers exist which are also an nth power?

(defun euler-63 (&aux res)
  (do ((i 1 (1+ i)))
      ((< (ndigits (expt 9 i)) i) (values (length res) res))
    (dotimes (j 10)
      (if (= (ndigits (expt (1+ j) i)) i)
	  (push (expt (1+ j) i) res)))))