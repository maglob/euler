;; euler 38: What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n  1?

;; 9-digit number using digits 1..9
(defun pandigital? (n &aux (bitmask 0))
  (dotimes (i 9 (= bitmask #b1111111110))
    (setf bitmask (boole boole-ior bitmask (ash 1 (mod n 10))))
    (setf n (floor (/ n 10)))))

;; catenation of n * (1, 2, ... range)
(defun concat-product (n range &aux (res 0) p)
  (dotimes (i range res)
    (setf p (* n (1+ i)))
    (setf res (+ (* (expt 10 (ceiling (log p 10)))
		    res)
		 p))))

(defun euler-38 (&aux (max 0))
  (do ((range 2 (1+ range)))
      ((> range 9) max)
    (do* ((n 1 (1+ n))
	  (p (concat-product n range) (concat-product n range)))
	 ((> p 987654321))
      (if (pandigital? p)
	  (setf max (max max p))))))
