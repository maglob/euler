;; What is the side length of the square spiral for which the ratio of primes along both diagonals first falls below 10%?

(defun primep (n)
  (cond ((< n 2) nil)
	((= n 2) t)
	((zerop (mod n 2)) nil)
	(t (do ((i 3 (+ i 2))
		(end (1+ (floor (sqrt n)))))
	       ((>= i end) t)
	     (if (zerop (mod n i))
		 (return nil))))))

(defun range (n)
  (labels ((range-helper (nn)
	     (if (zerop nn) 
		 nil
		 (cons nn (range-helper (1- nn))))))
    (nreverse (range-helper n))))

(defun diagonal-number (n)
  (let ((res 0))
    (dotimes (i (1- n) (1+ res))
      (let ((d (* (+ 1 (floor (/ i 4))) 2)))
	(incf res d)))))

(defun diagonal-number-iterator ()
  (let ((res 0)
	(i 0))
    (lambda () 
      (incf res (* (1+ (floor (/ i 4))) 2))
      (incf i)
      (1+ res))))
	    
(defun diagonal-prime-ratio (r)
  (let* ((n (1+ (* (1- r) 4)))
	 (dn (mapcar #'diagonal-number (range n))))
    (/ (length (remove-if-not #'primep dn))
       (length dn))))

(defun euler-58 (&optional (end 100000))
  (do ((i 1 (1+ i))
       (prime-cnt 0)
       (itr (diagonal-number-iterator)))
      ((and (> i 2) 
	    (< (/ prime-cnt i) 0.1)) 
       (if (<= i end) 
	   (1+ (ceiling (/ i 2)))))
    (if (primep (funcall itr))
	(incf prime-cnt))))