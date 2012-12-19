;;;; Which is the first triangle number to have over five-hundred divisors? 

;; > (nfact (* 2 2 2 2 3 3 3 5 5 5 5 5 7 7 7))
;; 480
;; > (+ (* 4 3 5 3) (* 4 3 5) (* 4 5 3) (* 4 3 3) (* 3 5 3) (* 4 3) (* 4 5) (* 4 3) (* 3 5) (* 3 3) (* 5 3) 4 3 5 3)
;; 479
;;
;; > (nfact-2 76576500)
;; 576

(defun primep(n)
  (let ((end (floor (sqrt n))))
    (cond ((= n 1) nil)
          ((= n 2) t)
          ((= 0 (mod n 2)) nil)
          (t (do ((i 3 (+ i 2)))
              ((>  i end) t)
            (if (= 0 (mod n i))
                (return-from primep nil)))))))


(defun nfact(x)
  (let ((m (floor (/ x 2))))
  (do ((i 1 (1+ i))
       (n 0))
      ((> i m) (+ n 1))
    (if (zerop (mod x i)) 
	(setf n (1+ n))))))

(defun nfact-2(x)
  (1+ (length
       (remove-duplicates (comball 
			   (prime-factors x)) :test #'equal))))

(defun nfact-3(x)
  (let ((res 1))
    (dolist (j 
	      (comball (let ((r nil)
			     (l (prime-factors x)))
			 (dolist (i (remove-duplicates l) r) 
			   (setf r (cons (count i l) r)))))
	     res)
      (incf res (apply #'* j)))))

(defun multiples(lst)
  (if (= 1 (length lst))
      lst))
      

(defun next-prime(n)
  (if (< n 2)
      2
      (if (= (mod n 2) 1)
	  (next-prime (1+ n))
	  (do ((n (+ n 1) (+ n 2)))
	      (nil)
	    (let ((end (floor (sqrt n))))
	      (do ((i 3 (+ i 2)))
		  ((> i end) 
		   (return-from next-prime n))
		(if (zerop (mod n i))
		    (return ))))))))

(defun prime-factors(n)
  (if (= n 1)
      nil
      (if (primep n)
	  (list n)
	  (do ((i 2 (next-prime i)))
	      ((zerop (mod n i)) (cons i (prime-factors (/ n i))))))))

(defun ncomb(n k)
  (let ((r 1))
    (dotimes (i k r)
      (setf r (* r (/ (- n i) (1+ i)))))))

(defun comb(lst n)
  (cond ((or (null lst)
	     (< n 1)) nil)
	((= n 1) (let ((r nil))
		   (dolist (i lst r)
		     (setf r (cons (list i) r)))))
	(t (let ((r nil))
	     (dotimes (i (1- (length lst)) r)
	       (dolist (l (comb (nthcdr (1+ i) lst) (1- n)) r)
		 (setf r (cons 
			  (cons (nth i lst) l) r))))))))


(defun comball(lst)
  (let ((r nil))
    (dotimes (i (length lst) r)
      (setf r (append 
	       (comb lst (1+ i)) 
	       r)))))
      
(defun euler-12()
  (do* ((i 1 (1+ i))
	(n i (+ n i)))
	((> (nfact-3 n) 500) n)))
;       (format t "~A ~A ~%" n (nfact-2 n))))
