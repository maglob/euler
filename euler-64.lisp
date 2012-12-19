;; How many continued fractions for N<=10000 have an odd period?

(load "~/Dropbox/prg/euler/toolkit.lisp")

(defun semi-equal (a b)
  "compares list for equality and stop if b stops"
  (if (or (null a) (null b))
      t
      (and (= (car a) (car b))
	   (semi-equal (cdr a) (cdr b)))))

(defun cycle (list)
  "Find cycle from begining the list"
  (do ((l (cdr list) (cdr l))
       (minlen (floor (/ (length list) 2)))
       (n 1 (1+ n)))
      ((<= (length l) minlen) nil)
    (if (semi-equal list l)
	(return (subseq list 0 n)))))

(defun step-1 (collect a b c d)
  (if (funcall collect a)
      (step-2 collect 
	      b
	      c 
	      (- d) 
	      (- c (* d d)))))

(defun step-2 (collect a b c d)
  (let ((na (floor (/ (* (+ (isqrt b) c) a) d)))
	(nd (/ d a)))
    (step-3 collect 
	    na
	    b
	    (- c (* nd na))
	    nd)))
	  

(defun step-3 (collect a b c d)
  (step-1 collect a d b c))

(defun cont-fract-cycle (n &optional (limit 20) &aux res (len 0))
  (if (/= (sqrt n) (isqrt n))
      (labels ((col (x)
		 (push x res)
		 (< (incf len) limit)))
	(step-1 #'col
		(isqrt n) 
		1
		n
		(- (isqrt n)))
	(cycle (cdr (nreverse res))))))
	    

	
(defun euler-64 ()
  (count-if #'oddp 
	    (mapcar (lambda (x) (length (cont-fract-cycle x 500))) 
		    (range 10000))))
