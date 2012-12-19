;; Consider quadratic Diophantine equations of the form: x2 – Dy2 = 1. 
;; Find the value of D  1000 in minimal solutions of x for which the largest value of x is obtained.

(load "~/Dropbox/prg/euler/toolkit.lisp")

(defun solution-p (x y d)
  "Predicate for solution for equation: x**2 - d*y**2 = 1"
  (= (- (* x x) (* d y y)) 1))

(defun semi-equal (a b)
  "compares list for equality and stop when shorter list ends"
  (if (or (null a) (null b))
      t
      (and (= (car a) (car b))
	   (semi-equal (cdr a) (cdr b)))))

(defun cycle (list)
  "Find cycle from begining of the list"
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

(defun cont-fract-cycle (n &optional (limit 500) &aux res (len 0))
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

(defun convergent (fracts n)
  (if (zerop n)
      0
      (/ (+ (car fracts) (convergent (cdr fracts) (1- n))))))

(defun find-solution (d &optional (limit 100))
  (if (/= (expt (isqrt d) 2) d)
      (let ((fracts (circular (cont-fract-cycle d))))
	(dotimes (n limit)
	  (let* ((cand  (+ (isqrt d) 
			  (convergent fracts (1+ n))))
		 (x (numerator cand))
		 (y (denominator cand)))
	    (if (solution-p x y d)
		(return-from find-solution (list x y d))))))))

(defun euler-66 ()
  (last (car (sort (remove-if #'null 
			      (mapcar #'find-solution (range 1000))) 
		   (lambda (a b) (> (car a) (car b)))))))