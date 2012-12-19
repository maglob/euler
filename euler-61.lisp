;; Find the sum of the only ordered set of six cyclic 4-digit numbers for which each polygonal type: triangle, square, pentagonal, hexagonal, heptagonal, and octagonal, is represented by a different number in the set.

(load "~/Dropbox/prg/euler/toolkit.lisp")

(defun poly (p n)
  (case p 
    ((3) (/ (* n (1+ n)) 2))
    ((4) (* n n))
    ((5) (/ (* n (1- (* 3 n))) 2))
    ((6) (* n (1- (* 2 n))))
    ((7) (/ (* n (- (* 5 n) 3)) 2))
    ((8) (* n (- (* 3 n) 2)))))

(defun poly-list (p &aux res)
  "All 4 digis numbers from poly(p n)"
  (do* ((i 1 (1+ i))
	(m (poly p i) (poly p i)))
       ((> m 9999) res)
    (if (> m 999)
	(push m res))))

(defun path-list (n p &aux res)
  (dolist (e (poly-list p) res)
    (if (= (mod n 100)
	   (floor (/ e 100)))
	(push e res))))
    
(defun walk (map collect &optional (n 0) (start 0) (buf nil))
  (cond ((null map)
	 (if (= (mod n 100) 
		(floor (/ start 100)))
	     (funcall collect buf)))
	((= n 0) 
	 (dolist (e (poly-list (car map)))
	   (walk (cdr map) collect e e (list e))))

	(t
	 (dolist (e (path-list n (car map)))
	   (walk (cdr map) collect e start (cons e buf)))))
  nil)

(defun euler-61(&aux res)
  (mapcar (lambda (x) (walk x (lambda (y) (push y res)))) 
	  (permutate '(3 4 5 6 7 8)))
  (values 
   (apply #'+ (car res))
   (car res)))