;; What is the maximum 16-digit string for a "magic" 5-gon ring?

(load "~/Dropbox/prg/euler/toolkit.lisp")

(defun solution-p (strips)
  "Is this set of strips a solution for ngon?"
  (and (apply #'= (mapcar (lambda (x) (apply #'+ x)) strips))  ; Sum of all strips equal?
       (= (caar strips)
	  (reduce #'min (mapcar #'car strips)))))    ; First number is lowest "outer node"

(defun ngon-strips (lst)
  "Generete all triple-strips for N-gon"
  (let ((a (coerce lst 'vector))
	(n (/ (length lst) 2))
	(res))
    (dotimes (i n (nreverse res))
      (push (list (aref a (+ i))
		  (aref a (+ n i))
		  (aref a (+ n (mod (+ i 1) n))))
	    res))))
    
(defun single-digits (lst &aux res)
  (dolist (i lst (nreverse res))
    (if (< i 10)
	(push i res)
	(progn (push (floor (/ i 10)) res)
	       (push (mod i 10) res)))))


(defun euler-68-old (&optional (n 10) (len 16))
  (reduce #'max 
	  (mapcar #'to-number 
		  (remove-if-not (lambda (x) (= (length x) len)) 
				 (mapcar #'single-digits 
					 (mapcar #'flatten 
						 (mapcar #'ngon-strips 
							 (remove-if-not (lambda (x) (solution-p (ngon-strips x))) 
									(permutate (range n))))))))))
(defun euler-68 (&optional (n 10) (len 16))
  (reduce #'max 
	  (mapcar #'to-number 
		  (remove-if-not (lambda (x) (= (length x) len)) 
				 (mapcar (lambda (x) (single-digits (flatten (ngon-strips x))))
					 (permutate (range n) 
						    (lambda (x) (solution-p (ngon-strips x)))))))))