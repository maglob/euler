;; Find the smallest cube for which exactly five permutations of its digits are cube.

(load "~/Dropbox/prg/euler/toolkit.lisp")

(defun cube-root (n)
  (expt n 1/3))

(defun cube-p (n)
  (= n (expt (floor (cube-root n)) 3)))
  
(defun fingerprint (n)
  (sort (histogram (to-digits n)) (lambda (a b) (< (car a) (car b)))))

(defun euler-62 (&optional (n 5) (limit 9000) &aux (ht (make-hash-table :test 'equal)))
  (mapcar (lambda (x) (incf (gethash (fingerprint (expt x 3)) ht 0))) 
	  (range limit))
  (let ((needle (caar (remove-if-not (lambda (x) (= (cadr x) n)) 
			       (hash-table-list ht)))))
    (expt (car (remove-if-not (lambda (x) (equal needle (fingerprint (expt x 3)))) 
			      (range limit)))
	  3)))
