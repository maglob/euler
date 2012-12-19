;;;; How many different ways can £2 be made using any number of coins?


(defun npart(x list)
  (cond ((= x 0) 1) 
	((or (< x 0) (null list)) 0)
	(t (do ((n (pop list))
		(x x (- x n))
		(sum 0 (+ sum (npart x list))))
	       ((< x 0) sum)))))


(defun euler-31()
  (npart 200 '(200 100 50 20 10 5 2 1)))