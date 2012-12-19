;;;; How many routes are there through a 20×20 grid? 

(defun downp(x y n)
  (< y n))

(defun rightp(x y n)
  (< x n))

(defun npath(x y n sum)
  (cond ((and (= x n)
	      (= y n))
	 (1+ sum))
	((> y n) sum)
	((> x n) sum)
	(t (npath (1+ x) y n (npath x (1+ y) n sum)))))


(defun npath-2(x y n)
  (cond ((and (= x n)
	      (= y n))
	 1)
	((or (> y n)
	     (> x n))
	 0)	    
	(t (+ (npath-2 (1+ x) y n) (npath-2 x (1+ y) n)))))

(defun npath-3(x y n a)
  (cond ((and (= x n)
	      (= y n))
	 1)
	((or (> y n)
	     (> x n))
	 0)	    
	(t (if (aref a y x)
	       (aref a y x)
	       (setf (aref a y x)
		     (+ (npath-3 (1+ x) y n a) (npath-3 x (1+ y) n a)))))))

(defun euler-15()
  (npath-3 0 0 20 
	    (make-array '(21 21) :initial-element nil)))
