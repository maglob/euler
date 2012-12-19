;; What is the first value which can be written as the sum of primes in over five thousand different ways?

(load "~/Dropbox/prg/euler/toolkit.lisp")

(defun sum-count (n &optional (d 1) (sum 0))
  "how many ways n can be written as sum of integers"
  ;(format t "~A: d: ~A, sum: ~A~%" n d sum)
  (cond ((> sum n) 0)
	((= sum n) 1)
	(t
	 (let ((count 0))
	   (do ((i d (1+ i)))
	       ((> i n) count)
	     (incf count (sum-count n i (+ sum i))))))))
      
(defun sum-count-prime (n &optional (d 2) (sum 0))
  "How many ways n can be written as sum of primes"
  (cond ((> sum n) 0)
	((= sum n) 1)
	(t
	 (let ((count 0))
	   (do ((i d (+ i 1)))
	       ((> i n) count)
	     (if (primep i)
		 (incf count (sum-count-prime n i (+ sum i)))))))))

(defun euler-77 (&optional (limit 5000))
  (do ((i 3 (1+ i)))
      (nil)
    (let ((n (sum-count-prime i)))
      (if (primep i)
	  (decf n))
      (if (> n limit)
	  (return i)))))
	 
  
  