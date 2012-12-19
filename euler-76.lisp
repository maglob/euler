;; How many different ways can one hundred be written as a sum of at least two positive integers?

(defun partition-count (n)
  "Partitions for n, see http://en.wikipedia.org/wiki/Partition_function_(number_theory)#Intermediate_function"
  (let ((cache (make-hash-table :test 'equal)))
    (labels ((part (k n)
	       (let ((l (list k n)))
		 (cond ((gethash l cache) (gethash l cache))
		       ((> k n) (setf (gethash l cache) 0))
		       ((= k n) (setf (gethash l cache) 1))
		       (t (setf (gethash l cache) 
				(+ (part (1+ k) n)
				   (part k (- n k)))))))))
      (part 1 n))))


(defun euler-76 (&optional (n 100))
  (1- (partition-count n)))