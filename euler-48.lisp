;; euler 48: Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

(defun euler-48 (&optional (limit 1000) &aux (res 0) (mask (expt 10 10)))
  (dotimes (i limit res)
    (setf res (mod (+ res
		      (mod (expt (1+ i) (1+ i)) mask))
		   mask))))
  