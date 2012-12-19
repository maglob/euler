;; How many chains, with a starting number below one million, contain exactly sixty non-repeating terms?

(load "~/Dropbox/prg/euler/toolkit.lisp")

(defun digit-factorial (n)
  (apply #'+ (mapcar #'fract (to-digits n))))

(defun digi-fact-cycle-length (n &optional (ht (make-hash-table)))
  (let ((nht (gethash n ht)))
    (if nht
	nht
	(let ((df (digit-factorial n)))
	  (setf (gethash n ht) 0)
	  (setf (gethash n ht) (1+ (digi-fact-cycle-length df ht)))))))


(defun euler-74 (&optional (limit 999999))
  (let ((ht (make-hash-table)))
    (count 60
	   (mapcar (lambda (x) (digi-fact-cycle-length x ht)) (range limit)))))
