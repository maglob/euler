;;;; Which starting number, under one million, produces the longest chain?
;;;; > (euler-14)
;;;; 837799

(defun seq(x)
  (if (evenp x)
      (/ x 2)
      (1+ (* 3 x))))

(defun seqlen-a(x a)
  (if (= x 1)
      1
      (if (> (aref a x) -1)
	  (aref a x)
	  (setf (aref a x)
		(1+ (seqlen (seq x) a))))))

(defun seqlen(x ht)
  (if (= x 1)
      1
      (if (gethash x ht)
	  (gethash x ht)
	  (setf (gethash x ht)
		(1+ (seqlen (seq x) ht))))))


(defun seqlen-simple(x)
  (do ((i 0 (1+ i))
       (n x (seq n)))
      ((= n 1) (1+ i))))

(defun euler-14()
  (let ((m 1)
	(ht (make-hash-table)))
    (dotimes (i 1000000 m)
      (if (> (seqlen (1+ i) ht) (seqlen m ht))
	  (setf m (1+ i))))))

(defun euler-14-a()
  (let ((m 1)
	(a (make-array 32000000 :element-type 'fixnum :initial-element -1)))
    (dotimes (i 1000000 m)
      (if (> (seqlen (1+ i) a) (seqlen m a))
	  (setf m (1+ i))))))
