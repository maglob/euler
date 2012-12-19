;;;; Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

(defun divisors(x &optional (d (floor (sqrt x))))
  (cond
         ((= 1 d) (list 1))
         ((zerop (mod x d)) (append (remove-duplicates (list d (/ x d)))
                                    (divisors x (1- d))))
         (t (divisors x (1- d)))))


(defun abundantp(x)
  (and (> x 0)
       (> (apply #'+ (divisors x)) x)))

(defun abundant-list(x)
  (do ((i 1 (1+ i))
       (lst))
      ((> i x) (reverse lst))
    (if (abundantp i)
	(setq lst (cons i lst)))))

(defun filla(a lst x)
  (dolist (i lst a)
    (if (array-in-bounds-p a (+ x i))
	(setf (aref a (+ x i)) 0)
	(return-from filla))))
	

(defun euler-23(&optional (limit 28184))
  (let* ((n limit)
	 (a (make-array n :element-type 'fixnum))
	 (lst (abundant-list n)))
    (dotimes (i (length a)) 
      (setf (aref a i) i))
    (do ()
	((> (car lst) (/ n 2)))
      (filla a lst (car lst))
      (setf lst (cdr lst)))
    (reduce #'+ a)))

  

    