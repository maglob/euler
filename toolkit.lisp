(defun permutate(list &optional fn-p &aux result)
  (if list
      (dolist (n list result)
	(dolist (p (permutate (remove n list)))
	  (if fn-p
	      (let ((e (cons n p)))  
		(if (funcall fn-p e)
		    (push e result)))
	      (push (cons n p) result))))
      (list ()) ))

(defun ncomb(n k)
  (let ((r 1))
    (dotimes (i k r)
      (setf r (* r (/ (- n i) (1+ i)))))))

(defun comb(lst n)
  (cond ((or (null lst)
             (< n 1)) nil)
        ((= n 1) (let ((r nil))
                   (dolist (i lst r)
                     (setf r (cons (list i) r)))))
        (t (let ((r nil))
             (dotimes (i (1- (length lst)) r)
               (dolist (l (comb (nthcdr (1+ i) lst) (1- n)) r)
                 (setf r (cons 
                          (cons (nth i lst) l) r))))))))

(defun to-number(list)
  (reduce (lambda(x y)(+ (* x 10) y)) 
	  list))

(defun to-digits(n &aux result)
  (do ((n n (floor (/ n 10))))
      ((= n 0) result)
    (push (mod n 10) result)))

(defun fract(n)
  (if (<= n 1)
      1
      (* n (fract (1- n)))))

(defun primep(n &aux (limit (1+ (floor (sqrt n)))))
  (cond ((= n 2) t)
        ((or (< n 2) (zerop (mod n 2))) nil)
        (t (do ((i 3 (+ i 2)))
               ((> i limit) t)
             (if (zerop (mod n i))
                 (return nil))))))

(defun range(n &optional end)
  (if (null end)
      (range 1 n)
      (do ((i end (1- i))
	   (lst nil (cons i lst)))
	  ((< i n) lst))))

(defun split (str delim &aux res buf)
  "Split string on delim chars, and return a list of substrings"
  (dolist (s (coerce str 'list) (nreverse (push (coerce (nreverse buf) 'string) res)))
    (if (char= s delim)
	(progn (push (coerce (nreverse buf) 'string) res)
	       (setf buf nil))
	(push s buf))))

(defun histogram (list)
  "Creates a histogram from values in list. Returns a unordered list of poirs (value count). For example (1 5 1 2 5 1) -> ((1 3) (2 1) (5 2))"
  (let ((ht (make-hash-table))
        (res))
    (dolist (s list)
      (incf (gethash s ht 0)))
    (maphash (lambda (key value) (push (list key value) res)) ht)
    res))

(defun hash-table-list (ht &aux res)
  "Converts hash table into list list ((k1 v1) (k2 v2) ... (kn vn))"
  (maphash (lambda (k v) (push (list k v) res)) ht)
  res)

(defun ndigits (n)
  "Number of digits in integer n"
  (1+ (floor (log n 10))))

(defun circular (list)
  (let ((l (copy-list list)))
    (setf (cdr (last l)) l)))

(defun flatten (lst)
  "Flatten list"
  (if lst
      (if (atom lst)
	  (list lst)
	  (append (flatten (car lst)) 
		  (flatten (cdr lst))))))

(defun prime-factors (n)
  "List of prime factors of n"
  (if (> n 1)
      (let ((limit (isqrt n)))
	(do ((i 2 (1+ i)))
	    ((> i limit) (list n))
	  (if (zerop (mod n i))
	      (return-from prime-factors (cons i (prime-factors (/ n i)))))))))


(defun coprimep (a b)
  "a relatively prime to b?"
  (= (gcd a b) 1))

(defun head (lst &optional (len 10))
  (let ((res nil))
    (dotimes (i len)
      (if (car lst)
	  (push (car lst) res)
	  (return))
      (pop lst))
    (nreverse res)))