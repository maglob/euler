;; Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.

; CL-USER> (time (potential-list 200000000 5))
; Evaluation took:
;   2949.807 seconds of real time
;   2949.525712 seconds of total run time (2946.719333 user, 2.806379 system)
;   [ Run times consist of 6.114 seconds GC time, and 2943.412 seconds non-GC time. ]
;   99.99% CPU
;   5,870,353,870,748 processor cycles
;   8,008,140,752 bytes consed
; ((7 (3 19 97 109)))


; CL-USER> (time (remove-if (lambda (x) (< (length (cadr x)) 4))  (clean-list (potential-list 500000000 5))))
; Evaluation took:
;  6726.180 seconds of real time
;   6725.295847 seconds of total run time (6715.176968 user, 10.118879 system)
;   [ Run times consist of 10.801 seconds GC time, and 6714.495 seconds non-GC time. ]
;   99.99% CPU
;   21,470,298,601,804 processor cycles
;   19,713,777,024 bytes consed
; ((1237 (3 7 19 37)))

;  (remove-if (lambda (x) (< (length (cadr x)) 4))  (clean-list (potential-list 13000000 5))))

; CL-USER> (time (remove-if (lambda (x) (< (length (cadr x)) 4))  (clean-list (potential-list  200000000 5))))
; Evaluation took:
;   1733.038 seconds of real time
;   1731.977745 seconds of total run time (1727.598714 user, 4.379031 system)
;   [ Run times consist of 7.794 seconds GC time, and 1724.184 seconds non-GC time. ]
;   99.94% CPU
;   5,531,786,276,452 processor cycles
;   8,011,528,976 bytes consed
; ((1237 (3 7 19 37)))

;;; (time (remove-if (lambda (x) (< (length (cadr x)) 4))  (clean-list (potential-list  10321937 5))))
;;; (remove-if (lambda (x) (< (length (cadr x)) 4)) (clean-list *))   ; repeat until stable
;;; (sort (mapcar (lambda (x) (list (+ (car x) (apply #'+ (cadr x))) x)) *foo*) (lambda (a b) (< (car a) (car b)))) ; result from  *foo*

;; CL-USER> (remove-if-not #'special-primeset-p (comb (mapcar #'car *foo*) 5))
;; ((823 541 283 7 3))
;; CL-USER> (apply #'+ (car *))
;; 1657

;; (remove-if-not #'special-primeset-p (reduce #'append (mapcar (lambda (x) (comb (cons (car x) (cadr x)) 5)) *foo*)))

;; ; (time (clean-list-length (potential-list 68000000 5) 5))
;; ; 372.110 seconds of real time
;; CL-USER> (time (clean-list-length (potential-list 128000000 5) 5))
;; Evaluation took:
;;   890.995 seconds of real time
;; CL-USER> (clean-list-length * 5)
;; ((8389 (13 5197 5701 6733)) (6733 (13 5197 5701 8389)) (5701 (13 5197 6733 8389)) (5197 (13 5701 6733 8389)) (13 (5197 5701 6733 8389)))
;; (setf *foo* *)
;; CL-USER> (remove-if-not #'special-primeset-p (reduce #'append (mapcar (lambda (x) (comb (cons (car x) (cadr x)) 5)) *foo*)))
;; CL-USER> (mapcar #'car *foo*)
;; (8389 6733 5701 5197 13)
;; CL-USER> (special-primeset-p (mapcar #'car *foo*))
;; T
;; CL-USER> (apply #'+ (mapcar #'car *foo*))
;; 26033

(load "~/Dropbox/prg/euler/toolkit.lisp")

(defun special-pair-p (a b)
  (and (primep (concat a b)) 
       (primep (concat b a))))

(defun clean-sublist (list n)
  (remove-if-not (lambda (x) (>= (length (prime-pairs-filter list x)) (- n 2))) 
		 list))

(defun prime-pairs-filter (list n)
  (remove-if-not (lambda (x) (and (primep (concat n x)) (primep (concat x n)))) 
		 (remove n list)))

(defun special-primeset-p (lst)
  (dolist (l lst t)
    (dolist (m lst)
      (if (and (/= m l)
	       (not (primep (concat l m))))
	  (return-from special-primeset-p (values nil (list l m)))))))

(defun memoize (fn)
  (let ((ht (make-hash-table)))
    (lambda (x)
      (if (nth-value 1 (gethash x ht))
	  (gethash x ht)
	  (setf (gethash x ht) (funcall fn x))))))

(defun ndigits (n)
  "Number of digits in integer n"
  (1+ (floor (log n 10))))

(defun clean-list-length (list n)
  (remove-if (lambda (x) (< (length (cadr x)) (1- n)))
	     (mapcar (lambda (x) (list (car x) (clean-sublist (cadr x) n))) 
		     (clean-list list))))

(defun clean-list (list)
  (let ((candidates (mapcar (lambda (x) (car x)) list)))
    (mapcar (lambda (x) (list (car x) (remove-if-not (lambda (y) (find y candidates)) (cadr x)))) 
	    list)))

(defun prime-split (n &optional (primep #'primep) &aux res)
  "Split integer n into two halves, eg. 1234 -> 12 and 34. Returns list of cons pairs."
  (dotimes (i (floor (log n 10)) res)
    (let ((m (expt 10 (1+ i))))
      (let ((a (floor (/ n m)))
	    (b (mod n m)))
	(if (and (funcall primep a)
		 (funcall primep b)
		 (funcall primep (concat b a))
		 (= (ndigits n) (+ (ndigits a) (ndigits b))))
	      (push (cons a b) res))))))

(defun concat (a b)
  "Concatenates two numbers together"
  (+ (* a (expt 10 (floor (1+ (log b 10)))))
     b))

(defun remove-non-brothers (ht list prime n)
  (remove-if (lambda (x) (< (n-prime-brothers prime x ht) n)) 
	     list))

(defun hash-table-list (ht &aux res)
  (maphash (lambda (k v) (push (list k v) res)) ht)
  res)

(defun hash-table-clean-length (ht limit)
  (maphash (lambda (key value)
	     (if (< (length value) limit)
		 (progn
		   (remhash key ht)
		   (maphash (lambda (k v)
			      (setf (gethash k ht)
				    (remove key v)))
			    ht))))
	   ht)
  ht)

(defun potential-list (limit n)
  (let ((ht (prime-pairs limit)))
    (remove-if (lambda (y) (or (null (cadr y)) (< (length (cadr y)) (1- n)))) 
	       (mapcar (lambda (x) (list (car x) (remove-non-brothers ht (cadr x) (car x) (- n 2)))) 
		       (hash-table-list ht)))))

(defun number-of-hits (pair-list)
  (let ((ht (make-hash-table)))
    (dolist (p pair-list ht)
      (dolist (i (cadr p))
	(if (gethash i ht)
	    (incf (gethash i ht))
	    (setf (gethash i ht) 1))))))
		  
(defun number-of-common-elements (a b)
  "Number of same numbers in order lists"
  (if (or (null a) (null b))
      0
      (if (= (car a) (car b))
	  (1+ (number-of-common-elements (cdr a) (cdr b)))
	  (if (< (car a) (car b))
	      (number-of-common-elements (cdr a) b)
	      (number-of-common-elements a (cdr b))))))

(defun hash-sort (ht)
  (maphash (lambda (k v) 
	     (setf (gethash k ht) (sort v #'<))) ht)
  ht)

(defun n-prime-brothers (a b ht)
  (number-of-common-elements (gethash a ht) 
			     (gethash b ht)))

(defun prime-pairs (limit)
  "Return prime pairs, and concatenated pair is also prime"
  (let ((ht (make-hash-table)))
	;(primep-mem #'primep)) ;(memoize #'primep)))
    (labels ((put-hash (key value)
	       (if (gethash key ht)
		   (pushnew value (gethash key ht))
		   (setf (gethash key ht) (list value)))))
      (do ((n 3 (+ n 2)))
	  ((> n limit) (hash-sort ht))
;      (dotimes (n limit (hash-sort ht))
	(if (primep n)    ;(funcall primep-mem n)
	    (dolist (l (prime-split n))
	      (put-hash (car l) (cdr l))
	      (put-hash (cdr l) (car l))))))))

;; ----------------

(defun run (list n collect &optional (depth 0) (res nil))
  (dolist (l list)
    (let ((buf nil))
      (dolist (m list)
	(if (and (/= l m)
		 (special-pair-p l m))
	    (push m buf)))
      (if (and (= depth (- n 2))
	       (>= (length buf) 1))
	  (funcall collect (cons l (append res buf)))
	  (run buf n collect (1+ depth) (cons l res))))))

(defun euler-60 (&optional (n 5) (limit 10000) &aux res)
  (run (remove-if-not #'primep (range limit)) 
       n 
       (lambda (x) (push x res)))
  (car (sort (mapcar (lambda (x) (apply #'+ x)) 
		     res) 
	     #'<)))



