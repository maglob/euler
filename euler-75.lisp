;; Given that L is the length of the wire, for how many values of L  1,500,000 can exactly one integer sided right angle triangle be formed?

(defun pythagorean-triple (m n)
  "Genereate pythagorean triple a^2 + b^2 = c^2. m>n"
  (list (- (* m m) (* n n))
	(* 2 m n)
	(+ (* m m) (* n n))))

(defun primitive-triple-p (m n)
  "Is generated pythagorean triple is primitive?"
  (and (= 1 (gcd m n))
       (oddp (- m n))))

(defun euler-75 (&optional (limit 1500000))
  "Use formula from wiki to generate primitive pythagorean triples. See http://en.wikipedia.org/wiki/Pythagorean_triple"
  (let ((hits (make-hash-table)))
    (labels ((add-hit (n)
	       (if (gethash n hits)
		   (incf (gethash n hits))
		   (setf (gethash n hits) 1))))
      (do ((m 2 (1+ m))
	   (end (isqrt (/ limit 2))))
	  ((> m end) )
	(do ((n 1 (1+ n)))
	    ((>= n m))
	  (if (primitive-triple-p m n)
	      (let ((base-trip (pythagorean-triple m n)))
		(do* ((i 1 (1+ i))
		      (trip base-trip (mapcar #'(lambda (x) (* x i)) base-trip))
		      (len (apply #'+ trip) (apply #'+ trip)))
		     ((> len limit))
		  (add-hit len)))))))
    (let ((res 0))
      (maphash #'(lambda (key val) 
		   (declare (ignore key))
		   (if (= 1 val) (incf res))) 
	       hits)
      res)))




;; below are two false starts, with more or less brute force attempts

(defun rat-p (a b c)
  "Is triangle a-b-c right angle triangle, eg. a*a + b*b = c*c"
  (= (* c c) 
     (+ (* b b) (* a a))))

(defun quadratic-roots (a b c)
  "Real solutions for quadratic functions ax**2 + bx + c = 0"
  (let ((d (- (* b b) (* 4 a c))))
    (cond
      ((zerop d) (list (/ (- b) (+ a a))
		       (/ (- b) (+ a a))))
      ((> d 0) (list (/ (+ (- b) (sqrt d)) (+ a a))
		     (/ (- (- b) (sqrt d)) (+ a a))))
       (t nil))))

(defun quadratic-roots-fast (b c)
  "Real solutions for quadratic functions ax**2 + bx + c = 0"
  (let ((d (- (* b b) (* 2 c))))
    (cond
      ((zerop d) (list (/ (- b) 2)
		       (/ (- b) 2)))
      ((> d 0) (list (/ (+ (- b) (sqrt d)) 2)
		     (/ (- (- b) (sqrt d)) 2)))
       (t nil))))


(defun integer-quad-roots-p (b c)
  (let ((d (- (* b b) (* 2 c))))
    (if (>= d 0)
	(let ((isqrtd (isqrt d)))
	  (and (= d (* isqrtd isqrtd))
	       (zerop (mod (- b isqrtd) 2))
	       (zerop (mod (+ b isqrtd) 2)))))))

(defun integer-quad-roots (b c)
  (let ((d (- (* b b) (* 2 c))))
    (if (>= d 0)
	(let ((isqrtd (my-isqrt d)))  ;(isqrt d)))
	  (if (and isqrtd    ;(= d (* isqrtd isqrtd))
		   (zerop (mod (+ b isqrtd) 2)))
	      (list (/ (- isqrtd b) 2)
		    (/ (- (- b) isqrtd) 2)))))))

    
(defun quad-rat (c len)
  (let ((len-c (- len c)))
    (integer-quad-roots (- len-c)
			(- (- (* c c) (* len-c len-c))))))


(defun rat-solutions (c len)
  "Find right-angle-triangle solutions with c and a+b+c=len"
  (do* ((a (max 1 (ceiling (/ (- len c c) 2))) (1+ a))
	(b (- len c a) (1- b)) 
	(cc (* c c))
	(end (floor (/ (- len c) 2))))
       ((> a end) )
    (if (= cc (+ (* b b) (* a a)))
	(return (list a b c)))))

(defvar *ht-sqrt*)
(defvar *ht-rat-count*)

(defun my-isqrt (n)
  (gethash n *ht-sqrt*))

(defun my-add-hits (len n)
  ;(format t "add: ~A ~A ~%" len n)
  (if (gethash len *ht-rat-count*)
      (incf (gethash len *ht-rat-count*) n)
      (setf (gethash len *ht-rat-count*) n)))

	      
      
    
      


(defun euler-75-c (&optional (limit 100))
  (setf *ht-rat-count* (make-hash-table))
  (setf *ht-sqrt* (make-hash-table))
  (do ((i 1 (1+ i))
       (m (/ limit 2)))
      ((> i m))
    (setf (gethash (* i i) *ht-sqrt*) i))
  (let ((res))
    (do ((len 4 (+ len 2))
	 (res-tmp nil nil))
	((> len limit) res)
      (do ((c (1- (floor (/ len 2))) (1- c))
	   (end (isqrt (* (expt (floor (/ len 4)) 2) 2))))
	  ((< c end) res-tmp)
	(let ((r (quad-rat c len)))
	  (when r
	    (push (append r (list c)) res-tmp))))
      (when res-tmp
	(my-add-hits len (length res-tmp))
	(push res-tmp res)))))

  
(defun euler-75-b (&optional (limit 100))
  (let ((res))
    (dotimes (i limit res)
      (let ((len (1+ i))
	    res-tmp)
	(do ((c (floor (/ len 2)) (1- c))
	     (end (isqrt (* (expt (ceiling (/ len 4)) 2) 2))))
	    ((< c end) res-tmp)
	  (let ((r (rat-solutions c len)))
	    ;(format t "~A ~A~%" c end)
	    (when r
	      (push r res-tmp))))
	(when res-tmp
	  (push res-tmp res))))))
	 