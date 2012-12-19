;; euler 45: It can be verified that T285 = P165 = H143 = 40755. Find the next triangle number that is also pentagonal and hexagonal.

(defun hexagonal (n)
  (* n (+ n n -1)))

(defun pentagonal? (x)
  (let ((r (/ (1+ (sqrt (1+ (* 24d0 x))))
	      6)))
    (= r (floor r))))
    ;(integerp r)))

(defun triangle? (x)
  (let ((r (/ (- (sqrt (1+ (* 8d0 x))) 1)
	      2)))
    (= r (floor r))))
    ;(integerp r)))

(defun triangle-inv (x)
  (let ((r (/ (- (sqrt (1+ (* 8 x))) 1)
	      2)))
    (floor r)))

(defun euler-45 (&optional (start  144) &aux res)
  (do ((i start (1+ i)))
      (res res)
    (let ((n (hexagonal i)))
      (if (and (triangle? n)
	       (pentagonal? n))
	  (setf res n)))))
