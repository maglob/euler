;;;; Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

(defun range(n)
  (do ((i n (1- i))
       (lst nil (cons i lst)))
       ((<= i 0) lst)))

(defun mul(a)
  (* a a))
 
(defun sqdiff(n)
  (abs (- (mul (apply #'+ (range n)))
	  (apply #'+ (mapcar (lambda(x)(* x x)) (range n))))))

(defun sumsq(n)
  (/ (* n (+ n 1) (+ n n 1)) 6))

(defun sqdiff2(n)
  (let ((s (/ (* n (+ n 1)) 2)))
    (- (* s s) (sumsq n))))

(defun euler-6()
  (list (sqdiff 100)
	(sqdiff2 100)))

