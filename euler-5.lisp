;;;; What is the smallest number that is evenly divisible by all of the numbers from 1 to 20?

(defun primep(n)
  (let ((end (floor (sqrt n))))
    (cond ((= n 1) t)
	  ((= 0 (mod n 2)) nil)
	  (t (do ((i 3 (+ i 2)))
	      ((>  i end) t)
	    (if (= 0 (mod n i))
		(return-from primep nil)))))))

(defun range(n)
  (do ((i n (1- i))
       (lst nil (cons i lst)))
       ((<= i 0) lst)))

(defun foo(n)
  (let ((step 
	 (apply #'* (remove-if-not #'primep (range n)))))
    (do ((i step (+ i step)))
	((every (lambda (x) (= 0 (mod i x))) (cdr (range n)))
	     i))))

(defun euler-5()
  (foo 20))