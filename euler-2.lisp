(defun fib1((n) 
  (if (<= n 1)
      1
      (+ (fib (- n 1)) 
	 (fib (- n 2)))))

(defun fib2(n)
  (do ((i 0 (1+ i))
       (b 0 a)
       (a 1 (+ a b))
       (lst nil (if (evenp a)
		    (cons a lst)
		    lst)))
      ((>= a n) lst)))

(defun euler-2()
  (progn
    (print (apply #'+ (fib2 1000000)))))