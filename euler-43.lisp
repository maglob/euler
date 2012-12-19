;; euler 43: Find the sum of all 0 to 9 pandigital numbers with this property.

(load "/Users/marko/prg/lisp/toolkit.lisp")

(defun sub-list (lst n len)
  (if (<= len 0)
      nil
      (if (> n 0)
	  (sub-list (cdr lst) (1- n) len)
	  (cons (car lst) (sub-list (cdr lst) n (1- len))))))

(defun tri-div? (lst divisors)
  (if divisors
      (and (= (mod (to-number (list (car lst) (cadr lst) (caddr lst)))
		   (car divisors))
	      0)
	   (tri-div? (cdr lst) (cdr divisors)))
      t))

(defun euler-43()
  (reduce #'+ (mapcar #'to-number 
		      (remove-if-not (lambda (x) (tri-div? (cdr x) '(2 3 5 7 11 13 17)))
				     (permutate '(0 1 2 3 4 5 6 7 8 9))))))