;; How many Lychrel numbers are there below ten-thousand?

(defun reverse-number (n)
  "Returns a reverse number, eg. 123 -> 321"
  (do ((res 0))
      ((< n 10) (+ (* res 10) n))
    (setf res (+ (* res 10)
		 (mod n 10))
	  n (floor (/ n 10)))))
    
(defun palindrome-p (n)
  (= n (reverse-number n)))

(defun lychrel-number-p (n)
  (dotimes (i 49 t)
    (setf n (+ n (reverse-number n)))
    (if (palindrome-p n)
	(return nil))))

(defun range(n)
  (do ((i n (1- i))
       (lst nil (cons i lst)))
       ((<= i 0) lst)))


(defun euler-55 ()
  (count-if (lambda (x) x) (mapcar #'lychrel-number-p (range 10000))))

