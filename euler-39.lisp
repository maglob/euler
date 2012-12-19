;; euler 39: For which value of p<=1000 (rat a,b,c), is the number of solutions maximised?


(defun rat? (a b c)
  (declare ((integer 0 9999) a b c)
	   (optimize (speed 3) (safety 0)))
  (= (+ (* a a)
	(* b b))
     (* c c)))

;; rat - right angle triangle
(defun rat-count (perimeter &aux (sum 0))
  (declare ((integer 0 99999) perimeter sum)
	   (optimize (speed 3)))
  "return number of right angle triangle with _perimeter_"
  (do ((a 1 (1+ a)))
      ((> a (- perimeter 2)) sum)
    (declare ((integer 0 99999) a))    
    (do ((b 1 (1+ b)))
	((> (+ a (+ a b) 1) perimeter))
      (declare ((integer 0 99999) b))
      (if (rat? a (+ a b) (- perimeter a (+ a b)))
	  (incf sum)))))
      
(defun euler-39 (&optional (limit 1000) &aux (max 0))
  (dotimes (i limit max)
    (if (> (rat-count (1+ i))
	   (rat-count max))
	(setf max (1+ i)))))
	   