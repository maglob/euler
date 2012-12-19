;; Find the minimal path sum, in matrix.txt a file containing a 80 by 80 matrix, from the top left to the bottom right by only moving right and down.

(defun read-data (&optional (file "~/Dropbox/prg/euler/matrix.txt"))
  (let ((res nil))
    (with-open-file (f file)
      (do ((s (read-line f nil) (read-line f nil)))
	  ((null s) (nreverse res))
	(push (mapcar #'parse-integer (split s #\,)) res)))))

(defun get-value (data x y &optional (default 99999999999999999))
  (if (and (>= x 0) (< x 80)
	   (>= y 0) (< y 80))
      (aref data y x)
      default))

(defun walk (data sum)
  (do ((y 79 (1- y)))
      ((< y 0) )
    (do ((x 79 (1- x)))
	((< x 0) )
      (if (and (= x 79) (= y 79))
	  (setf (aref sum y x)
		(get-value data x y))
	  (setf (aref sum y x)
		(+ (get-value data x y)
		   (min (get-value sum (1+ x) y)
			(get-value sum x (1+ y)))))))))
    

  
(defun euler-81 ()
  (let ((data (make-array '(80 80) :initial-contents (read-data)))
	(sum (make-array '(80 80) :initial-element nil)))
    (walk data sum)
    (aref sum 0 0)))