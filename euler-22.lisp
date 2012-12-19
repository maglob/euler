;;;; What is the total of all the name scores in the file?

(defun avalue(s)
  (apply #'+ 
	 (mapcar #'(lambda(x) (1+ (- (char-code x) (char-code #\A)))) 
		 (coerce s 'list))))

(defun worklist(lst)
  (let ((sum 0)
	(cnt 1))
    (dolist (s (sort lst #'string<) sum)
      (incf sum (* cnt (avalue s)))
      (incf cnt))))



(defun euler-22()
  (let ((in (open "/Users/marko/prg/lisp/names.txt"))
	(str))
    (setf str (read-line in))
    (close in)
    (setf str (remove #\, str))
    (setf str (read-from-string (concatenate 'string "(" str ")")))
    (worklist str)))
  