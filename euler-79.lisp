;; Given that the three characters are always asked for in order, analyse the file so as to determine the shortest possible secret passcode of unknown length.

(defun read-data (&optional (file "~/Dropbox/prg/euler/keylog.txt"))
  (let ((res nil))
    (with-open-file (f file)
      (do ((s (read-line f nil) (read-line f nil)))
	  ((null s) (nreverse res))
      (push s res)))))

(defun match (digits key)
  (dolist (i digits)
    (if (null key)
	(return)
	(when (= i (car key))
	  (pop key))))
  (null key))

(defun euler-79 ()
  (let* ((keys (mapcar #'to-digits
		      (mapcar #'parse-integer
			      (read-data))))
	 (digits (mapcar #'car (histogram (flatten keys)))))
    (dolist (n (permutate digits))
      (let ((hit t))
	(dolist (k keys)
	  (when (not (match n k))
	    (setf hit nil)
	    (return)))
	(if hit
	    (return-from euler-79 (to-number n)))))))