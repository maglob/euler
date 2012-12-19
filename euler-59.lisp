;; decrypt the message and find the sum of the ASCII values in the original text.

(load "~/Dropbox/prg/euler/toolkit.lisp")

(defun read-data ()
  (with-open-file (s "~/Dropbox/prg/euler/cipher1.txt")
    (mapcar #'parse-integer (split (read-line s nil) #\,))))

(defun gen-passwd (n &aux res)
  (flet ((get-char (m)
	   (code-char (+ (char-code #\a) m))))
    (dotimes (i 3 (coerce res 'string))
      (push (get-char (mod n 26)) res)
      (setf n (floor n 26)))))

(defun decode (data password &aux res)
  (let ((p password))
    (dolist (s data (nreverse res))
      (push (logxor s (car p)) res)
      (setf p (cdr p))
      (if (null p)
	  (setf p password)))))
   
(defun count-hits (needle data)
  (let ((pos (search needle data)))
    (if pos
	(+ 1 (count-hits needle (subseq data (1+ pos))))
	0)))
  
(defun string-code (s)
  (mapcar #'char-code (coerce s 'list)))

(defun code-string (s)
  (coerce (mapcar #'code-char s) 'string))

(defun score (data words &aux (res 0))
  (dolist (s words res)
    (incf res (count-hits s data))))

(defun euler-59 ()
  (let ((data (read-data))
	(words (mapcar #'string-code '("and" "the" "you")))
	(res (list 0)))
    (dotimes (a 26 (values (apply #'+ (caddr res)) (cadr res) (code-string (caddr res))))
      (dotimes (b 26)
	(dotimes (c 26)
	  (let* ((password (mapcar (lambda (x) 
				     (+ x (char-code #\a)))
				     (list a b c)))
		 (s (decode data password)))
	    (if (> (score s words) (car res))
		(setf res (list (score s words) password s)))))))))
			     
	