;; Find the maximum total from top to bottom in triangle.txt containing a triangle with one-hundred rows.

(defun read-data (file &aux res)
  (with-open-file (s file)
    (do ((l (read-line s nil) (read-line s nil)))
	((null l) res)
      (push (mapcar #'parse-integer (split l #\Space)) res))))

(defun pair-list (lst)
  "Genere list of pairs: (1 2 3) -> ((1 2) (2 3))"
  (mapcar #'list lst (cdr lst)))

(defun max-row (row-new row-buffer)
  (mapcar (lambda (a b) (+ a (max (car b) (cadr b)))) 
	  row-new (pair-list row-buffer)))

(defun max-path (list &optional buffer)
  (if (null list)
      buffer
      (if (null buffer)
	  (max-path (cdr list) (car list))
	  (max-path (cdr list) (max-row (car list) buffer)))))

(defun euler-67 (&optional (file "~/Dropbox/prg/euler/triangle.txt"))
  (car (max-path (read-data file))))
  