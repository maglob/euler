;; Find the minimal path sum, in matrix.txt,  and only moving up, down, and right.

(defun load-matrix-data (file width height)
  (with-open-file (f file)
    (let ((data (make-array (list height width)))) 
      (do ((line (read-line f nil) (read-line f nil))
           (j 0 (1+ j)))
          ((null line) data)
        (do ((pos 0)
             (i 0 (1+ i)))
            ((>= i width))
          (multiple-value-bind (n endpos)
              (parse-integer line :start pos :junk-allowed t)
            (setf (aref data j i) n)
            (setf pos (1+ endpos))))))))

(defun path-score (data path)
  (let ((score 0))
    (dolist (p path score)
      (incf score (aref data (cdr p) (car p))))))

