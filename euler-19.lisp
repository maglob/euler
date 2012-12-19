;;;; How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)? 
;;;; 1 Jan 1900 was a Monday.

(defun euler-19()
  (do ((sum 0)
       (d (encode-universal-time 0 0 4 1 1 1901) (+ d (* 7 24 3600)))
       (end (encode-universal-time 0 0 4 1 1 2001)))
      ((>= d end) sum)
    (if (= 1 (nth 3 (multiple-value-list (decode-universal-time d))))	
	(incf sum))))
      
     
(defun numdays(month year)
  (cond ((find month '(1 3 5 7 8 10 12)) 31)
	((find month '(4 6 9 11)) 30)
	(t (if (zerop (mod year 4))
	       (if (and (zerop (mod year 100))
			(not (zerop (mod year 400))))
		   28
		   29)
	       28))))

(defun euler-19-2()
  (do* ((sum 0)
	(m 0 (1+ m))
	(d 1 (mod (+ d (numdays (1+ (mod m 12)) (+ 1900 (floor (/ m 12))))) 7)))
       ((> (/ m 12) 100) sum)
    (if (and (zerop (mod d 7))
	     (>= (floor (/ m 12)) 1))
	(incf sum))))
	