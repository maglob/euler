;; Find the value of n, 1  n  107, for which φ(n) is a permutation of n and the ratio n/φ(n) produces a minimum.

;; (remove-if-not #'totient-permutation-p (range 2 9999999))
;; (setf *foo* *)
;; (reduce (lambda (a b) (if (< (cadr a) (cadr b)) a b)) (mapcar (lambda (x) (list x (totient-ratio x))) *foo*))
;; (8319823 8319823/8313928)

(load "~/Dropbox/prg/euler/toolkit.lisp")

(defun signature-list (lst)
  (let ((res 0))
    (dolist (e lst res)
      (incf res (expt 10 e)))))

(defun totient (n)
  "Algorithm from http://en.wikipedia.org/wiki/Euler's_totient_function"
  (let ((pf (delete-duplicates (prime-factors n)))
	(res n))
    (dolist (e pf res)
      (setf res (* res (- 1 (/ e)))))))

(defun totient-ratio (n)
  (/ n (totient n)))

(defun totient-permutation-p (n)
  (let ((phi (totient n)))
    (if (= (mod n 9) (mod phi 9))
	(let ((phi-dig (to-digits phi))
	      (n-dig (to-digits n)))
	  (= (signature-list n-dig) (signature-list phi-dig))))))
	 

(defun euler-70 (&optional (limit (expt 10 7)))
  (let ((min))
    (do ((i 3 (+ i 2)))
	((>= i limit) min)
      (if (totient-permutation-p i)
	  (if (null min)
	      (setf min i)
	      (if (< (totient-ratio i)
		     (totient-ratio min))
		  (setf min i)))))))


(defun euler-70-fast (&optional (limit (expt 10 7)))
  (let* ((r (floor (expt limit 1/3)))
	 (prime-list (remove-if-not #'primep (range r (expt (1+ r) 2))))
	 res)
    (reduce (lambda (a b) (if (< (cadr a) (cadr b)) a b)) 
	    (mapcar (lambda (x) (list x (totient-ratio x)))
		    (dolist (e1 prime-list res)
		      (do* ((l prime-list (cdr l))
			    (e (* e1 (car l)) (* e1 (car l))))
			   ((or (null l) (> e limit)))
			(if (totient-permutation-p e)
			    (push e res))))))))