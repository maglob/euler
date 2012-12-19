;;;; Find the largest palindrome made from the product of two 3-digit numbers.

(defun palinp(s)
  (string= s (reverse s)))

(defun palin-number-p(n)
  (palinp (write-to-string n)))

(defun divp(n)
  (do ((i 100 (1+ i)))
      ((> i 999) nil)
    (if (and (zerop (mod n i))
	     (>= (/ n i) 100)
	     (<= (/ n i) 999))
	(return i))))

(defun euler-4()
  (loop for i from 999999 downto 999 when 
       (and (palin-number-p i) 
	    (divp i)) 
     return i))