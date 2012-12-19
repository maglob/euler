;;;; If all the numbers from 1 to 1000 (one thousand) inclusive were 
;;;; written out in words, how many letters would be used?
;;;;
;;;; > (format t "~R"  953)
;;;; nine hundred fifty-three 

(defvar single-digits)
(setf single-digits 
      (apply #'+ (map 'list #'length 
		      '("one" "two" "three" "four" "five" 
			"six" "seven" "eight" "nine"))))

(defvar teens)
(setf teens 
      (apply #'+ (map 'list #'length 
		      '("eleven" "twelve" "thirteen" "fourteen" "fifteen" 
			"sixteen" "seventeen" "eighteen" "nineteen"))))

(defvar tens20)
(setf tens20
      (apply #'+ (map 'list #'length 
		      '("twenty" "thirty" "forty" "fifty" 
			"sixty" "seventy" "eighty" "ninety"))))


(defun euler-17()
  (+
   (length "onethousand")
   (* 100 (+ single-digits (* 9 (length "hundredand"))))
   (- (* 9 (length "and")))
   (* 9 10 single-digits)
   (* 10 teens)
   (* 10 10 tens20)
   (* 10 (length "ten"))
  ))