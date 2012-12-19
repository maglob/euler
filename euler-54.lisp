;; poker hands in file poker.txt. How many hands does Player 1 win?

(load "~/prg/lisp/poker-sexpr.txt")

(defun histogram (list)
  "Creates a histogram from values in list. Returns a unordered list of poirs (value count). For example (1 5 1 2 5 1) -> ((1 3) (2 1) (5 2))"
  (let ((ht (make-hash-table))
	(res))
    (dolist (s list)
      (incf (gethash s ht 0)))
    (maphash (lambda (key value) (push (list key value) res)) ht)
    res))

(defun card-code (rank suit)
  "Rank in 1..13, suit in 0..3" 
  (+ (* rank 4) suit))

(defun card-rank (code)
  (nth-value 0 (floor (/ code 4))))

(defun card-suit (code)
  (mod code 4))

(defun card-code-from-string (s)
  "Create card-code from string, for examlpe from '4H' 'QS'"
  (card-code (position (char s 0) "_23456789TJQKA")
	     (position (char s 1) "CDHS")))


(defun hand-create (list)
  "Create hand from list of card-strings, for examepl '(\"4H\" \"TD\")"
  (let ((res))
    (dolist (s list (nreverse res))
      (push (card-code-from-string s) res))))

(defun hand-flush-p (hand)
  (every (lambda (x) (= (card-suit (car hand))
			(card-suit x))) 
	 (cdr hand)))

(defun hand-straight-p (hand)
  (reduce (lambda (a b) (if (and a (= (1+ a) b)) 
			    b))
	  (sort (mapcar #'card-rank hand) #'<)))

(defun hand-of-a-kind-ranks (hand n)
  "Returns an orderd (descending) list rank-numbers of n-of-a-kind"
  (sort (mapcar (lambda (x) (car x))
		(remove-if-not (lambda (x) (= (cadr x) n)) 
			       (histogram (mapcar #'card-rank hand))))
	#'>))

(defun hand-score (hand)
  (let ((res)
	(2akind (hand-of-a-kind-ranks hand 2))
	(3akind (hand-of-a-kind-ranks hand 3))
	(4akind (hand-of-a-kind-ranks hand 4))
	(straight (hand-straight-p hand)))
    ; A high card
    (setf res (sort (mapcar #'card-rank hand) #'>))
    ; Pair and two pairs
    (if (not 2akind)
	(setf 2akind '(0 0))
	(if (not (cdr 2akind))
	    (push 0 2akind)))
    (setf res (append 2akind res))
    ; 3 of a kind
    (push (if 3akind (car 3akind) 0) res)
    ; Straight
    (push (if straight straight 0) res)
    ; Flush
    (push (if (hand-flush-p hand) 1 0) res)
    ; 4 of a kind
    (push (if 4akind (car 4akind) 0) res)
    ; Straight flush
    (push (if (and straight
		   (hand-flush-p hand))
	      straight
	      0) res)
    (reduce (lambda (a b) (+ (* a 14) b)) res)))


(defun euler-54 ()
  (count t (mapcar (lambda (x) (> (hand-score (hand-create (car x))) (hand-score (hand-create (cadr x)))))
		   *hand-list*)))