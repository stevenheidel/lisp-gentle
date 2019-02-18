; 4.1
(defun make-even (x)
  (if (evenp x) x (+ x 1)))

; 4.2
(defun further (x)
  (if (< x 0) (- x 1) (+ x 1)))

; 4.3
(defun my-not (x)
  (if x nil t))

; 4.4
(defun ordered (x y)
  (if (< x y) (list x y) (list y x)))

; 4.6
(defun my-abs (x)
  (cond ((< x 0) (* x -1))
        (t x)))

; 4.8
(defun emphasize3 (x)
  (cond ((equal (first x) 'good) (cons 'great (rest x)))
        ((equal (first x) 'bad) (cons 'awful (rest x)))
        (t (cons 'very x))))

; 4.9
(defun make-odd (x)
  (cond ((not (oddp x)) (+ x 1))
        (t x)))

; 4.10
(defun constrain (x min max)
  (cond ((< x min) min)
        ((> x max) max)
        (t x)))

; 4.11
(defun firstzero (x)
  (cond ((equal (first x) 0) 'first)
        ((equal (second x) 0) 'second)
        ((equal (third x) 0) 'third)
        (t 'none)))

; 4.13
(defun howcompute (x y z)
  (cond ((equal (+ x y) z) 'sum-of)
        ((equal (* x y) z) 'product-of)
        (t '(beats me))))

; 4.15
(defun geqp (x y)
  (or (> x y) (equal x y)))

; 4.16
(defun ex416 (x)
  (cond ((and (oddp x) (> x 0)) (* x x))
        ((and (oddp x) (< x 0)) (* x 2))
        (t (/ x 2))))

; 4.18
(defun play (a b)
  (cond ((or (and (equal a 'rock) (equal b 'scissors)) (and (equal a 'scissors) (equal b 'paper)) (and (equal a 'paper) (equal b 'rock))) 'first-wins)
        ((or (and (equal b 'rock) (equal a 'scissors)) (and (equal b 'scissors) (equal a 'paper)) (and (equal b 'paper) (equal a 'rock))) 'second-wins)
        (t 'tie)))

; 4.19
(defun my-and (x)
  (cond ((equal x nil) t)
        ((equal (first x) nil) nil)
        (t (my-and (rest x)))))

; 4.37
(defun nand (x y) (not (and x y)))

(defun not2 (x) (nand x x))
(defun and2 (x y) (nand (nand x y) (nand x y)))
(defun or2 (x y) (nand (nand x x) (nand y y)))

; 4.38
(defun nor (x y) (not (or x y)))

(defun not3 (x) (nor x x))
(defun and3 (x y) (nor (nor x x) (nor y y)))
(defun or3 (x y) (nor (nor x y) (nor x y)))
