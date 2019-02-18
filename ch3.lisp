; 3.22(c)
(defun myfun (a b) (list (list a) b))

; 3.22(d)
(defun firstp (a list)
  (equal a (car list)))

; 3.22(e)
(defun mid-add1 (l)
  (list (first l) (+ (second l) 1) (third l)))

; 3.22(f)
(defun f-to-c (temp)
  (/ (* 5 (- temp 32)) 9))
