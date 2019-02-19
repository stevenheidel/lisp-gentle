; 9.2
(defun draw-line (n)
  (if (= n 0)
    (format t "~%")
    (progn (format t "*") (draw-line (- n 1)))))

; 9.3
(defun draw-box (n m)
  (if (= m 0) nil
    (progn (draw-line n) (draw-box n (- m 1)))))

; 9.6
(defun gross-pay ()
  (format t "Enter hourly wage in dollars: ")
  (let ((wage (read)))
    (format t "Enter number of hours worked: ")
    (let ((hours (read)))
      (format t "The gross pay is ~S.~%" (* wage hours)))))
