; 6.6
(defun last-element1 (x)
  (first (last x)))
(defun last-element2 (x)
  (first (reverse x)))
(defun last-element3 (x)
  (nth (- (length x) 1) x))

; 6.7
(defun next-to-last (x)
  (second (reverse x)))

; 6.8
(defun my-butlast (x)
  (reverse (rest (reverse x))))


; 6.10
(defun palindromep (x)
  (equal x (reverse x)))

; 6.21
(defun my-subsetp (x y)
  (equal (set-difference x y) nil))

; 6.24
(defun set-equal (x y)
  (and (subsetp x y) (subsetp y x)))

; 6.25
(defun proper-subsetp (x y)
  (and (subsetp x y) (not (set-equal x y))))
