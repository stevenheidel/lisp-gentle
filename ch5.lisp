; 5.6
(defun throw-die () (+ (random 6) 1))

(defun throw-dice () (list (throw-die) (throw-die)))

(defun snake-eyes-p (throw) (equal throw '(1 1)))
(defun boxcars-p (throw) (equal throw '(6 6)))

(defun throw-sum (throw) (+ (first throw) (second throw)))
(defun instant-win-p (throw) (equal (throw-sum throw) 7))
(defun instant-loss-p (throw)
  (let ((sum (throw-sum throw)))
    (or (equal sum 2) (equal sum 3) (equal sum 12))))

(defun say-throw (throw)
  (cond ((snake-eyes-p throw) 'snake-eyes)
        ((boxcars-p throw) 'boxcars)
        (t (throw-sum throw))))

(defun craps ()
  (let* ((throw (throw-dice))
         (msg1 (list 'throw (first throw) 'and (second throw) '--))
         (msg2 (cond ((instant-loss-p throw) (list (say-throw throw) '-- 'you 'lose))
                     ((instant-win-p throw) (list (say-throw throw) '-- 'you 'win))
                     (t (list 'your 'point 'is (say-throw throw))))))
    (append msg1 msg2)))
