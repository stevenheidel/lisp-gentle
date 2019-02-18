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

; 6.26
(defun right-side (x)
  (rest (member '-vs- x)))

(defun left-side (x)
  (reverse (right-side (reverse x))))

(defun count-common (x)
  (length (intersection (left-side x) (right-side x))))

(defun compare (x)
  (list (count-common x) 'common 'features))

; 6.35
(defvar nerd-states)
(setf nerd-states '((sleeping eating)
                    (eating waiting-for-a-computer)
                    (waiting-for-a-computer programming)
                    (programming debugging)
                    (debugging sleeping)))

(defun nerdus (state) (second (assoc state nerd-states)))

(defun sleepless-nerd (state)
  (let ((next (nerdus state)))
    (cond ((equal next 'sleeping) (nerdus 'sleeping))
          (t next))))

(defun nerd-on-caffeine (state)
  (nerdus (nerdus state)))

(defun how-many-until-debugging (state)
  (let ((next (nerd-on-caffeine state)))
    (cond ((equal next 'debugging) 1)
          (t (+ (how-many-until-debugging next) 1)))))

; 6.36
(defun swap-first-last (x)
  (append (last x) (rest (butlast x)) (list (first x))))

; 6.37
(defun rotate-left (x)
  (append (rest x) (list (first x))))
(defun rotate-right (x)
  (append (last x) (butlast x)))

; 6.40
(defun list-to-table (x)
  (cond (x (cons x (list-to-table (rest x))))
        (t nil)))

; 6.42
(defun royal-we (x) (subst 'we 'I x))
