; 11.1
(defun it-member (elem list)
  (dolist (x list)
    (when (equal x elem) (return t))))

; 11.2
(defun it-assoc (key table)
  (dolist (x table)
    (when (equal (first x) key) (return x))))

; 11.5
(defun it-nth (n list)
  (let ((i 0))
    (dolist (x list)
      (when (eq i n) (return x))
      (incf i))))

; 11.9
(defun check-all-odd (list)
  (do ((z list (rest z))
       (all-odd t (and all-odd (oddp (first z)))))
      ((null z) all-odd)))

; 11.11
(defun find-largest (list-of-numbers)
  (do* ((x list-of-numbers (rest x))
        (e (first x) (first x))
        (largest e (max largest e)))
       ((null x) largest)))
