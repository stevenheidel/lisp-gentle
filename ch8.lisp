; 8.5
(defun add-up (x)
  (cond ((null x) 0)
        (t (+ (first x) (add-up (rest x))))))

; 8.7
(defun rec-member (elem x)
  (cond ((null x) nil)
        ((equal elem (first x)) x)
        (t (rec-member elem (rest x)))))

; 8.10
(defun rec-plus (x y)
  (let ((x+1 (+ x 1))
        (y-1 (- y 1)))
    (cond ((= y 0) x)
          (t (rec-plus x+1 y-1)))))

; 8.22
(defun all-equal (x)
  (cond ((< (length x) 2) t)
        ((equal (first x) (second x)) (all-equal (rest x)))
        (t nil)))

; 8.31
(defun compare-lengths (x y)
  (cond ((and (null x) (null y)) 'same-length)
        ((null y) 'first-is-longer)
        ((null x) 'second-is-longer)
        (t (compare-lengths (rest x) (rest y)))))

; 8.41
(defun sum-tree (x)
  (cond ((null x) 0)
        ((listp x) (+ (sum-tree (first x)) (sum-tree (rest x))))
        ((numberp x) x)
        (t 0)))

; 8.42
(defun my-subst (new old tree)
  (cond ((equal old tree) new)
        ((atom tree) tree)
        (t (cons (my-subst new old (first tree))
                 (my-subst new old (rest tree))))))

; 8.43
(defun flatten (tree)
  (cond ((null tree) nil)
        ((atom (first tree)) (cons (first tree) (flatten (rest tree))))
        (t (append (flatten (first tree)) (flatten (rest tree))))))

; 8.44
(defun tree-depth (tree)
  (cond ((null tree) 0)
        ((atom tree) 0)
        (t (max (+ (tree-depth (first tree)) 1) (+ (tree-depth (rest tree)) 1)))))

; 8.45
(defun paren-depth (tree)
  (cond ((null tree) 0)
        ((atom tree) 0)
        (t (max (+ (paren-depth (first tree)) 1) (paren-depth (rest tree))))))

; 8.51
(defun my-reverse (x) (my-reverse-recursively x nil))

(defun my-reverse-recursively (x y)
  (if (null x) y (my-reverse-recursively (rest x) (cons (first x) y))))

; 8.56
(defun every-other (x) (every-other-recursively x t))
(defun every-other-recursively (x other)
  (cond ((null x) nil)
        (other (cons (first x) (every-other-recursively (rest x) nil)))
        (t (every-other-recursively (rest x) t))))

; 8.58
(defun merge-lists (x y)
  (cond ((null x) y)
        ((null y) x)
        ((< (first x) (first y)) (cons (first x) (merge-lists (rest x) y)))
        (t                       (cons (first y) (merge-lists x (rest y))))))

; 8.64
(defun tree-find-if (fn tree)
  (cond ((null tree) nil)
        ((atom tree) (if (funcall fn tree) tree nil))
        (t (or (tree-find-if fn (first tree)) (tree-find-if fn (rest tree))))))

; 8.66
(defun arith-eval (expr)
  (cond ((numberp expr) expr)
        ((eq (second expr) '+) (+ (arith-eval (first expr)) (arith-eval (third expr))))
        ((eq (second expr) '-) (- (arith-eval (first expr)) (arith-eval (third expr))))
        ((eq (second expr) '*) (* (arith-eval (first expr)) (arith-eval (third expr))))
        ((eq (second expr) '/) (/ (arith-eval (first expr)) (arith-eval (third expr))))))

; 8.67
(defun legalp (expr)
  (or (numberp expr) (and (listp expr) (legalp (first expr)) (member (second expr) '(+ - * /)) (legalp (third expr)))))
