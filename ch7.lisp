; 7.1
(defun add1 (x) (+ x 1))
(defun add1-list (x) (mapcar #'add1 x))

; 7.2
(defvar daily-planet)
(setf daily-planet '((olsen jimmy 123-76-4535 cub-reporter)
                     (kent  clark 089-52-6787 reporter)
                     (lane  lois  951-26-1438 reporter)
                     (white perry 355-16-7439 editor)))

(defun social-security-numbers () (mapcar #'third daily-planet))

; 7.7
(defun flip (x)
  (mapcar #'(lambda (n) (if (eq n 'up) 'down 'up)) x))

; 7.8
(defun roughly-equal (x k)
  (find-if #'(lambda (n) (and (< n (+ k 10)) (> n (- k 10)))) x))

; 7.9
(defun find-nested (x)
  (find-if #'listp x))

; 7.10
(defvar note-table)
(setf note-table '((c 1) (c-sharp 2)
                   (d 3) (d-sharp 4)
                   (e 5)
                   (f 6) (f-sharp 7)
                   (g 8) (g-sharp 9)
                   (a 10) (a-sharp 11)
                   (b 12)))

(defun numbers (notes)
  (mapcar #'(lambda (note) (second (assoc note note-table))) notes))

(defun notes (numbers)
  (mapcar #'(lambda (number) (first (find-if #'(lambda (elem) (eql number (second elem))) note-table))) numbers))

(defun raise (n numbers)
  (mapcar #'(lambda (x) (+ x n)) numbers))

(defun normalize (numbers)
  (mapcar #'(lambda (x) (cond ((> x 12) (- x 12)) ((< x 1) (+ x 12)) (t x))) numbers))

(defun transpose (n song)
  (notes (normalize (raise n (numbers song)))))

; 7.14
(defun my-intersection (x y)
  (remove-if-not #'(lambda (elem) (member elem y)) x))

(defun my-union (x y)
  (append x (remove-if #'(lambda (elem) (member elem x)) y)))

; 7.17
(defun flattened-length1 (x)
  (reduce #'+ (mapcar #'length x)))
(defun flattened-length2 (x)
  (length (reduce #'append x)))

; 7.29
(defvar database)
(setf database '((b1 shape brick)(b1 color green)(b1 size small)(b1 supported-by b2)(b1 supported-by b3)(b2 shape brick)(b2 color red)(b2 size small)(b2 supports b1)(b2 left-of b3)(b3 shape brick)(b3 color red)(b3 size small)(b3 supports b1)(b3 right-of b2)(b4 shape pyramid)(b4 color blue)(b4 size large)(b4 supported-by b5)(b5 shape cube)(b5 color green)(b5 size large)(b5 supports b4)(b6 shape brick)(b6 color purple)(b6 size large)))

(defun match-element (x y)
  (or (eq x '?) (eq y '?) (eq x y)))

(defun match-triple (assertion pattern)
  (every #'match-element assertion pattern))

(defun fetch (pattern)
  (remove-if-not #'(lambda (x) (match-triple pattern x)) database))

(defun color-pattern (block)
  (list block 'color '?))

(defun supporters (block)
  (mapcar #'third (fetch (list block 'supported-by '?))))

(defun any (list) (reduce #'(lambda (x y) (or x y)) list))
(defun supp-cubep (block)
  (listp (any (mapcar #'(lambda (x) (fetch (list x 'shape 'cube))) (supporters block)))))

(defun description (block)
  (let* ((desc1 (fetch (list block '? '?)))
         (desc2 (mapcar #'rest desc1)))
    (reduce #'append desc2)))
