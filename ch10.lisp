; 10.5
(defun ugly (x y)
  (let* ((max (max x y))
         (avg (/ (+ x y) 2.0))
         (pct (* 100 (/ avg max))))
    (list 'average avg 'is pct 'percent 'of 'max max)))

; 10.8

(defun make-board () (list 'board 0 0 0
                                  0 0 0
                                  0 0 0))

(defvar *computer*)
(setf *computer* 4)
(defvar *opponent*)
(setf *opponent* 1)

(defun convert-to-letter (v)
  (cond ((eq v *opponent*) "O")
        ((eq v *computer*) "X")
        (t " ")))

(defun print-row (x y z)
  (format t "~&   ~A | ~A | ~A"
    (convert-to-letter x)
    (convert-to-letter y)
    (convert-to-letter z)))

(defun print-board (board)
  (format t "~%")
  (print-row (nth 1 board) (nth 2 board) (nth 3 board))
  (format t "~&  -----------")
  (print-row (nth 4 board) (nth 5 board) (nth 6 board))
  (format t "~&  -----------")
  (print-row (nth 7 board) (nth 8 board) (nth 9 board))
  (format t "~%~%"))

(defun make-move (player pos board)
  (setf (nth pos board) player)
  board)

(defvar *triplets*)
(setf *triplets*
  '((1 2 3) (4 5 6) (7 8 9) ; horizontal
    (1 4 7) (2 5 8) (3 6 9) ; vertical
    (1 5 9) (3 5 7)))       ; diagonal

(defun sum-triplet (board triplet)
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)))

(defun compute-sums (board)
  (mapcar #'(lambda (triplet) (sum-triplet board triplet)) *triplets*))

(defun winner-p (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *computer*) sums)
        (member (* 3 *opponent*) sums))))

(defun board-full-p (board)
  (not (member 0 board)))

(defun play-game (board opponent-first moves)
  (if opponent-first
      (opponent-move board moves)
      (computer-move board moves)))

(defun opponent-move (board moves)
  (cond ((null moves) nil)
        ((plusp (nth (first moves) board)) (opponent-move board (rest moves)))
        (t (let* ((pos (first moves))
                  (new-board (make-move *opponent* pos board)))
              (format t "~&Your move: ~S" pos)
              (print-board new-board)
              (cond ((winner-p new-board)
                     (format t "~&You win!"))
                    ((board-full-p new-board)
                     (format t "~&Tie game."))
                    (t (computer-move new-board (rest moves))))))))

(defun computer-move (board moves)
  (let* ((best-move (choose-best-move board))
         (pos (first best-move))
         (strategy (second best-move))
         (new-board (make-move *computer* pos board)))
    (format t "~&My move: ~S" pos)
    (format t "~&My strategy: ~A" strategy)
    (print-board new-board)
    (cond ((winner-p new-board)
           (format t "~&I win!"))
          ((board-full-p new-board)
           (format t "~&Tie game."))
          (t (opponent-move new-board moves)))))

(defun choose-best-move (board)
  (or (make-three-in-a-row board)
      (block-opponent-win board)
      (block-squeeze-play board)
      (block-two-on-one board)
      (random-move-strategy board)))

(defun random-move-strategy (board)
  (list (pick-random-empty-position board) "random move"))

(defun pick-random-empty-position (board)
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
        pos
        (pick-random-empty-position board))))

(defun make-three-in-a-row (board)
  (let ((pos (win-or-block board (* 2 *computer*))))
    (and pos (list pos "make three in a row"))))

(defun block-opponent-win (board)
  (let ((pos (win-or-block board (* 2 *opponent*))))
    (and pos (list pos "block opponent"))))

(defun win-or-block (board target-sum)
  (let ((triplet (find-if #'(lambda (trip) (equal (sum-triplet board trip) target-sum)) *triplets*)))
    (when triplet
      (find-empty-position board triplet))))

(defun find-empty-position (board squares)
  (find-if #'(lambda (pos) (zerop (nth pos board))) squares))

; (a)
(defvar *corners*)
(setf *corners* '(1 3 7 9))
(defvar *sides*)
(setf *sides* '(2 4 6 8))

; (b)
(defun board-subset (board squares)
  (mapcar #'(lambda (pos) (nth pos board)) squares))

(defun diagonals (board)
  (list (board-subset board '(1 5 9)) (board-subset board '(3 5 7))))

(defun block-squeeze-play (board)
  (let ((pattern (list *opponent* *computer* *opponent*))
        (diagonals (diagonals board)))
    (let ((pos (and
                (member pattern diagonals :test #'equal)
                (find-empty-position board *sides*))))
      (and pos (list pos "block squeeze play")))))

; (c)
(defun block-two-on-one (board)
  (let ((pattern (list *opponent* *opponent* *computer*))
        (diagonals (diagonals board)))
    (let ((pos (and
                (intersection (list pattern (reverse pattern)) diagonals :test #'equal)
                (find-empty-position board *corners*))))
      (and pos (list pos "block two on one play")))))
