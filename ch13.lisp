; 13.8
(defvar *hist-array*)
(defvar *total-points*)

(defun new-histogram (num-bins)
  (setf *hist-array* (make-array num-bins :initial-element 0))
  (setf *total-points* 0))

(defun record-value (val)
  (unless (and (>= val 0) (< val (length *hist-array*)))
    (error (format nil "val ~S is out of range [~S-~S)" val 0 (length *hist-array*))))
  (incf (aref *hist-array* val))
  (incf *total-points*))

(defun print-histogram ()
  (flet ((print-line (line)
           (let ((count (aref *hist-array* line)))
             (format t "~&~2D [~3D] " line count)
             (dotimes (n count) (format t "*")))))
    (dotimes (n (length *hist-array*)) (print-line n))
    (format t "~&    ~3S total" *total-points*)))

; 13.9

(defvar crypto-text)

(setf crypto-text
  '("zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf"
     "enlpo pib slafml pvv bfwkj"))

(defvar *encipher-table*)
(defvar *decipher-table*)

(setf *encipher-table* (make-hash-table))
(setf *decipher-table* (make-hash-table))

(defun make-substitution (old new)
  (when (gethash new *encipher-table*)
    (let ((existing (gethash new *encipher-table*)))
      (error (format nil "'~A' already decodes to '~A'" existing new))))
  (setf (gethash old *decipher-table*) new)
  (setf (gethash new *encipher-table*) old)
  t)

(defun undo-substitution (char)
  (let ((deciphered-char (gethash char *decipher-table*)))
    (remhash char *decipher-table*)
    (remhash deciphered-char *encipher-table*)))

(defun clear ()
  (clrhash *encipher-table*)
  (clrhash *decipher-table*))

(defun decipher-string (encoded-string)
  (let* ((length (length encoded-string))
         (result (make-string length :initial-element #\Space)))
    (dotimes (i length result)
      (let* ((old (aref encoded-string i))
             (new (gethash old *decipher-table*)))
        (when new
          (setf (aref result i) new))))))

(defun show-line (encoded-string)
  (let ((deciphered-string (decipher-string encoded-string)))
    (format t "~&~A" encoded-string)
    (format t "~&~A" deciphered-string)))

(defun show-text (cryptogram)
  (dolist (string cryptogram)
    (show-line string)
    (format t "~%~%")))

(defun solve (old new)
  (make-substitution old new)
  (show-text crypto-text))
