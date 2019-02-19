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
