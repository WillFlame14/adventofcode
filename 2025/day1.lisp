(defun parse-line (s)
  (list (char s 0) (parse-integer (subseq s 1))))

(defun rotate (dial inst)
  (destructuring-bind (dir amt) inst
    (let ((res (if (eq dir #\L) (- dial amt) (+ dial amt))))
      (mod res 100))))

(defun count-0s (dial inst)
  (destructuring-bind (dir amt) inst
    (let* ((final (if (eq dir #\L) (- dial amt) (+ dial amt)))
           (actual (mod final 100))
           ; Counts the "extra" 0-crossings from a large rotation.
           ; NOTE: The dial always starts between 0 - 99.
           (extra-0s (floor (abs final) 100))
           ; If we went negative, we crossed 0 one extra time.
           (adj (if (and (> dial 0) (< final 0)) 1 0))
           ; If we end at 0, this doesn't count.
           (adj2 (if (zerop actual) -1 0)))
      (list actual (max 0 (+ extra-0s adj adj2))))))

(defparameter *input* (uiop:read-file-lines "input.txt"))
(defparameter *insts* (mapcar #'parse-line *input*))

(defun part-1 (insts)
  (loop for inst in insts
        for d = (rotate 50 inst) then (rotate d inst)
        count (zerop d)))

(defun part-2 (insts)
  (let ((cross-0s (loop for inst in insts
                        for (d x) = (count-0s 50 inst) then (count-0s d inst)
                        sum x)))
    (+ cross-0s (part-1 insts))))
