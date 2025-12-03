(defparameter *input* (uiop:read-file-lines "input.txt"))

(defparameter *lines* (mapcar (lambda (s) (mapcar #'digit-char-p (coerce s 'list))) *input*))

(defun max-joltage (l)
  (let* ((m (apply #'max l))
         (i (position m l)))
    (if (= i (1- (length l)))
        ; Search for 10s digit prior to m
        (let ((n (apply #'max (remove m l :count 1))))
          (+ (* n 10) m))
        ; Search for 1s digit after m
        (let ((n (apply #'max (subseq l (1+ i)))))
          (+ (* m 10) n)))))

(defun part-1 ()
  (loop for l in *lines*
        sum (max-joltage l)))

(defun max-joltage-2 (l rem acc)
  (if (zerop rem)
      acc
      ; Find the max digit, excluding the last rem digits.
      ; Then recurse on all digits after the max digit.
      (let* ((digit (apply #'max (subseq l 0 (- (length l) rem -1))))
             (pos (position digit l)))
        (max-joltage-2 (subseq l (1+ pos)) (1- rem) (+ (* acc 10) digit)))))

(defun part-2 ()
  (loop for l in *lines*
        sum (max-joltage-2 l 12 0)))
