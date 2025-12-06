(defun parse-ints (s)
  (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" s)))

(defun transpose (m)
  (apply #'mapcar #'list m))

(defun comp (l op)
  (cond ((equal op "+") (apply #'+ l))
        ((equal op "*") (apply #'* l))))

(defun part-1 ()
  (let* ((input (uiop:read-file-lines "input.txt"))
         (ops (remove-if (lambda (s) (zerop (length s))) (uiop:split-string (car (last input)))))
         (lines (transpose (mapcar #'parse-ints (butlast input)))))
    (loop for l in lines
          for i from 0 below (length lines)
          sum (comp l (nth i ops)))))

;; Helper function that turns ((1) (2) (3) NIL (4) (5) (6) ...)
;; into ((1 2 3) (4 5 6) ...).
(defun make-groups (l &optional (acc nil) (curr nil))
  (if (null l)
      (reverse (cons curr acc))
      (let ((head (first l)))
        (if head
            (make-groups (rest l) acc (cons (first head) curr))
            (make-groups (rest l) (cons curr acc) nil)))))

(defun part-2 ()
  (let* ((input (uiop:read-file-lines "input.txt"))
         (ops (remove-if (lambda (s) (zerop (length s))) (uiop:split-string (car (last input)))))
         (strs (transpose (mapcar (lambda (s) (coerce s 'list)) (butlast input))))
         (raw-nums (mapcar (lambda (l) (parse-ints (concatenate 'string l))) strs))
         (nums (make-groups raw-nums)))
    (loop for l in nums
          for i from 0 below (length nums)
          sum (comp l (nth i ops)))))
