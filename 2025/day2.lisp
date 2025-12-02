(defun parse-range (s)
  (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" s)))

(defparameter *input* (uiop:read-file-string "input.txt"))

(defparameter *ranges*
  (let ((parts (uiop:split-string *input* :separator ",")))
    (mapcar #'parse-range parts)))

(defun invalid? (id)
  (let* ((s (format nil "~a" id))
         (half (floor (length s) 2)))
    (equal (subseq s 0 half) (subseq s half))))

(defun all-invalids (range test)
  (destructuring-bind (start end) range
    (loop for i from start below end
          when (funcall test i)
            collect i)))

(defun part-1 ()
  (loop for r in *ranges*
        sum (loop for i in (all-invalids r #'invalid?) sum i)))

(defun repeat? (s pattern)
  (or (zerop (length s))
      (and (zerop (mod (length s) (length pattern)))
           (equal pattern (subseq s 0 (length pattern)))
           (repeat? (subseq s (length pattern)) pattern))))

(defun invalid2? (id)
  (let* ((s (format nil "~a" id))
         (len (length s)))
    (loop for i from 1 to (floor len 2)
          thereis (repeat? s (subseq s 0 i)))))

(defun part-2 ()
  (loop for r in *ranges*
        sum (loop for i in (all-invalids r #'invalid2?) sum i)))
