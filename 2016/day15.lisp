(eval-when (:load-toplevel)
  (defparameter *this-script* (truename *load-truename*)))

(defparameter *this-dir*
  (merge-pathnames "." *this-script*))

(defparameter *input*
   (uiop:read-file-lines (merge-pathnames "input.txt" *this-dir*)))

(defun parse-line (line)
  (let ((parts (uiop:split-string line :separator " ")))
    (list (parse-integer (string-trim "." (nth 11 parts))) (parse-integer (nth 3 parts)))))

(defun pass-time (discs)
  (mapcar #'(lambda (disc) (list (mod (1+ (first disc)) (second disc)) (second disc))) discs))

(defun fall-through (discs)
  (do ((i 0 (1+ i)))
      ((= i (length discs)) t)
    (let ((disc (nth i discs)))
      (if (/= 0 (mod (+ i (first disc)) (second disc)))
          (return-from fall-through nil)))))

(defun press-when (discs)
  (do ((i 0 (1+ i))
       (curr discs (pass-time curr)))
      ((fall-through curr) (1- i))))

(defparameter *discs* (mapcar #'parse-line *input*))

(defun part-1 () (press-when *discs*))

(defun part-2 () (press-when (append *discs* '((0 11)))))
