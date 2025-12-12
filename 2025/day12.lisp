(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-ppcre" :silent t)
  (load "util.lisp"))

(defun parse-line (s)
  (let ((parts (util:parse-ints s)))
    `((,(first parts) ,(second parts)) ,(subseq parts 2))))

;; A configuration is impossible if the total # of cells
;; in the pieces is greater than the total area.
(defun impossible (area amts)
  (> (apply #'+ (mapcar #'* amts '(7 7 6 5 7 7)))
     (apply #'* area)))

(defun part-1 ()
  (let* ((input (uiop:read-file-lines "input.txt"))
         (problems (mapcar #'parse-line input)))
    (loop for p in problems
          ; Hope that every not-impossible problem is possible.
          count (not (apply #'invalid p)))))
