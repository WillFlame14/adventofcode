(defun parse-value(cpu x)
  (if (keywordp x) (getf cpu x) x))

(defun parse-loc (str)
  (let ((num (parse-integer str :junk-allowed t)))
    (if num
        num 
        (intern (string-upcase str) :keyword))))

(defun parse-inst (str)
  (let* ((parts (uiop:split-string str :separator " "))
         (first2 (list (intern (string-upcase (first parts)) :keyword)
                       (parse-loc (second parts)))))
    (if (= (length parts) 2)
        first2
        (let ((y (parse-loc (third parts))))
          (append first2 (list y))))))

(defun make-cpu () (list :a 7 :b 0 :c 0 :d 0))

(defun exec (cpu insts i)
  (if (>= i (length insts))
      cpu
      (destructuring-bind (cmd x &optional y) (aref insts i)
        (cond ((eq cmd :cpy)
               (progn (when (keywordp y)
                        (setf (getf cpu y) (parse-value cpu x)))
                      (exec cpu insts (1+ i))))
              ((eq cmd :inc)
               (progn (when (keywordp x)
                        (setf (getf cpu x) (1+ (getf cpu x))))
                      (exec cpu insts (1+ i))))
              ((eq cmd :dec)
               (progn (when (keywordp x)
                        (setf (getf cpu x) (1- (getf cpu x))))
                      (exec cpu insts (1+ i))))
              ((eq cmd :jnz)
               (if (zerop (parse-value cpu x))
                   (exec cpu insts (1+ i))
                   (exec cpu insts (+ (parse-value cpu y) i))))
              ((eq cmd :tgl)
               (let ((j (+ (parse-value cpu x) i)))
                 (if (or (< j 0) (>= j (length insts)))
                     (exec cpu insts (1+ i))
                     (let* ((inst (aref insts j))
                            (cmd (first inst)))
                       (cond ((eq cmd :inc)       (setf (first (aref insts j)) :dec))
                             ((= (length inst) 2) (setf (first (aref insts j)) :inc))
                             ((eq cmd :jnz)       (setf (first (aref insts j)) :cpy))
                             ((= (length inst) 3) (setf (first (aref insts j)) :jnz)))
                       (exec cpu insts (1+ i))))))))))

(defparameter *input* (uiop:read-file-lines "input.txt"))

(defparameter *insts* (make-array (length *input*) :initial-contents (mapcar #'parse-inst *input*)))

; Part 1. For part 2, change a to start at 12. The program takes a couple minutes to run.
(exec (make-cpu) *insts* 0)

