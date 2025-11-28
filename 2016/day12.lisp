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

(defun make-cpu () (list :a 0 :b 0 :c 0 :d 0))

(defun exec (cpu insts i)
  (if (>= i (length insts))
      cpu
      (destructuring-bind (cmd x &optional y) (aref insts i)
        (cond ((eq cmd :cpy)
               (progn (setf (getf cpu y) (parse-value cpu x))
                      (exec cpu insts (1+ i))))
              ((eq cmd :inc)
               (progn (setf (getf cpu x) (1+ (getf cpu x)))
                      (exec cpu insts (1+ i))))
              ((eq cmd :dec)
               (progn (setf (getf cpu x) (1- (getf cpu x)))
                      (exec cpu insts (1+ i))))
              ((eq cmd :jnz)
               (if (zerop (parse-value cpu x))
                   (exec cpu insts (1+ i))
                   (exec cpu insts (+ (parse-value cpu y) i))))))))

(defparameter *input* (uiop:read-file-lines "input.txt"))

(defparameter *insts* (make-array (length *input*) :initial-contents (mapcar #'parse-inst *input*)))

; Part 1. For part 2, change c to start at 1.
(exec (make-cpu) *insts* 0)

