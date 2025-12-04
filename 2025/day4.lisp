(defparameter *input* (uiop:read-file-lines "input.txt"))

(defparameter *grid* (make-array (list (length *input*) (length (first *input*))) :initial-contents *input*))

(defun at (grid pos)
  (aref grid (second pos) (first pos)))

(defparameter *adj* '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)))

(defun pos-add (a b)
  (list (+ (first a) (first b))
        (+ (second a) (second b))))

(defun inbounds (grid pos)
  (destructuring-bind (max-y max-x) (array-dimensions grid)
    (destructuring-bind (y x) pos 
      (and (>= x 0) (< x max-x)
           (>= y 0) (< y max-y)))))

(defun accessible? (grid pos)
  (let ((paper-rolls (loop for d in *adj*
                           for dest = (pos-add pos d)
                           count (and (inbounds grid dest)
                                      (eq (at grid dest) #\@)))))
    (< paper-rolls 4)))

(defun part-1 ()
  (destructuring-bind (max-y max-x) (array-dimensions *grid*)
    (loop for y from 0 below max-y
          sum (loop for x from 0 below max-x
                    for pos = (list x y)
                    count (and (eq (at *grid* pos) #\@)
                               (accessible? *grid* pos))))))

(defun remove-roll (grid pos)
  (setf (aref grid (second pos) (first pos)) #\.))

(defun part-2 (&optional (ans 0))
  (destructuring-bind (max-y max-x) (array-dimensions *grid*)
    (let ((removable (loop for y from 0 below max-y
                           nconcing (loop for x from 0 below max-x
                                          for pos = (list x y)
                                          if (and (eq (at *grid* pos) #\@)
                                                  (accessible? *grid* pos))
                                            collect pos))))
      (if (null removable)
          ans
          (progn (loop for p in removable
                       do (remove-roll *grid* p))
                 (part-2 (+ ans (length removable))))))))
