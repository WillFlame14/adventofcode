(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-ppcre" :silent t))

(defun parse-ints (s)
  (mapcar #'parse-integer (ppcre:all-matches-as-strings "-?\\d+" s)))

(defun parse-line (s)
  (let ((on? (string= (subseq s 0 2) "on")))
    (cons on? (parse-ints s))))

(defun make-cube-dims (coords)
  (loop for (_ x1 x2 y1 y2 z1 z2) in coords
        nconcing (list (1- x1) x1 (1+ x1) (1- x2) x2 (1+ x2)) into xs
        nconcing (list (1- y1) y1 (1+ y1) (1- y2) y2 (1+ y2)) into ys
        nconcing (list (1- z1) z1 (1+ z1) (1- z2) z2 (1+ z2)) into zs
        finally (return
                  (mapcar (lambda (es)
                            (let ((list (sort (remove-duplicates es) #'<)))
                              (make-array (length list) :initial-contents list)))
                          (list xs ys zs)))))

(defun toggle (cubes cube-dims coords)
  (destructuring-bind ((xs ys zs) (on? x1 x2 y1 y2 z1 z2)) (list cube-dims coords)
    (loop for x from (position x1 xs) to (position x2 xs)
          do (loop for y from (position y1 ys) to (position y2 ys)
                   do (loop for z from (position z1 zs) to (position z2 zs)
                            do (setf (aref cubes x y z) (if on? 1 0)))))))

(defun on-count (cubes cube-dims)
  (destructuring-bind ((xs ys zs) (x-dim y-dim z-dim)) (list cube-dims (array-dimensions cubes))
    (loop for x from 1 below (1- x-dim)
          summing (loop for y from 1 below (1- y-dim)
                        summing (loop for z from 1 below (1- z-dim)
                                      for on? = (= (aref cubes x y z) 1)
                                      if on?
                                        summing (* (- (aref xs (1+ x)) (aref xs x))
                                                   (- (aref ys (1+ y)) (aref ys y))
                                                   (- (aref zs (1+ z)) (aref zs z))))))))

(defun part-1 ()
  (let* ((input (uiop:read-file-lines "input.txt"))
         (coords (mapcar #'parse-line (subseq input 0 20)))
         (cube-dims (make-cube-dims coords))
         (cubes (make-array (mapcar #'length cube-dims) :initial-element 0 :element-type 'bit)))
    (loop for c in coords
          do (toggle cubes cube-dims c)
          finally (return (on-count cubes cube-dims)))))

; 9.5 min runtime, but it works.
(defun part-2 ()
  (let* ((input (uiop:read-file-lines "input.txt"))
         (coords (mapcar #'parse-line input))
         (cube-dims (make-cube-dims coords))
         (cubes (make-array (mapcar #'length cube-dims) :initial-element 0 :element-type 'bit)))
    (loop for c in coords
          do (toggle cubes cube-dims c)
          finally (return (on-count cubes cube-dims)))))
