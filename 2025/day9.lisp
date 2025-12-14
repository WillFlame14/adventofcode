(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-ppcre" :silent t)
  (load "util.lisp"))

(defun area (a b)
  (apply #'* (mapcar #'1+ (mapcar #'abs (mapcar #'- a b)))))

(defun part-1 ()
  (let* ((input (uiop:read-file-lines "input.txt"))
         (pts (mapcar #'util:parse-ints input)))
    (loop for p-rest on pts
          for p = (car p-rest)
          maximize (loop for q in (cdr pts)
                            maximize (area p q)))))

(defun xor (a b)
  (if a (not b) b))

;; Important observation:
;; Two edges going up/down doesn't toggle inside/outside.
;; These are always consecutive edges.
(defun get-relevant (sorted-walls y &optional (last-dir 0) (inside-before-ambig nil) (interval-start nil) (acc nil))
  (if (null sorted-walls)
      (reverse acc)
      (destructuring-bind (x y1 y2) (first sorted-walls)
        (let* ((curr-dir (cond ((= y y1) -1)
                               ((= y y2) 1)
                               (t 0)))
               (toggle (= -1 (* last-dir curr-dir)))
               (now-inside (or (null interval-start) ; outside -> inside for all walls
                               (if (zerop last-dir)
                                   (not (zerop curr-dir)) ; inside -> inside if hitting a first corner
                                   (xor inside-before-ambig toggle)))) ; toggled in or didn't toggle out
               (boundary? (xor interval-start now-inside))
               (new-ambig (and (zerop last-dir)
                               (not (zerop curr-dir))
                               interval-start))) ; whether inside or not
          (if boundary?
              (if interval-start
                  (get-relevant (rest sorted-walls) y curr-dir new-ambig nil (cons (list interval-start x) acc))
                  (get-relevant (rest sorted-walls) y curr-dir new-ambig x acc))
              (get-relevant (rest sorted-walls) y curr-dir new-ambig interval-start acc))))))

(defun fst< (a b)
  (< (first a) (first b)))

(defun fst> (a b)
  (> (first a) (first b)))

(defun make-intervals (walls min max)
  (let ((intervals (make-hash-table)))
    (loop for y from min to max
          for relevant-walls = (remove-if-not (lambda (w) (<= (second w) y (third w))) walls)
          for sorted-walls = (get-relevant (sort (copy-list relevant-walls) #'fst<) y)
          when sorted-walls
            do (setf (gethash y intervals) sorted-walls))
    intervals))

(defun split-walls (pts)
  (let* ((all-xs (mapcar #'first pts))
         (all-ys (mapcar #'second pts))
         (min-x (apply #'min all-xs))
         (max-x (apply #'max all-xs))
         (min-y (apply #'min all-ys))
         (max-y (apply #'max all-ys)))
    (loop for p-rest on pts
          for (x1 y1) = (car p-rest)
          for (x2 y2) = (or (cadr p-rest) (first pts))
          if (= x1 x2)
            collect `(,x1 ,(min y1 y2) ,(max y1 y2)) into y-walls
          else
            collect `(,y1 ,(min x1 x2) ,(max x1 x2)) into x-walls
          finally (return (list (make-intervals x-walls min-x max-x)
                                (make-intervals y-walls min-y max-y))))))

(defun inside? (a b1 b2 intervals)
  (let ((is (gethash a intervals)))
    (some (lambda (i) (<= (first i) (min b1 b2) (max b1 b2) (second i))) is)))

(defun rect-inside (a b intervals)
  (destructuring-bind ((x1 y1) (x2 y2) (x-intervals y-intervals)) `(,a ,b ,intervals)
    (and (inside? x1 y1 y2 x-intervals)
         (inside? x2 y1 y2 x-intervals)
         (inside? y1 x1 x2 y-intervals)
         (inside? y2 x1 x2 y-intervals))))

(defun part-2 ()
  (let* ((input (uiop:read-file-lines "input.txt"))
         (pts (mapcar #'util:parse-ints input))
         (walls (split-walls pts))
         (poss (loop for p-rest on pts
                     for p = (car p-rest)
                     nconcing (loop for q in (cdr pts)
                                    collect (list (area p q) p q))))
         (sorted-poss (sort poss #'fst>)))
    (loop for (a p q) in sorted-poss
          when (rect-inside p q walls)
            return a)))
