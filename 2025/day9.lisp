(defun area (a b)
  (apply #'* (mapcar #'1+ (mapcar #'abs (mapcar #'- a b)))))

(defun part-1 ()
  (let* ((input (uiop:read-file-lines "input.txt"))
         (pts (mapcar #'util:parse-ints input)))
    (loop for p-rest on pts
          for p = (car p-rest)
          maximize (loop for q in (cdr pts)
                            maximize (area p q)))))

(defun pairs (l &optional (acc nil))
  (if (< (length l) 2)
      (reverse acc)
      (pairs (cddr l) (cons (list (first l) (second l)) acc))))

;; Two edges going up/down doesn't toggle inside/outside.
;; These are always consecutive edges.
(defun get-relevant (sorted-walls y &optional (last-dir 0) (acc nil))
  (if (null sorted-walls)
      (reverse acc)
      (destructuring-bind (_ y1 y2) (first sorted-walls)
        (let ((curr-dir (cond ((= y y1) -1)
                              ((= y y2) 1)
                              (t 0))))
          (if (= -1 (* last-dir curr-dir))
              (get-relevant (rest sorted-walls) y 0 acc)
              (get-relevant (rest sorted-walls) y curr-dir (cons (first sorted-walls) acc)))))))

(defun fst< (a b)
  (< (first a) (first b)))

(defun fst> (a b)
  (> (first a) (first b)))

(defun split-walls (pts)
  (let* ((all-ys (mapcar #'second pts))
         (min-y (apply #'min all-ys))
         (max-y (apply #'max all-ys))
         (wall-table (make-hash-table :test #'equal))
         (y-wall-table (make-hash-table))
         (walls (loop for p-rest on pts
                      for (x1 y1) = (car p-rest)
                      for (x2 y2) = (or (cadr p-rest) (first pts))
                      if (= x1 x2)
                        collect `(,x1 ,(min y1 y2) ,(max y1 y2)) into y-walls
                      else
                        collect `(,y1 ,(min x1 x2) ,(max x1 x2)) into x-walls
                      finally (return (list x-walls y-walls)))))
    (destructuring-bind (x-walls y-walls) walls
      (loop for y from min-y to max-y
            for relevant-y-walls = (remove-if-not (lambda (w) (<= (second w) y (third w))) y-walls)
            for sorted-y-walls = (mapcar #'first (get-relevant (sort (copy-list relevant-y-walls) #'fst<) y))
            do (setf (gethash y y-wall-table)
                       (append (pairs sorted-y-walls) (gethash y y-wall-table))))
      (loop for (x y1 y2) in y-walls
            do (loop for y from y1 to y2
                     for p = `(,x ,y)
                     do (setf (gethash p wall-table) t)))
      (loop for (y x1 x2) in x-walls
            do (loop for x from x1 to x2
                     for p = `(,x ,y)
                     do (setf (gethash p wall-table) t)))
      (list wall-table y-wall-table))))

(defun inside? (pt walls)
  (destructuring-bind (all-walls y-walls) walls
    (or (gethash pt all-walls)
        (let ((ws (gethash (second pt) y-walls)))
          (some (lambda (w) (<= (first w) (first pt) (second w))) ws)))))

(defun rect-inside (a b walls)
  (destructuring-bind (x1 y1) a
    (destructuring-bind (x2 y2) b
      (and (loop for x from (min x1 x2) to (max x1 x2)
                 always (and (inside? `(,x ,y1) walls)
                             (inside? `(,x ,y2) walls)))
           (loop for y from (min y1 y2) to (max y1 y2)
                 always (and (inside? `(,x1 ,y) walls)
                             (inside? `(,x2 ,y) walls)))))))

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
          if (rect-inside p q walls)
            return a)))
