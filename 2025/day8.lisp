(defun distance (a b)
  (sqrt (loop for e in (mapcar #'- a b)
              sum (* e e))))

(defun sort-pairs (nodes)
  (let ((pairs (loop for rest-nodes on nodes
                     for n = (first rest-nodes)
                     nconcing (loop for m in (rest rest-nodes)
                                    collect (list n m (distance n m))))))
    (sort pairs (lambda (a b) (< (third a) (third b))))))

(defun connect (groups a b)
  (let ((a-group (position-if (lambda (g) (member a g)) groups))
        (b-group (position-if (lambda (g) (member b g)) groups)))
    (cond
      ((and (null a-group) (null b-group))
       (cons (list a b) groups))
      ((null a-group)
       (progn
         (setf (nth b-group groups) (cons a (nth b-group groups)))
         groups))
      ((null b-group)
       (progn
         (setf (nth a-group groups) (cons b (nth a-group groups)))
         groups))
      ((eq a-group b-group) groups)
      (t (progn
           (setf (nth a-group groups) (append (nth a-group groups) (nth b-group groups)))
           (util:remove-index groups b-group))))))

(defun part-1 ()
  (let* ((input (uiop:read-file-lines "input.txt"))
         (nodes (mapcar #'util:parse-ints input))
         (sorted-pairs (sort-pairs nodes))
         (conns (loop for i from 0 below 1000
                      for (a b _) in sorted-pairs
                      for acc = (connect '() a b) then (connect acc a b)
                      finally (return acc))))
    (apply #'* (subseq (sort (mapcar #'length conns) #'>) 0 3))))

(defun part-2 ()
  (let* ((input (uiop:read-file-lines "input.txt"))
         (nodes (mapcar #'util:parse-ints input))
         (sorted-pairs (sort-pairs nodes)))
    (loop for (a b _) in sorted-pairs
          for acc = (connect '() a b) then (connect acc a b)
          when (and (= (length acc) 1) (= (length (first acc)) (length nodes)))
            return (* (first a) (first b)))))
