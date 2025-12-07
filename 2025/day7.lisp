(defun advance (grid i beams)
  (let ((line (nth i grid)))
    (loop for b in beams
          for split = (eq (nth b line) #\^)
          if split
            sum 1 into splits
          nconcing (if split (list (1- b) (1+ b)) (list b)) into new-beams
          finally (return (list splits (remove-duplicates new-beams))))))

(defun part-1 ()
  (let* ((input (uiop:read-file-lines "input.txt"))
         (grid (mapcar (lambda (s) (coerce s 'list)) input))
         (initial (list (position #\S (first grid)))))
    (loop for i from 0 below (length grid)
          for (splits new-beams) = (advance grid i initial) then (advance grid i new-beams)
          sum splits)))

(defun update-or (table key f default)
  (setf (gethash key table) (funcall f (gethash key table default))))

(defun freqs (list)
  (let ((table (make-hash-table)))
    (loop for l in list
          do (update-or table l #'1+ 0)
          finally (return table))))

(defun advance-2 (grid i freqs)
  (let ((line (nth i grid))
        (new-beams (make-hash-table)))
    (loop for k being the hash-key using (hash-value v) of freqs
          do (if (eq (nth k line) #\^)
                 (progn (update-or new-beams (1- k) (lambda (x) (+ x v)) 0)
                        (update-or new-beams (1+ k) (lambda (x) (+ x v)) 0))
                 (update-or new-beams k (lambda (x) (+ x v)) 0))
          finally (return new-beams))))

(defun part-2 ()
  (let* ((input (uiop:read-file-lines "input.txt"))
         (grid (mapcar (lambda (s) (coerce s 'list)) input))
         (initial (let ((table (make-hash-table)))
                    (setf (gethash (position #\S (first grid)) table) 1)
                    table)))
    (loop for i from 0 below (length grid)
          for new-beams = (advance-2 grid i initial) then (advance-2 grid i new-beams)
          finally (return (loop for v being the hash-value of new-beams sum v)))))

