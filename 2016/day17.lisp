(defun hash-md5 (s)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :md5 (ironclad:ascii-string-to-byte-array s))))

(defun door-open (c)
  (not (or (digit-char-p c) (char= #\a c))))

(defparameter directions '(:up :down :left :right))

(defun dir-to-char (dir)
  (cond ((eq dir :up) #\U)
        ((eq dir :right) #\R)
        ((eq dir :down) #\D)
        ((eq dir :left) #\L)))

(defun travel (loc dir)
  (destructuring-bind (x y) loc
    (cond ((eq dir :up)    (list x (1- y)))
          ((eq dir :right) (list (1+ x) y))
          ((eq dir :down)  (list x (1+ y)))
          ((eq dir :left)  (list (1- x) y)))))

(defun inbounds (loc)
  (destructuring-bind (x y) loc
    (and (>= x 0) (< x 4)
         (>= y 0) (< y 4))))

(defun open-doors (code)
  (let ((hash (hash-md5 code)))
    (loop for dir in directions
          for i from 0 to 3
          if (door-open (char hash i))
            collect dir)))

(defun neighbours (code loc)
  (let ((doors (open-doors code)))
    (loop for dir in doors
          for dest = (travel loc dir)
          if (inbounds dest)
            collect (list (format nil "~a~a" code (dir-to-char dir)) dest))))

(defun traverse (pairs)
  (if (null pairs)
      (format t "Failed to traverse")
      (let ((res (loop for (code loc) in pairs
                       for ns = (neighbours code loc)
                       for complete = (find-if (lambda (x) (equal (second x) '(3 3))) ns)
                       if complete
                         return (list :complete complete)
                       else
                         collect ns)))
        (if (eq (first res) :complete)
            (subseq (caadr res) 8)
            (traverse (apply 'concatenate 'list res))))))

(defun longest (code loc)
  (if (equal loc '(3 3))
      (length (subseq code 8))
      (loop for (c l) in (neighbours code loc)
            maximize (longest c l))))

;; Part 1
(traverse '(("udskfozm" (0 0))))

;; Part 2
(longest "udskfozm" '(0 0))
