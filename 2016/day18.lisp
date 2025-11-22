(defun trap? (prev i)
  (and (>= i 0) (< i (length prev))
       (eq (elt prev i) #\^)))

(defun next-trap? (prev i)
  (destructuring-bind (l c r) (mapcar (lambda (dx) (trap? prev (+ i dx))) '(-1 0 1))
    (or (and l c (not r))
        (and c r (not l))
        (and l (not c) (not r))
        (and r (not l) (not c)))))

(defun next-line (prev)
  (loop for i from 0 below (length prev)
        collect (if (next-trap? prev i) #\^ #\.)))

(defun gen-map (initial rows)
  (loop for i from 0 below rows
        for line = initial then (next-line line)
        collect line))

(defun count-safe (trap-map)
  (loop for line in trap-map
        sum (loop for c in line count (eq c #\.))))

;; For part 2, change the # of rows to 400000.
(let* ((input (uiop:read-file-string "input.txt"))
       (trap-map (gen-map (butlast (coerce input 'list)) 40))) ; need to remove the newline
  (count-safe trap-map))
