(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-ppcre" :silent t)
  (ql:quickload "cl-heap" :silent t)
  (ql:quickload "cl-containers" :silent t)
  (load "util.lisp"))

(defun press (state button)
  (let ((new-s (copy-list state)))
    (loop for i in button
          do (setf (nth i new-s) (not (nth i new-s))))
    new-s))

(defun parse-line (s)
  (ppcre:register-groups-bind (goal buttons joltage)
      ("\\[(.*)\\] (\\(.*\\))+ {(.*)}" s)
    (list
     (mapcar (lambda (c) (eq c #\#)) (coerce goal 'list))
     (mapcar #'util:parse-ints (uiop:split-string buttons :separator " "))
     (util:parse-ints joltage))))

(defun fst-equal (a b)
  (equal (first a) (first b)))

(defun attempt (frontier goal buttons)
  (if (null frontier)
      (format t "failed!")
      (destructuring-bind (state path) (first frontier)
        (let* ((neighbours (loop for b in buttons
                                collect (list (press state b) (cons b path))))
               (filtered (remove-if (lambda (n) (some (lambda (f) (fst-equal n f)) frontier)) neighbours))
               (sol (second (find-if (lambda (n) (equal (first n) goal)) filtered))))
          (or sol (attempt (append (rest frontier) filtered) goal buttons))))))

(defun part-1 ()
  (let* ((input (uiop:read-file-lines "input.txt")))
    (loop for l in input
          for (goal buttons joltage) = (parse-line l)
          sum (length (attempt `((,(make-list (length goal) :initial-element nil) nil)) goal buttons)))))

(defun make-matrix (goal buttons)
  (let ((matrix (util:transpose
                 (loop for b in buttons
                       collect (loop for i from 0 below (length goal)
                                     collect (if (member i b) 1 0))))))
    (make-array (list (length matrix) (length (first matrix))) :initial-contents matrix)))

(defun get-lead-pos (row)
  (position-if-not #'zerop row))

(defun array-slice (arr row)
  (make-array (array-dimension arr 1)
              :displaced-to arr
              :displaced-index-offset (* row (array-dimension arr 1))))

(defun some-arr (predicate arr)
  (loop for i from 0 below (array-dimension arr 0)
        thereis (funcall predicate (array-slice arr i))))

(defun position-if-arr (predicate arr)
  (loop for i from 0 below (array-dimension arr 0)
        if (funcall predicate (array-slice arr i) i)
          return i))

(defun solve (matrix constants &optional (sol (make-list (length constants) :initial-element nil)))
  (loop for row-i from (1- (array-dimension matrix 0)) downto 0
        for row = (array-slice matrix row-i)
        for lead-pos = (get-lead-pos row)
        if lead-pos
          do (let* ((addend (loop for j from (1+ lead-pos) below (length row)
                                  for coeff = (aref row j)
                                  if (not (zerop coeff))
                                    sum (* coeff (nth j sol))))
                    (ans (- (nth row-i constants) addend)))
               (if (and (nth lead-pos sol) (not (= (nth lead-pos sol) ans)))
                   (setf (nth lead-pos sol) -99)
                   (setf (nth lead-pos sol) ans)))
        finally (return sol)))

(defun free-vars (matrix)
  (loop for i from 0 below (array-dimension matrix 1)
        if (not (some-arr (lambda (row) (and (get-lead-pos row) (= (get-lead-pos row) i))) matrix))
          collect i))

(defun swap-rows (matrix a b)
  (let ((row-b (loop for i from 0 below (array-dimension matrix 1) collect (aref matrix b i))))
    (loop for i from 0 below (array-dimension matrix 1)
          do (setf (aref matrix b i) (aref matrix a i)))
    (loop for i from 0 below (array-dimension matrix 1)
          do (setf (aref matrix a i) (nth i row-b)))))

(defun normalize-lead (matrix constants i)
  (let ((lead-pos (get-lead-pos (array-slice matrix i))))
    (when lead-pos
      (let ((lead-digit (aref matrix i lead-pos)))
        (when (not (= lead-digit 1))
          (loop for j from lead-pos below (array-dimension matrix 1)
                do (setf (aref matrix i j) (* (/ 1 lead-digit) (aref matrix i j))))
          (setf (nth i constants) (* (/ 1 lead-digit) (nth i constants))))))
    (list matrix constants)))

(defun get-lead (matrix constants i)
  (let ((lead-pos (position-if-arr (lambda (row row-i) (and (>= row-i i) (get-lead-pos row) (= i (get-lead-pos row)))) matrix)))
    (if (null lead-pos)
        (normalize-lead matrix constants i)
        (progn
          (when (not (= lead-pos i))
            (swap-rows matrix lead-pos i)
            (rotatef (nth lead-pos constants) (nth i constants)))
          (normalize-lead matrix constants i)))
    (list matrix constants)))

(defun clear-lead (matrix constants i)
  (let ((lead-row (array-slice matrix i)))
    (loop for j from (1+ i) below (array-dimension matrix 0)
          for row = (array-slice matrix j)
          for corr-digit = (aref row i)
          do (when (not (zerop corr-digit))
               (loop for k from 0 below (array-dimension matrix 1)
                     do (setf (aref matrix j k) (- (aref matrix j k) (* corr-digit (aref lead-row k)))))
               (setf (nth j constants) (- (nth j constants) (* (nth i constants) corr-digit))))
          finally (return (list matrix constants)))))

(defun rref (matrix constants)
  (loop for i from 0 below (apply #'min (array-dimensions matrix))
        do (progn (get-lead matrix constants i)
                  (clear-lead matrix constants i))
        finally (return (list matrix constants))))

(defun copy-matrix (matrix)
  (mapcar #'copy-list matrix))

(defun min-solve (matrix constants frontier visited best)
  (cond ((= (cl-containers:size frontier) 0) best)
        ((> (apply #'+ (mapcar #'second (cl-containers:first-element frontier))) best)
         (progn
           (cl-containers:dequeue frontier)
           (min-solve matrix constants frontier visited best)))
        (t (let* ((curr-free (cl-containers:dequeue frontier))
                  (curr-free-sol (loop for i from 0 below (array-dimension matrix 1)
                                       collect (second (find-if (lambda (f) (= i (first f))) curr-free))))
                  (curr-sol (solve matrix constants curr-free-sol))
                  (sum (apply #'+ curr-sol)))
             (loop for i from 0 below (length curr-free)
                   for n = (let ((copy (copy-matrix curr-free)))
                             (setf (nth 1 (nth i copy)) (1+ (nth 1 (nth i copy))))
                             copy)
                   do (when (not (gethash (hash n) visited))
                        (setf (gethash (hash n) visited) t)
                        (cl-containers:enqueue frontier n)))
             (if (and (every #'integerp curr-sol) (every (lambda (s) (>= s 0)) curr-sol) (< sum best))
                 (min-solve matrix constants frontier visited sum)
                 (min-solve matrix constants frontier visited best))))))

(defun hash (free)
  (+ (or (cadar free) 0)
     (* (or (cadadr free) 0) 300)
     (* (or (car (cdaddr free)) 0) 90000)))

(defun find-sol (buttons joltage)
  (let* ((matrix (make-matrix joltage buttons))
         (reduced (rref (util:copy-arr matrix) (copy-list joltage)))
         (free (free-vars (first reduced)))
         (initial-state (mapcar (lambda (f) (list f 0)) free))
         (frontier (let ((queue (cl-containers:make-container 'cl-containers:basic-queue)))
                     (cl-containers:enqueue queue initial-state)))
         (visited (let ((table (make-hash-table)))
                    (setf (gethash (hash initial-state) table) t)
                    table)))
    (min-solve (first reduced) (second reduced) frontier visited (apply #'+ joltage))))

(defun part-2 ()
  (let* ((input (uiop:read-file-lines "input.txt"))
         (problems (mapcar #'parse-line input)))
    (loop for (goal buttons joltage) in problems
          for i from 0 to 99999
          for sol = (find-sol buttons joltage)
          do (format t "Done ~a! ~a~%" i sol)
          sum sol)))
