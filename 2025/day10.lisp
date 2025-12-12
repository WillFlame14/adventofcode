(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-ppcre" :silent t)
  (ql:quickload "cl-heap" :silent t)
  (ql:quickload "cl-containers" :silent t)
  (load "util.lisp"))

(defpackage :aoc2025-10
  (:use :cl :util))

(in-package :aoc2025-10)

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
     (mapcar #'parse-ints (uiop:split-string buttons :separator " "))
     (parse-ints joltage))))

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
  (let ((matrix (transpose
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
        when lead-pos
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
        if (not (some-arr (lambda (row) (when-let (lead-pos (get-lead-pos row)) (= lead-pos i))) matrix))
          collect i))

(defun swap-rows (matrix a b)
  (let ((row-b (loop for i from 0 below (array-dimension matrix 1) collect (aref matrix b i))))
    (loop for i from 0 below (array-dimension matrix 1)
          do (setf (aref matrix b i) (aref matrix a i)))
    (loop for i from 0 below (array-dimension matrix 1)
          do (setf (aref matrix a i) (nth i row-b)))))

(defun normalize-lead (matrix constants i)
  (when-let (lead-pos (get-lead-pos (array-slice matrix i)))
    (let ((lead-digit (aref matrix i lead-pos)))
      (when (not (= lead-digit 1))
        (loop for j from lead-pos below (array-dimension matrix 1)
              do (multf (aref matrix i j) (/ 1 lead-digit)))
        (multf (nth i constants) (/ 1 lead-digit))))
    (list matrix constants)))

(defun get-lead (matrix constants i)
  (if-let (lead-pos (position-if-arr (lambda (row row-i) (and (>= row-i i) (when-let (pos (get-lead-pos row)) (= i pos)))) matrix))
    (progn (when (not (= lead-pos i))
             (swap-rows matrix lead-pos i)
             (rotatef (nth lead-pos constants) (nth i constants)))
           (normalize-lead matrix constants i))
    (normalize-lead matrix constants i))
  (list matrix constants))

(defun clear-lead (matrix constants i)
  (let ((lead-row (array-slice matrix i)))
    (loop for j from (1+ i) below (array-dimension matrix 0)
          for row = (array-slice matrix j)
          for corr-digit = (aref row i)
          do (when (not (zerop corr-digit))
               (loop for k from 0 below (array-dimension matrix 1)
                     do (decf (aref matrix j k) (* corr-digit (aref lead-row k))))
               (decf (nth j constants) (* (nth i constants) corr-digit)))
          finally (return (list matrix constants)))))

(defun rref (matrix constants)
  (loop for i from 0 below (apply #'min (array-dimensions matrix))
        do (progn (get-lead matrix constants i)
                  (clear-lead matrix constants i))
        finally (return (list matrix constants))))

(defun copy-matrix (matrix)
  (mapcar #'copy-list matrix))

(defun impossible (free best)
  (> (apply #'+ free) best))

(defun min-solve (matrix constants free max-presses frontier visited best)
  (cond ((= (cl-containers:size frontier) 0) best)
        ((impossible (cl-containers:first-element frontier) best)
         (progn
           (cl-containers:dequeue frontier)
           (min-solve matrix constants free max-presses frontier visited best)))
        (t (let* ((curr-free (cl-containers:dequeue frontier))
                  (curr-free-sol (loop for i from 0 below (array-dimension matrix 1)
                                       for free-pos = (position i free)
                                       collect (if free-pos (nth free-pos curr-free) nil)))
                  (curr-sol (solve matrix constants curr-free-sol))
                  (sum (apply #'+ curr-sol)))
             (loop for i from 0 below (length curr-free)
                   for n = (loop for j from 0 below (length curr-free)
                                 for e in curr-free
                                 collect (if (= i j) (1+ e) e))
                   when (not (or (gethash (hash n) visited)
                                 (impossible n best)
                                 (loop for i in free
                                       for p in n
                                       for m = (nth i max-presses) thereis (> p m))))
                        do (setf (gethash (hash n) visited) t)
                           (cl-containers:enqueue frontier n))
             (if (and (every #'integerp curr-sol) (every (lambda (s) (>= s 0)) curr-sol) (< sum best))
                 (min-solve matrix constants free max-presses frontier visited sum)
                 (min-solve matrix constants free max-presses frontier visited best))))))

(defun max-presses (button goal)
  (loop for i in button
        minimize (nth i goal)))

(defun hash (free)
  (+ (or (first free) 0)
     (* (or (second free) 0) 300)
     (* (or (third free) 0) 90000)))

(defun find-sol (buttons joltage)
  (let* ((matrix (make-matrix joltage buttons))
         (reduced (rref (copy-arr matrix) (copy-list joltage)))
         (free (free-vars (first reduced)))
         (max-presses (mapcar (lambda (b) (max-presses b joltage)) buttons))
         (initial-state (make-list (length free) :initial-element 0))
         (frontier (let ((queue (cl-containers:make-container 'cl-containers:basic-queue)))
                     (cl-containers:enqueue queue initial-state)))
         (visited (let ((table (make-hash-table)))
                    (setf (gethash (hash initial-state) table) t)
                    table)))
    (min-solve (first reduced) (second reduced) free max-presses frontier visited (apply #'+ joltage))))

(defun part-2 ()
  (let* ((input (uiop:read-file-lines "input.txt"))
         (problems (mapcar #'parse-line input)))
    (loop for (goal buttons joltage) in problems
          for i from 0 below (length problems)
          for sol = (find-sol buttons joltage)
          do (format t "Done ~a! ~a~%" i sol)
          sum sol)))
