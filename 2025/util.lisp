(defpackage :util
  (:use :cl)
  (:export #:parse-ints
           #:str-to-keyword
           #:rotate-left
           #:rotate-right
           #:remove-index
           #:insert-index
           #:gen-combinations
           #:copy-arr
           #:adj
           #:inbounds
           ))

(in-package :util)

(defun parse-ints (s)
  (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" s)))

(defun str-to-keyword (s)
  (intern (string-upcase s) :keyword))

(defun rotate-left (l n)
  (append (nthcdr n l) (butlast l (- (length l) n))))

(defun rotate-right (l n)
  (let ((rotations (- (length l) n)))
    (rotate-left l (if (< rotations 0) (+ rotations (length l)) rotations))))

(defun remove-index (l n)
  (if (= n 0) (cdr l) (cons (first l) (remove-index (cdr l) (1- n)))))

(defun insert-index (l n item)
  (if (= n 0) (cons item l) (cons (first l) (insert-index (cdr l) (1- n) item))))

(defun gen-permutations (l)
  (if (= (length l) 1)
      (list l)
      (loop for item in l
            nconcing (loop for nested-item in (gen-permutations (remove item l))
                          collect (cons item (copy-list nested-item))))))

(defun copy-arr (node-arr)
  (let* ((dims (array-dimensions node-arr))
         (new-arr (make-array dims)))
    (loop for y from 0 below (first dims)
          do (loop for x from 0 below (second dims)
                   do (setf (aref new-arr y x) (copy-list (aref node-arr y x)))))
    new-arr))

(defparameter adj '((-1 0) (1 0) (0 1) (0 -1)))

(defun inbounds (x y dims)
  (and (>= x 0) (< x (second dims))
       (>= y 0) (< y (first dims))))
