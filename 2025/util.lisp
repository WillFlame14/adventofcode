(defpackage :util
  (:use :cl)
  (:export #:parse-ints
           #:transpose
           #:str-to-keyword
           #:rotate-left
           #:rotate-right
           #:remove-index
           #:insert-index
           #:gen-combinations
           #:copy-arr
           #:adj
           #:inbounds
           #:when-let
           #:if-let
           #:multf
           #:divf))

(in-package :util)

(defun parse-ints (s)
  (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" s)))

(defun transpose (m)
  (apply #'mapcar #'list m))

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

(defun copy-arr (arr)
  (let* ((dims (array-dimensions arr))
         (new-arr (make-array dims)))
    (loop for y from 0 below (first dims)
          do (loop for x from 0 below (second dims)
                   do (setf (aref new-arr y x) (aref arr y x))))
    new-arr))

(defparameter adj '((-1 0) (1 0) (0 1) (0 -1)))

(defun inbounds (x y dims)
  (and (>= x 0) (< x (second dims))
       (>= y 0) (< y (first dims))))

(defun prime-factorize (x &optional (acc nil) (last-prime 2))
  (let ((factor (loop for i from last-prime below x
                      if (zerop (mod x i))
                        return i)))
    (if factor
        (prime-factorize (/ x factor) (cons factor acc) factor)
        (cons x acc))))

(defun frequencies (l)
  (let ((table (make-hash-table :test #'equal)))
    (loop for e in l
          do (setf (gethash e table) (1+ (or (gethash e table) 0)))
          finally (return table))))

(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

(defun print-hash-table (table)
  (with-output-to-string (s)
    (maphash (lambda (k v) (format s "~a: ~a, " k v)) table)))

(defmacro when-let ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro if-let ((var expr) then else)
  `(let ((,var ,expr))
     (if ,var ,then ,else)))

(defmacro multf (place val)
  `(setf ,place (* ,place ,val)))

(defmacro divf (place val)
  `(setf ,place (/ ,place ,val)))
