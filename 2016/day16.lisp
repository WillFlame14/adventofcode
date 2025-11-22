(defun dragon (a)
  (let ((b (toggle (reverse a))))
    (append a '(0) b)))

(defun toggle (a)
  (mapcar #'(lambda (x) (- 1 x)) a))

(defun inner-checksum (a acc)
  (if (null a)
      (reverse acc)
      (let ((i (if (= (first a) (second a)) 1 0)))
        (inner-checksum (cddr a) (cons i acc)))))

(defun checksum (a)
  (if (oddp (length a))
      a
      (checksum (inner-checksum a nil))))

(defun fill-disk (a len)
  (if (> (length a) len)
      (subseq a 0 len)
      (fill-disk (dragon a) len)))

(defun gen-checksum (s len)
  (let* ((digits (mapcar #'digit-char-p (coerce s 'list)))
         (disk (fill-disk digits len))
         (csum (checksum disk)))
    (format t "~{~a~}" csum)))
