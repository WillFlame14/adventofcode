;; An elf is a pair (index #-of-presents).
;; The elves sit in a circular loop.
(defun make-loop (size)
  (let* ((l (loop for i from 1 to size collect (list i 1)))
         (final (last l)))
    (setf (cdr final) l)
    (list l final)))

(defun take-left-present (setup)
  (destructuring-bind (circle final) setup
    (let* ((taker (first circle))
           (giver (second circle))
           (new-elf (cons (list (first taker) (+ (second taker) (second giver))) nil)))
      (setf (cdr final) new-elf)
      (setf (cdr new-elf) (cddr circle))
      (list (cdr new-elf) new-elf))))

(defun take-left-presents (size)
  (loop for s = (make-loop size) then (take-left-present s)
        when (eq (first s) (second s))
          return (caaar s)))

;; To avoid traversing from the beginning to the midpoint every time,
;; keep track of a known elf and its index in the current circle.
;; Then it's only necessary to seek from that elf to the midpoint.
(defun take-across-present (setup)
  (destructuring-bind (circle final size ptr index) setup
    (let* ((taker (first circle))
           (giver-index (floor size 2))
           (prev-giver (loop for i = index then (1+ i)
                             for p = ptr then (cdr p)
                             when (= i (1- giver-index))
                               return p))
           (giver (cadr prev-giver))
           (new-elf (cons (list (first taker) (+ (second taker) (second giver))) nil)))
      (setf (cdr final) new-elf)
      (setf (cdr prev-giver) (cddr prev-giver))
      (setf (cdr new-elf) (cdr circle))
      (list (cdr new-elf) new-elf (1- size) prev-giver (- giver-index 2)))))

(defun take-across-presents (size)
  (destructuring-bind (circle final) (make-loop size)
    (loop for s = (list circle final size circle 0) then (take-across-present s)
          when (= (third s) 1)
            return (caaar s))))

(let ((size 3005290))
  ;; Part 1
  (format t "~a~%" (take-left-presents size))
  ;; Part 2
  (format t "~a~%" (take-across-presents size)))
