;; Parse each range into two pairs, (start t) and (end nil).
;; Create a "difference list" with all the pairs.
(defun parse-line (line)
  (let* ((parts (uiop:split-string line :separator "-"))
         (nums (mapcar #'parse-integer parts)))
    `((,(first nums) t) (,(second nums) nil))))

(defun min-ip (blacklist)
  (loop for r on blacklist
        for e = (first r)
        ; Stores the number of ranges that are currently excluding this ip.
        for x = (if (second e) 1 0) then (if (second e) (1+ x) (1- x))
        if (and
            (= x 0)
            (not (eq r blacklist))
            ; Doesn't count if the next pair starts a new range with the next number.
            (not (let ((next (cadr r)))
                   (= (first next) (1+ (first e))))))
          return (1+ (first e))))

(defun allowed-ips (blacklist)
  (loop for r on blacklist
        for e = (first r)
        for x = (if (second e) 1 0) then (if (second e) (1+ x) (1- x))
        if (and
            (= x 0)
            (not (null (cdr r)))
            (not (eq r blacklist))
            (not (let ((next (cadr r)))
                   (and (= (first next) (1+ (first e))) (second next)))))
          ; Add all the ips between this and the next pair.
          sum (- (first (cadr r)) (first e) 1)))

(let* ((lines (uiop:read-file-lines "input.txt"))
       (raw-blacklist (apply 'concatenate 'list (mapcar #'parse-line lines)))
       (blacklist (sort raw-blacklist (lambda (a b) (< (first a) (first b))))))
  ;; Part 1
  (format t "~a~%" (min-ip blacklist))
  ;; Part 2
  (format t "~a~%" (allowed-ips blacklist)))
