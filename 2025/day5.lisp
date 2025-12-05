(defun parse-range (s)
  (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" s)))

(defparameter *input* (uiop:read-file-lines "input.txt"))

(defparameter *ranges*
  (loop for l in *input*
        collect (parse-range l) into rs
        if (zerop (length l))
          return (butlast rs)))

(defun in-range (i p)
  (<= (first p) i (second p)))

(defun part-1 ()
  (let* ((split (position-if (lambda (s) (zerop (length s))) *input*))
         (ranges (mapcar #'parse-range (subseq *input* 0 split)))
         (nums (mapcar #'parse-integer (subseq *input* (1+ split)))))
    (loop for n in nums
          count (some (lambda (p) (in-range n p)) ranges))))

(defun part-2 ()
  (let* ((split (position-if (lambda (s) (zerop (length s))) *input*))
         (ranges (mapcar #'parse-range (subseq *input* 0 split)))
         (combined (loop for r in ranges
                         nconcing `((,(first r) t) (,(second r) nil))))
         (sorted (sort combined #'< :key #'car)))
    (loop for rest on sorted
          for (i toggle) = (first rest)
          ; Tracks the start of the current fresh interval.
          ; Resets when acc was previously 0.
          for start = i then (if (zerop acc) i start)
          ; Tracks how deeply nested we are inside fresh intervals.
          for acc = 1 then (+ acc (if toggle 1 -1))
          if (zerop acc)
            ; If we ended an interval and the next one starts on the same number,
            ; don't count 1 extra.
            sum (- i start (if (= (caadr rest) i) 0 -1)))))
