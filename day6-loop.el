;;; day6-loop --- My solution to day6-loop -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day6-loop

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defun day6-put-matrix (value x y matrix)
  "Set the slot to VALUE at X Y in MATRIX."
  (aset (aref matrix x) y value))

(defun day6-get-matrix (x y matrix)
  "Get the value at X, Y in MATRIX."
  (aref (aref matrix x) y))

(defun day6-loop-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (cl-loop
     with light-grid = (apply #'vector (cl-loop
                                          for i from 0 to 999
                                          collect (make-vector 1000 nil)))
     for instruction in (mapcar (lambda (line) (split-string line " " t " "))
                                (split-string input-file "\n" t " "))
     if (string-equal (car instruction) "toggle")
       do (pcase instruction
            (`(,_ ,coord-start ,_ ,coord-end)
              (pcase `(,(mapcar #'string-to-number (split-string coord-start ","))
                        ,(mapcar #'string-to-number (split-string coord-end ",")))
                (`((,x1 ,y1) (,x2 ,y2))
                  (cl-loop
                     for i from x1 to x2
                     do (cl-loop
                           for j from y1 to y2
                           for current-state = (day6-get-matrix i j light-grid)
                           if current-state
                             do (day6-put-matrix nil i j light-grid)
                           else
                             do (day6-put-matrix t i j light-grid)))))))
     else
       do (let ((newState (string-equal (cadr instruction) "on")))
            (pcase instruction
              (`(,_ ,_ ,coord-start ,_ ,coord-end)
                (pcase `(,(mapcar #'string-to-number (split-string coord-start ","))
                          ,(mapcar #'string-to-number (split-string coord-end ",")))
                  (`((,x1 ,y1) (,x2 ,y2))
                    (cl-loop
                       for i from x1 to x2
                       do (cl-loop
                             for j from y1 to y2
                             do (day6-put-matrix  newState i j light-grid))))))))
     finally return (cl-loop
                       for i from 0 to 999
                       sum (cl-loop
                              for j from 0 to 999
                              count (day6-get-matrix i j light-grid)))))

;; # PART 2:

(defun day6-inc-matrix (value x y matrix)
  "Increment the slot by VALUE at X Y in MATRIX."
  (let* ((new-brightness (+ value (day6-get-matrix x y matrix)))
         (brightness     (if (< new-brightness 0) 0 new-brightness)))
    (aset (aref matrix x) y brightness)))

(defun day6-loop-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (cl-loop
     with light-grid = (apply #'vector (cl-loop
                                          for i from 0 to 999
                                          collect (make-vector 1000 0)))
     for instruction in (mapcar (lambda (line) (split-string line " " t " "))
                                (split-string input-file "\n" t " "))
     if (string-equal (car instruction) "toggle")
       do (pcase instruction
            (`(,_ ,coord-start ,_ ,coord-end)
              (pcase `(,(mapcar #'string-to-number (split-string coord-start ","))
                        ,(mapcar #'string-to-number (split-string coord-end ",")))
                (`((,x1 ,y1) (,x2 ,y2))
                  (cl-loop
                     for i from x1 to x2
                     do (cl-loop
                           for j from y1 to y2
                           do (day6-inc-matrix 2 i j light-grid)))))))
     else
       do (let ((newState (if (string-equal (cadr instruction) "on") 1 -1)))
            (pcase instruction
              (`(,_ ,_ ,coord-start ,_ ,coord-end)
                (pcase `(,(mapcar #'string-to-number (split-string coord-start ","))
                          ,(mapcar #'string-to-number (split-string coord-end ",")))
                  (`((,x1 ,y1) (,x2 ,y2))
                    (cl-loop
                       for i from x1 to x2
                       do (cl-loop
                             for j from y1 to y2
                             do (day6-inc-matrix newState i j light-grid))))))))
     finally return (cl-loop
                       for i from 0 to 999
                       sum (cl-loop
                              for j from 0 to 999
                              sum (day6-get-matrix i j light-grid)))))

;; Run the solution:

(progn
  (message "\n********** OUTPUT **********")
  (let ((input-1 (with-temp-buffer
                   (find-file-literally "day6-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max))))
        (input-2 (with-temp-buffer
                   (find-file-literally "day6-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max)))))
    (message "Part 1: %s" (day6-loop-part-1 input-1))
    (message "Part 2: %s\n" (day6-loop-part-2 input-2))))

(provide 'day6-loop)
;;; day6-loop ends here
