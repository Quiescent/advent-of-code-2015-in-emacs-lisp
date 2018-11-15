;;; day1-loop --- My solution to day1-loop -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day1-loop

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defun day1-loop-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (cl-loop with floor = 0
     for bracket across input-file
     if (eq bracket ?\()
       do (cl-incf floor)
     else
       do (cl-decf floor)
     finally return floor))

;; # PART 2:

(defun day1-loop-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (cl-loop with floor = 0
     for bracket being the elements of input-file using (index i)
     if (eq bracket ?\()
       do (cl-incf floor)
     else
       do (cl-decf floor)
     until (eq floor -1)
     finally return (1+ i)))

;; Run the solution:

(progn
  (message "\n********** OUTPUT **********")
  (let ((input-1 (with-temp-buffer
                   (find-file-literally "day1-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max))))
        (input-2 (with-temp-buffer
                   (find-file-literally "day1-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max)))))
    (message "Part 1: %s" (day1-loop-part-1 input-1))
    (message "Part 2: %s\n" (day1-loop-part-2 input-2))))

(provide 'day1-loop)
;;; day1-loop ends here
