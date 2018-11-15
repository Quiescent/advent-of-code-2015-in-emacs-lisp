;;; day4-loop --- My solution to day4-loop -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day4-loop

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defun day4-loop-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (cl-loop
     for x from 0 to 1000000
     for hash = (md5 (concat input-file (number-to-string x)))
     when (cl-every (apply-partially #'eq ?0) (substring hash 0 5))
       return x))

;; # PART 2:

(defun day4-loop-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (cl-loop
     for x from 0 to 10000000
     for hash = (md5 (concat input-file (number-to-string x)))
     when (cl-every (apply-partially #'eq ?0) (substring hash 0 6))
       return x))

;; Run the solution:

(progn
  (message "\n********** OUTPUT **********")
  (let ((input-1 (with-temp-buffer
                   (find-file-literally "day4-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max))))
        (input-2 (with-temp-buffer
                   (find-file-literally "day4-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max)))))
    (message "Part 1: %s" (day4-loop-part-1 input-1))
    (message "Part 2: %s\n" (day4-loop-part-2 input-2))))


(provide 'day4-loop)
;;; day4-loop ends here
