;;; day2-loop --- My solution to day2-loop -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day2-loop

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defun day2-loop-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (cl-loop
     for line in (split-string input-file "\n" t " ")
     for (length width height) = (mapcar #'string-to-number (split-string line "x" t " "))
     for (x      y     z)      = (sort `(,length ,width ,height) #'<)
     sum (+ (* 2 length width)
            (* 2 width height)
            (* 2 height length)
            (* x y))))

;; # PART 2:

(defun day2-loop-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (cl-loop
     for line in (split-string input-file "\n" t " ")
     for (length width height) = (mapcar #'string-to-number (split-string line "x" t " "))
     for (x      y     z)      = (sort `(,length ,width ,height) #'<)
     sum (+ (* 2 x) (* 2 y)
                    (* length width height))))

;; Run the solution:

(progn
  (message "\n********** OUTPUT **********")
  (let ((input-1 (with-temp-buffer
                   (find-file-literally "day2-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max))))
        (input-2 (with-temp-buffer
                   (find-file-literally "day2-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max)))))
    (message "Part 1: %s" (day2-loop-part-1 input-1))
    (message "Part 2: %s\n" (day2-loop-part-2 input-2))))




(provide 'day2-loop)
;;; day2-loop ends here
