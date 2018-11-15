;;; day3-loop --- My solution to day3-loop -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day3-loop

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defun day3-loop-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (cl-loop with houses-visited = (make-hash-table :test #'equal)
           with coordinate = '(0 0)
           do (puthash coordinate t houses-visited)
           for direction across input-file
           for (x y) = coordinate
           if (eq direction ?<)
           do (setq coordinate `(,(1- x) ,y))
           else if (eq direction ?>)
           do (setq coordinate `(,(1+ x) ,y))
           else if (eq direction ?^)
           do (setq coordinate `(,x      ,(1+ y)))
           else if (eq direction ?v)
           do (setq coordinate `(,x      ,(1- y)))
           do (puthash coordinate t houses-visited)
           finally return (hash-table-count houses-visited)))

;; # PART 2:

(defun day3-loop-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (cl-loop with houses-visited = (make-hash-table :test #'equal)
           with coordinate = [(0 0) (0 0)]
           with santa = 0
           do (puthash '(0 0) t houses-visited)
           for direction across input-file
           for (x y) = (aref coordinate santa)
           if (eq direction ?<)
           do (aset coordinate santa `(,(1- x) ,y))
           else if (eq direction ?>)
           do (aset coordinate santa `(,(1+ x) ,y))
           else if (eq direction ?^)
           do (aset coordinate santa `(,x      ,(1+ y)))
           else if (eq direction ?v)
           do (aset coordinate santa `(,x      ,(1- y)))
           do (puthash (aref coordinate santa) t houses-visited)
           if (eq santa 0)
           do (setq santa 1)
           else
           do (setq santa 0)
           finally return (hash-table-count houses-visited)))

;; Run the solution:

(progn
  (message "\n********** OUTPUT **********")
  (let ((input-1 (with-temp-buffer
                   (find-file-literally "day3-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max))))
        (input-2 (with-temp-buffer
                   (find-file-literally "day3-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max)))))
    (message "Part 1: %s" (day3-loop-part-1 input-1))
    (message "Part 2: %s\n" (day3-loop-part-2 input-2))))

(provide 'day3-loop)
;;; day3-loop ends here
