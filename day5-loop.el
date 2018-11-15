;;; day5-loop --- My solution to day5-loop -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day5-loop

;;; Code:

;; # PART 1:

(require 'cl-lib)


;; It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
;; It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
;; It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.


(defun day5-nice-p (str)
  "Produce t if STR is naughty."
  (and
   (>= (length
        (cl-remove-if-not (lambda (c) (or (eq c ?a)
                                          (eq c ?e)
                                          (eq c ?i)
                                          (eq c ?o)
                                          (eq c ?u)))
                          str)) 3)
   (cl-loop
      for i from 0 to (- (length str) 2)
      thereis (eq (aref str i)
                  (aref str (1+ i))))
   (cl-loop
      for i from 0 to (- (length str) 2)
      for substring = (substring str i (+ i 2))
      never (or (string-equal substring "ab")
                (string-equal substring "cd")
                (string-equal substring "pq")
                (string-equal substring "xy")))))

(defun day5-loop-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (cl-loop
     for line in (split-string input-file "\n" t " ")
     count (day5-nice-p line)))

;; # PART 2:


;; It contains a pair of any two letters that appears at least twice in the string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).
;; It contains at least one letter which repeats with exactly one letter between them, like xyx, abcdefeghi (efe), or even aaa.

(defun day5-nice-2-p (str)
  "Produce t if STR is naughty."
  (and
   (cl-loop
      for i from 0 to (- (length str) 2)
      for sub = (substring str i (+ i 2))
      thereis (cl-loop
                 for j from (+ 2 i) to (- (length str) 2)
                 thereis (string-equal sub (substring str j (+ j 2)))))
   (cl-loop
      for i from 0 to (- (length str) 3)
      thereis (eq (aref str i)
                  (aref str (+ 2 i))))))


(defun day5-loop-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (cl-loop
     for line in (split-string input-file "\n" t " ")
     count (day5-nice-2-p line)))

;; Run the solution:

(progn
  (message "\n********** OUTPUT **********")
  (let ((input-1 (with-temp-buffer
                   (find-file-literally "day5-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max))))
        (input-2 (with-temp-buffer
                   (find-file-literally "day5-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max)))))
    (message "Part 1: %s" (day5-loop-part-1 input-1))
    (message "Part 2: %s\n" (day5-loop-part-2 input-2))))

(provide 'day5-loop)
;;; day5-loop ends here
