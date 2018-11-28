;;; day8-loop --- My solution to day8-loop -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day8-loop

;; This one proves to be fiendishly difficult in ELisp because of the
;; automatic things which Emacs Lisp does to strings for you to make
;; the general case of editing text more natural.  In the end I
;; iterated over each character in a buffer skipping ahead where
;; necessary -- goes to show that the low tech solution is sometimes
;; best.

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defun day8-loop-part-1 (buffer)
  "Run my solution to part one of the problem on the input BUFFER."
  (with-current-buffer buffer
    (let ((code-char-count 0)
          (mem-char-count  0))
      (goto-char (point-min))
      (while (not (eobp))
        (if (eq (char-after) ?\\)
            (pcase (char-after (1+ (point)))
              (?\\ (progn (cl-incf code-char-count 2)
                          (cl-incf mem-char-count)
                          (forward-char 2)))
              (?\" (progn (cl-incf code-char-count 2)
                          (cl-incf mem-char-count)
                          (forward-char 2)))
              (?x  (progn (cl-incf code-char-count 4)
                          (cl-incf mem-char-count)
                          (forward-char 4))))
            (if (eq (char-after) ?\")
                (progn (cl-incf code-char-count 1)
                       (forward-char))
                (progn (cl-incf code-char-count)
                       (cl-incf mem-char-count)
                       (forward-char)))))
      (- code-char-count mem-char-count))))

;; # PART 2:

(defun day8-loop-part-2 (buffer)
  "Run my solution to part two of the problem on the given BUFFER."
  (with-current-buffer buffer
    (let ((encoded-char-count  0))
      (goto-char (point-min))
      (while (not (eobp))
        (when (or (eq (char-after) ?\\)
                  (eq (char-after) ?\"))
          (cl-incf encoded-char-count))
        (when (eq (char-after) ?\n)
          (cl-incf encoded-char-count 2))
        (forward-char))
      encoded-char-count)))

;; 2085 was too high

;; Run the solution:

(progn
  (message "\n********** OUTPUT **********")
  (let ((input-1 (get-buffer (find-file-literally "day8-loop-part-1")))
        (input-2 (get-buffer (find-file-literally "day8-loop-part-1"))))
    (message "Part 1: %s" (day8-loop-part-1 input-1))
    (message "Part 2: %s\n" (day8-loop-part-2 input-2))))

(provide 'day8-loop)
;;; day8-loop ends here
