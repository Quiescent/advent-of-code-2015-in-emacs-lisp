;;; day6 --- my solution to day6 -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'seq)
(require 'calc)
(eval-when-compile
  (require 'cl))

;; part 1

(defun parse-coord (coord-string)
  "Parse a coordinate from COORD-STRING."
  (mapcar #'string-to-number (split-string coord-string "," t)))

(defun parse-instruction (instruction-line)
  "Parse an instruction and it's start-end coords from INSTRUCTION-LINE."
  (pcase (split-string instruction-line " " t)
    (`(,command ,sub-command ,start-coord ,_ ,end-coord)
     `(,(if (string-equal sub-command "off") 'TURN-OFF 'TURN-ON)
       ,(parse-coord start-coord)
       ,(parse-coord end-coord)))
    (`(,command ,start-coord ,_ ,end-coord)
     `(TOGGLE
       ,(parse-coord start-coord)
       ,(parse-coord end-coord)))))

(defun make-grid ()
  "Make a grid of oppropriate dimension."
  (let ((grid (make-vector 1000 nil)))
    (dotimes (idx 1000 grid)
      (aset grid idx (make-vector 1000 nil)))))

(defun parse-instructions (instruction-lines)
  "Parse all the instructions in INSTRUCTION-LINES."
  (mapcar #'parse-instruction instruction-lines))

(defun iter-grid-do (grid fn start-x start-y end-x end-y)
  "Modify GRID by applying FN to the coord given range.

Range is [(START-X, START-Y), (END-X, END-Y)]"
  (dotimes (x (1+ (- end-x start-x)))
    (dotimes (y (1+ (- end-y start-y)))
      (aset (aref grid (+ y start-y)) (+ x start-x)
            (funcall fn (aref (aref grid (+ y start-y))
                              (+ x start-x)))))))

(defun follow-instruction (grid instruction)
  "Modify grid with the given INSTRUCTION."
  (pcase instruction
    (`(TURN-OFF (,x-start ,y-start) (,x-end ,y-end))
     (iter-grid-do grid (lambda (x) nil) x-start y-start x-end y-end))
    (`(TURN-ON  (,x-start ,y-start) (,x-end ,y-end))
     (iter-grid-do grid (lambda (x) t) x-start y-start x-end y-end))
    (`(TOGGLE   (,x-start ,y-start) (,x-end ,y-end))
     (iter-grid-do grid (lambda (x) (not x)) x-start y-start x-end y-end))))

(defun count-lit (grid)
  "Count the number of non-nil blocks in GRID."
  (cl-count
   t
   (apply (apply-partially #'seq-concatenate 'list)
          (seq-map #'identity grid))))

(defun day6-part-1 (input)
  "Solve day6 part 1 for INPUT."
  (interactive "sInput: ")
  (let ((instructions (parse-instructions (split-string input "\n" t)))
        (grid         (make-grid)))
    (mapc (apply-partially #'follow-instruction grid)
          instructions)
    (message "%s" (count-lit grid))))

;; Part 2

(defun follow-corrected-instruction (grid instruction)
  "Modify GRID by applying brightness instruction INSTRUCTION."
  (pcase instruction
    (`(TURN-OFF (,x-start ,y-start) (,x-end ,y-end))
     (iter-grid-do grid (lambda (x) (if (< (1- x) 0) 0 (1- x))) x-start y-start x-end y-end))
    (`(TURN-ON  (,x-start ,y-start) (,x-end ,y-end))
     (iter-grid-do grid (lambda (x) (1+ x)) x-start y-start x-end y-end))
    (`(TOGGLE   (,x-start ,y-start) (,x-end ,y-end))
     (iter-grid-do grid (lambda (x) (+ x 2)) x-start y-start x-end y-end))))

(defun make-corrected-grid ()
  "Make a grid with brightnesses."
  (let ((grid (make-vector 1000 nil)))
    (dotimes (idx 1000 grid)
      (aset grid idx (make-vector 1000 0)))))

(defun day6-part-2 (input)
  "Solve day6 part 2 for INPUT."
  (interactive "sInput: ")
  (let ((instructions (parse-instructions (split-string input "\n" t)))
        (grid (make-corrected-grid)))
    (mapc (apply-partially #'follow-corrected-instruction grid)
          instructions)
    (message
     "%s"
     (cl-reduce #'math-add
                (apply (apply-partially #'seq-concatenate 'list)
                       (seq-map #'identity grid))))))

(provide 'day6)
;;; day6 ends here
