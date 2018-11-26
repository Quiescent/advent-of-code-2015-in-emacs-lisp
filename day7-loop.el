;;; day7-loop --- My solution to day7-loop -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day7-loop

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defun is-lit (x)
  "Produce t if X is a literal."
  (let ((char (aref x 0)))
    (and (<= char ?9) (>= char ?0))))

(defun lit-or-wire (x)
  "Produce the literal or wire for X."
  (if (is-lit x)
      `(LIT ,(string-to-number x))
      `(WIRE ,x)))

(defun parse-circuit (file)
  "Parse lines in FILE to a circuit."
  (cl-loop
     with circuit = (make-hash-table :test #'equal)
     for line in (split-string file "\n" t " ")
     do (pcase (split-string line " " t " ")
          (`(,a "AND" ,b "->" ,c)
            (puthash c `(,(lit-or-wire a) AND ,(lit-or-wire b)) circuit))
          (`(,a "OR" ,b "->" ,c)
            (puthash c `(,(lit-or-wire a) OR ,(lit-or-wire b)) circuit))
          (`(,a "->" ,c)
            (puthash c (lit-or-wire a) circuit))
          (`(,a "LSHIFT" ,b "->" ,c)
            (puthash c `(,(lit-or-wire a) LSHIFT ,(lit-or-wire b)) circuit))
          (`(,a "RSHIFT" ,b "->" ,c)
            (puthash c `(,(lit-or-wire a) RSHIFT ,(lit-or-wire b)) circuit))
          (`("NOT" ,b "->" ,c)
            (puthash c `(NOT ,(lit-or-wire b)) circuit)))
     finally return circuit))

(defun evaluate-unary-node (node circuit dest)
  "Produce the evaluation of NODE in CIRCUIT.

Put the result into DEST for future invocations."
  (pcase node
    (`(WIRE ,a) (evaluate (gethash a circuit) circuit dest))
    (`(LIT  ,a) a)))

(defun evaluate (node circuit dest)
  "Evaluate the value at NODE in CIRCUIT.

Store the result in DEST for future invocations."
  (let ((result (pcase node
                  (`(,a AND ,b)
                    (logand (evaluate-unary-node a circuit a)
                            (evaluate-unary-node b circuit b)))
                  (`(,a OR ,b)
                    (logior (evaluate-unary-node a circuit a)
                            (evaluate-unary-node b circuit b)))
                  (`(,a LSHIFT ,b)
                    (lsh (evaluate-unary-node a circuit a)
                         (evaluate-unary-node b circuit b)))
                  (`(,a RSHIFT ,b)
                    (lsh (evaluate-unary-node a circuit a)
                         (- 0 (evaluate-unary-node b circuit b))))
                  (`(NOT ,b)
                    (logand 65535 (lognot (evaluate-unary-node b circuit b))))
                  (`(LIT ,a) a)
                  (`(WIRE ,a) (evaluate (gethash a circuit) circuit a)))))
    (when (consp dest)
      (puthash (cadr dest) `(LIT ,result) circuit))
    result))

(defun day7-loop-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (let ((circuit (parse-circuit input-file)))
    (evaluate (gethash "a" circuit) circuit "a")))

;; # PART 2:

(defun day7-loop-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (let* ((circuit (parse-circuit input-file)))
    (puthash "b" '(LIT 956) circuit)
    (evaluate (gethash "a" circuit) circuit "a")))

;; Run the solution:

(progn
  (message "\n********** OUTPUT **********")
  (let ((input-1 (with-temp-buffer
                   (find-file-literally "day7-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max))))
        (input-2 (with-temp-buffer
                   (find-file-literally "day7-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max))))
        (max-lisp-eval-depth 10000))
    (message "Part 1: %s" (day7-loop-part-1 input-1))
    (message "Part 2: %s\n" (day7-loop-part-2 input-2))))

(provide 'day7-loop)
;;; day7-loop ends here
