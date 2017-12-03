;;; day4 --- my solution to day4 -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; part 1

(defun iter-hashes-until (start stop-p)
  (let ((i 0)
        current-hash)
    (while (or (null current-hash)
               (not (funcall stop-p current-hash)))
      (setq current-hash (md5 (concat start (number-to-string i))))
      (incf i))
    (1- i)))

(defun day4-part-1 (input)
  (interactive "sInput: ")
  (message "%s" (iter-hashes-until
                 input
                 (lambda (hash)
                   (string-equal "00000"
                                 (substring hash 0 5))))))

;;(day4-part-1 "abcdef")

;;(day4-part-1 "iwrupvqb")

;; part 2

(defconst one (math-read-bignum "1"))

(defun iter-hashes-until-bignum (start stop-p)
  (let ((i (math-read-bignum "0"))
        current-hash)
    (while (or (null current-hash)
               (not (funcall stop-p current-hash)))
      (setq current-hash (md5 (concat start (format "%s" i))))
      (setq i (math-add-bignum i one)))
    (math-add-bignum i one)))

(defun first-6-are-zero (hash)
  (let ((result t))
    (dotimes (i 6 result)
      (setq result (and result (eq ?0 (aref hash i)))))))

(defun day4-part-2 (input)
  (interactive "sInput: ")
  (message "%s" (iter-hashes-until
                 input
                 #'first-6-are-zero)))

;; answer: 9958218
(provide 'day4)
;;; day4 ends here
