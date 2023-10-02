;;; misc.el -*- lexical-binding: t; -*-

(defun random-el (items)
  "random element of list"
  (let* ((size (length items))
         (index (random size)))
    (nth index items)))

(defun color-picker ()
  "run xcolor and yank result"
  (interactive)
  (let ((res (shell-command-to-string "xcolor")))
    (doom-print res)
    (kill-new res)))
