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

(setq user-full-name "garlic0x1"
      user-mail-address "garlic1@protonmail.com"
      org-directory "~/org/"
      geiser-guile-load-path '("~/scheme")
      display-line-numbers-type t)

(set-face-attribute 'default nil :height 130) ; bigger font
