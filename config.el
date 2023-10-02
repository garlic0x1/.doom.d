;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "garlic0x1"
      user-mail-address "garlic1@protonmail.com"

      org-directory "~/org/"
      geiser-guile-load-path '("~/scheme")

      display-line-numbers-type t)

(set-face-attribute 'default nil :height 130) ; bigger font

(defun load-modules (directory)
  (mapcar #'load (cl-remove-if #'file-directory-p (directory-files directory t))))

(load-modules "~/.doom.d/modules")
