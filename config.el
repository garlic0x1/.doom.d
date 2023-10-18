;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(defvar custom-modules "~/.doom.d/modules")
(add-to-list 'load-path custom-modules)

;; get threading macros
(load "~/.doom.d/lang.el")

(defun load-modules (directory)
  (->>
   (directory-files directory t)
   (cl-remove-if #'file-directory-p)
   (mapcar #'load)))

;; load everything from ./modules
(load-modules "~/.doom.d/modules")
