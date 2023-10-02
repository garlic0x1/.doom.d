;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(defun load-modules (directory)
  (mapcar #'load (cl-remove-if #'file-directory-p (directory-files directory t))))

;; load everything from ./modules
(load-modules "~/.doom.d/modules")
