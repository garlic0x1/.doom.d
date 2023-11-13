;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; get threading macros
(load "~/.doom.d/lang.el")

(defun load-modules (directory)
  (->> (directory-files directory t)
       (cl-remove-if #'file-directory-p)
       (mapcar #'load)))

;; load everything from ./modules
(load-modules (config-path "modules"))
