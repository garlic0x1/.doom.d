;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; help load config files
(defun config-path (path)
  (concat doom-user-dir path))

;; get threading macros
(load (config-path "lang.el"))

;; convenience function for loading dirs
(defun load-modules (directory)
  (->> (directory-files directory t)
       (cl-remove-if #'file-directory-p)
       (mapcar #'load)))

;; load everything from ./modules
(load-modules (config-path "modules"))
