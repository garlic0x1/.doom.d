;;; common-lisp.el -*- lexical-binding: t; -*-

;; cl hyperspec helper
(load "/home/garlic/quicklisp/clhs-use-local.el" t)

(setq sly-lisp-implementations
      `((sbcl (,(concat (config-path "bin/inferior-lisp")))
         :coding-system utf-8-unix)))
