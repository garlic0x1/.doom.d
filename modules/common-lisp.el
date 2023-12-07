;;; common-lisp.el -*- lexical-binding: t; -*-

;; cl hyperspec helper
(load "/home/garlic/quicklisp/clhs-use-local.el" t)

;; use sbcl or roswell repl
(setq inferior-lisp-program
      (if (shell-command "which ros")
          "sbcl" "ros -Q run"))
