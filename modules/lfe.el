;;; lfe-custom.el -*- lexical-binding: t; -*-

;; from the LFE git repo, with modifications for lfe-ls
(defvar lfe-module "~/.doom.d/modules/lfe")
(add-to-list 'load-path lfe-module)
(load-modules "~/.doom.d/modules/lfe")

;;; set up completion
;; eglot
(with-eval-after-load 'eglot
  (add-to-list
   'eglot-server-programs
   '(lfe-mode . ("lfe-ls"
                 "--transport" "tcp" "--port" :autoport))))

;; idk how to make it use stdio
;; (defgroup lfe-ls nil "LSP group for LFE" :group 'lsp-mode)

;; (with-eval-after-load 'lsp-mode
;;   (add-to-list 'lsp-language-id-configuration '(lfe-mode . "lfe"))

;;   (lsp-register-client
;;    (make-lsp-client
;;     :new-connection (lsp-stdio-connection "lfe-ls --transport tcp --port 42069")
;;     :major-modes '(lfe-mode)
;;     :activation-fn (lsp-activate-on "lfe")
;;     :server-id 'lfe-ls
;;     :priority -1)))
