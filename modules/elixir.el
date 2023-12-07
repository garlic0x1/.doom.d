;;; elixir.el -*- lexical-binding: t; -*-

;; Configure lexical LSP
;; Make sure your Elixir version isn't old
;; (pacman package is outdated)
(use-package! lsp-mode
  :no-require t
  :commands lsp
  :hook (elixir-mode . lsp)
  :config
  (progn
    (lsp-lens-mode)
    ;; (setq lsp-enable-file-watchers nil)
    (add-to-list 'lsp-language-id-configuration '(elixir-mode . "elixir"))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection "/home/garlic/elixir/elixir-ls/release/language_server.sh")
      :major-modes '(elixir-mode)
      :activation-fn (lsp-activate-on "elixir")
      :server-id 'elixir-ls))))
