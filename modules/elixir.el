;;; elixir.el -*- lexical-binding: t; -*-

;; Elixir REPL setup
;; Some keybindings are made in ./keybindings.el
(use-package! alchemist
  :hook (elixir-mode . alchemist-mode)
  :config
  (set-lookup-handlers! 'elixir-mode
    :definition #'alchemist-goto-definition-at-point
    :documentation #'alchemist-help-search-at-point)
  (set-eval-handler! 'elixir-mode #'alchemist-eval-region)
  (set-repl-handler! 'elixir-mode #'alchemist-iex-project-run)
  (setq alchemist-mix-env "dev")
  (setq alchemist-hooks-compile-on-save t)
  (map! :map elixir-mode-map :nv "SPC m" alchemist-mode-keymap))

;; Configure lexical LSP
;; Make sure your Elixir version isn't old
;; (pacman package is outdated)
(use-package! lsp-mode
  :no-require t
  :commands lsp
  :hook (elixir-mode . lsp)
  :config
  (progn
    (setq lsp-enable-file-watchers nil)
    (add-hook 'elixir-mode-hook 'alchemist-mode)
    (add-to-list 'lsp-language-id-configuration '(elixir-mode . "elixir"))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection "/home/garlic/.elixir-lexical/_build/dev/package/lexical/bin/start_lexical.sh")
      :major-modes '(elixir-mode)
      :activation-fn (lsp-activate-on "elixir")
      :server-id 'elixir-lexical
      :priority 0))))
