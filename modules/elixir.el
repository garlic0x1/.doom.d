;;; elixir.el -*- lexical-binding: t; -*-

;; Elixir configuration
;; Start by configuring Alchemist for some tasks.
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
      :priority 0))
    ))

;; Enable format and iex reload on save (broken)
;; (after! lsp
;;   (add-hook 'elixir-mode-hook
;;             (lambda ()
;;               (add-hook 'before-save-hook 'elixir-format nil t)
;;               (add-hook 'after-save-hook 'alchemist-iex-reload-module))))
