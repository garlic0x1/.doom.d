;;; elixir.el -*- lexical-binding: t; -*-

(with-eval-after-load 'eglot
  (setf (alist-get 'elixir-mode eglot-server-programs)
        '("~/elixir/lexical/_build/dev/package/lexical/bin/start_lexical.sh")))

(add-hook 'elixir-mode-hook 'eglot-ensure)
(add-hook 'elixir-mode-hook (lambda () (apprentice-mode t)))
