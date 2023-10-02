;;; keybindings.el -*- lexical-binding: t; -*-

(map! :leader (:prefix ("d" . "garlic")
               :desc "toggle transprency" "t" #'toggle-transparency
               :desc "random known theme" "r" #'random-known-theme
               :desc "random light theme" "l" #'random-light-theme
               :desc "random theme"       "f" #'random-theme))

(map! :leader (:prefix ("l" . "various repls")
               :desc "common lisp" "l" #'sly
               :desc "scheme"      "s" #'geiser
               :desc "emacs"       "e" #'ielm
               :desc "python"      "p" #'run-python
               :desc "javascript"  "j" #'nodejs-repl
               :desc "elixir"      "e" #'alchemist-iex-run))

(map! :mode alchemist-mode
      :leader (:prefix ("m" . "alchemist")
               :desc "start project repl" "'" #'alchemist-iex-project-run
               :desc "start inferior repl" "\"" #'alchemist-iex-run
               :desc "compile buffer" "k" #'alchemist-iex-compile-this-buffer
               :desc "eval buffer" "E" #'alchemist-iex-compile-this-buffer
               :desc "eval region" "e" #'alchemist-eval-region
               :desc "send region" "s" #'alchemist-iex-send-region
               :desc "mix test" "t" #'alchemist-mix-test
               :desc "elixir format" "f" #'elixir-format))

;; quit eval buffers
(map! :mode alchemist-eval-mode
      :desc "close eval popup" :nvg "q" #'alchemist-eval-close-popup)

;; copied from https://codeberg.org/flowfx/doom.d
(define-key evil-motion-state-map (kbd "C-h") #'evil-window-left)
(define-key evil-motion-state-map (kbd "C-j") #'evil-window-down)
(define-key evil-motion-state-map (kbd "C-k") #'evil-window-up)
(define-key evil-motion-state-map (kbd "C-l") #'evil-window-right)
