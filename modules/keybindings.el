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


(define-minor-mode foo-mode
  "Get your foos in the right places."
  :lighter " foo"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c f") 'insert-foo)
            map)
  (make-local-variable 'foo-count))

(define-minor-mode evil-nav
  "Minor mode for my personal keybindings."
  :global t :keymap (make-sparse-keymap))

(map! :mode evil-nav
      :nvg "C-h" #'windmove-left
      :nvg "C-j" #'windmove-down
      :nvg "C-k" #'windmove-up
      :nvg "C-l" #'windmove-right)

(evil-nav)
