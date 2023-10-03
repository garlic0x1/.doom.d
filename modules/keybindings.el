;;; keybindings.el -*- lexical-binding: t; -*-

(map! :mode alchemist-mode
      :desc "easy close eval popup" :n "q" #'alchemist-eval-close-popup
      :leader (:prefix ("m" . "alchemist")
               :desc "start project repl" "'" #'alchemist-iex-project-run
               :desc "start inferior repl" "\"" #'alchemist-iex-run
               :desc "compile buffer" "k" #'alchemist-iex-compile-this-buffer
               :desc "eval buffer" "E" #'alchemist-iex-compile-this-buffer
               :desc "eval region" "e" #'alchemist-eval-region
               :desc "send region" "s" #'alchemist-iex-send-region
               :desc "mix test" "t" #'alchemist-mix-test
               :desc "elixir format" "f" #'elixir-format))


(defvar garlic-keymap (make-sparse-keymap))
(map! :map garlic-keymap
      :nvg "C-h" #'windmove-left
      :nvg "C-j" #'windmove-down
      :nvg "C-k" #'windmove-up
      :nvg "C-l" #'windmove-right
      :leader (:prefix ("d" . "garlic")
               :desc "toggle transprency" "t" #'toggle-transparency
               :desc "random known theme" "r" #'random-known-theme
               :desc "random light theme" "l" #'random-light-theme
               :desc "random theme"       "f" #'random-theme)
      :leader (:prefix ("l" . "various repls")
               :desc "common lisp" "l" #'sly
               :desc "scheme"      "s" #'geiser
               :desc "emacs"       "e" #'ielm
               :desc "python"      "p" #'run-python
               :desc "javascript"  "j" #'nodejs-repl
               :desc "elixir"      "e" #'alchemist-iex-run))

(define-minor-mode garlic-mode
  "Personal keybindings"
  :global t :keymap garlic-keymap)

;; override other minor modes
(add-to-list 'emulation-mode-map-alists `((garlic-mode . ,garlic-keymap)))
;; enable the mode
(garlic-mode)
