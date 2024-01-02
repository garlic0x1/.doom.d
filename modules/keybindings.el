;;; keybindings.el -*- lexical-binding: t; -*-

(map!
 ;; :mode alchemist-mode
 :after alchemist-mode-hook
 ;; :map alchemist-mode-keymap
 :map garlic-keymap
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
               :desc "random theme"       "f" #'random-theme
               :desc "LLM selected"       "a" #'ollama-prompt-region
               :desc "prompt LLM"         "a" #'ollama-prompt)
      :leader (:prefix ("l" . "various repls")
               :desc "common lisp" "l" #'garlic-start-sly
               :desc "vterm"       "v" #'vterm
               :desc "LFE"         "f" #'inferior-lfe
               :desc "scheme"      "s" #'geiser
               :desc "emacs"       "m" #'ielm
               :desc "clojure"     "c" #'cider-jack-in-clj
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
