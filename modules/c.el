(defun clang-format ()
  (interactive)
  (shell-command
   (concat
    "clang-format"
    " --style=file:" (config-path ".clang-format")
    " -i " buffer-file-name)))

(add-hook 'after-save-hook #'(lambda () (when c-buffer-is-cc-mode (clang-format))))
