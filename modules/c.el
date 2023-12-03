(defun clang-format ()
  (interactive)
  (save-buffer)
  (shell-command
   (concat
    "clang-format"
    " --style=file:" (config-path ".clang-format")
    " -i " buffer-file-name)))
