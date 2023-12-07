(defun clang-format-with-spec (spec)
  (shell-command
   (concat "clang-format"
           " --style=file:" spec
           " -i " buffer-file-name)))

(defun clang-format ()
  (interactive)
  (let ((project-spec (concat (projectile-project-root) ".clang-format"))
        (global-spec (config-path ".clang-format")))
    (clang-format-with-spec
     (if (file-exists-p project-spec)
         project-spec global-spec))))

(add-hook 'after-save-hook
          #'(lambda () (when c-buffer-is-cc-mode (clang-format))))
