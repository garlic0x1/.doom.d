;; runs clang-format with a specified config file
(defun clang-format-with-spec (spec)
  (shell-command
   (concat "clang-format"
           " --style=file:" spec
           " -i " buffer-file-name)))

;; runs clang-format with .clang-format either
;; in projectile root dir, or in doom config dir
(defun clang-format ()
  (interactive)
  (let ((project-spec (concat (projectile-project-root) ".clang-format"))
        (global-spec (config-path ".clang-format")))
    (clang-format-with-spec
     (if (file-exists-p project-spec)
         project-spec global-spec))))

;; create hook to format after save
(add-hook 'after-save-hook
          #'(lambda () (when c-buffer-is-cc-mode (clang-format))))
