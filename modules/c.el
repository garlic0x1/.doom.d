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
        (global-spec (config-path "resources/clang-format")))
    (clang-format-with-spec
     (if (file-exists-p project-spec)
         project-spec global-spec))))

;; create hook to format after save
(add-hook 'after-save-hook
          #'(lambda () (when c-buffer-is-cc-mode (clang-format))))

(defun create-c-project (dir name)
  (interactive "DDirectory: \nsName: ")
  (let* ((make-template (config-path "resources/Makefile"))
         (main-template (config-path "resources/main.c"))
         (subdir (expand-file-name name dir))
         (make-dest (expand-file-name "Makefile" subdir)))
    (make-directory subdir t)
    (copy-file make-template make-dest)
    (shell-command
     (format "sed -i 's/~{project-name}/%s/g %s" name make-dest))
    (shell-command
     (format "make prepare --directory=%s" subdir))
    (copy-file main-template (expand-file-name "src/main.c" subdir))))
