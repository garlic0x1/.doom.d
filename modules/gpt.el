;;; gpt.el -*- lexical-binding: t; -*-

(defgroup ollama nil
  "Ollama client for Emacs."
  :group 'ollama)

(defcustom ollama:endpoint "http://192.168.68.108:11434/api/generate"
  "Ollama http service endpoint."
  :group 'ollama
  :type 'string)

(defcustom ollama:model "llama2"
  "Ollama model."
  :group 'ollama
  :type 'string)

(defcustom ollama:language "English"
  "Language to translate to."
  :group 'ollama
  :type 'string)

(defun ollama-read-line (line)
  (cdr (assoc 'response (json-read-from-string line))))

(defun ollama-read-resp (data)
  (mapconcat #'ollama-read-line
             (cl-remove-if #'string-empty-p (split-string data "\n"))
             ""))

(defun ollama-response-handler (prompt response)
  (with-output-to-temp-buffer "*ollama*"
    (princ "Prompt:\n")
    (princ prompt)
    (princ "\n-----------------------------------------------------------\nResponse:\n")
    (princ response)))

(defun ollama-fetch (prompt)
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (json-encode `((model . ,ollama:model) (prompt . ,prompt)))))
    (url-retrieve
     ollama:endpoint
     (lambda (_status)
       (goto-char url-http-end-of-headers)
       (let ((data (decode-coding-string
                    (buffer-substring-no-properties (point) (point-max))
                    'utf-8)))
         (ollama-response-handler prompt (ollama-read-resp data)))))))

(defun current-region ()
  (buffer-substring
   (region-beginning)
   (region-end)))

(defun ollama-translate-word ()
  "Translate current word."
  (interactive)
  (ollama-fetch
   (format "translate \"%s\" to %s" (thing-at-point 'word) ollama:language)))

(defun ollama-summarize-region ()
  "Summarize marked text."
  (interactive)
  (ollama-fetch (format "summarize \"%s\"" (current-region))))

(defun ollama-prompt-region ()
  "Use the marked text as a prompt."
  (interactive)
  (ollama-fetch (current-region)))

(defun ollama-prompt (prompt)
  "Interactively prompt ollama."
  (interactive "sPrompt: ")
  (ollama-fetch prompt))
