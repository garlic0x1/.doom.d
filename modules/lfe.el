;;; lfe.el -*- lexical-binding: t; -*-


;;; lfe-mode.el --- Lisp Flavoured Erlang mode

;; Copyright (c) 2012-2020 Robert Virding
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Author Robert Virding

;;; Commentary:
;; Copied from `lisp-mode' and modified for LFE.

;;; Code:

(require 'lisp-mode)

(defgroup lfe nil
  "LFE support."
  :group 'lisp
  :group 'languages)

(defvar prettify-symbols-alist ())

(defconst lfe--prettify-symbols-alist '(("lambda"  . ?λ))
  "Prettfy symbols alist user in Lisp Flavoured Erlang mode.")

(defvar lfe-mode-syntax-table
  (let ((table (copy-syntax-table lisp-mode-syntax-table)))
    ;; Like scheme we allow [ ... ] as alternate parentheses.
    (modify-syntax-entry ?\[ "(]  " table)
    (modify-syntax-entry ?\] ")[  " table)
    table)
  "Syntax table in use in Lisp Flavoured Erlang mode buffers.")

(defvar lfe-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-shared-map)
    (define-key map "\e[" 'lfe-insert-brackets)
    map)
  "Keymap for Lisp Flavoured Erlang mode.")

(defvar lfe-mode-abbrev-table ()
  "Abbrev table used in Lisp Flavoured Erlang mode.")

(defvar lfe-mode-hook nil
  "*Hook for customizing Inferior LFE mode.")

(defun lfe-insert-brackets (&optional arg)
  "Enclose following `ARG' sexps in brackets.
Leave point after open-bracket."
  (interactive "P")
  (insert-pair arg ?\[ ?\]))

;;;###autoload
(defun lfe-mode ()
  "Major mode for editing Lisp Flavoured Erlang.  It's just like `lisp-mode'.

Other commands:
\\{lfe-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'lfe-mode)
  (setq mode-name "LFE")
  (lfe-mode-variables)
  (lfe-font-lock-setup)
  (use-local-map lfe-mode-map)
  (setq imenu-case-fold-search t)
  (run-mode-hooks 'lfe-mode-hook))

(defun lfe-mode-variables ()
  "Variables for LFE modes."
  (set-syntax-table lfe-mode-syntax-table)
  (setq local-abbrev-table lfe-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat page-delimiter "\\|$"))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'lisp-fill-paragraph)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because lisp-fill-paragraph should do the job.
  (make-local-variable 'adaptive-fill-mode)
  (setq adaptive-fill-mode nil)
  (make-local-variable 'normal-auto-fill-function)
  (setq normal-auto-fill-function 'lisp-mode-auto-fill)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'lisp-indent-line)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'outline-regexp)
  (setq outline-regexp ";;;;* \\|(")
  (make-local-variable 'outline-level)
  (setq outline-level 'lisp-outline-level)
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-start-skip)
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (setq comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+ *")
  (make-local-variable 'comment-add)
  (setq comment-add 1)                  ;default to `;;' in comment-region
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'lisp-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  ;; Make lisp-indent-line call lfe-indent-line.
  (make-local-variable 'lisp-indent-function)
  (set lisp-indent-function 'lfe-indent-function)
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression lisp-imenu-generic-expression)
  (make-local-variable 'multibyte-syntax-as-symbol)
  (setq multibyte-syntax-as-symbol t)
  ;; Don't use seq-local here for backwards compatibility.
  (make-local-variable 'prettify-symbols-alist)
  (setq prettify-symbols-alist lfe--prettify-symbols-alist))

;;; Font locking
;;; Include the older forms here as well.

(defconst lfe-font-lock-keywords
  (eval-when-compile
    (list
     ;; Type definition macros.
     (list
      (concat
       "("
       (regexp-opt '("defmodule" "defrecord" "deftype" "defopaque" "defspec") t)
       "\\>"
       ;; Any whitespace and declared object.
       "[ \t]*(?"
       "\\(\\sw+\\)?")
      '(1 font-lock-keyword-face)
      '(2 font-lock-type-face nil t))

     ;; Function/macro definition macros.
     (list
      (concat
       "("
       (regexp-opt '("defun" "defmacro" "defmethod" "define" "defsyntax") t)
       "\\>"
       ;; Any whitespace and declared object.
       "[ \t]*(?"
       "\\(\\sw+\\)?")
      '(1 font-lock-keyword-face)
      '(2 font-lock-function-name-face nil t))

     ;; LM flavor and struct macros.
     (list
      (concat
       ;; No defmethod here!
       "("
       (regexp-opt '("defflavor" "endflavor" "defstruct") t)
       "\\>"
       ;; Any whitespace and declared object.
       "[ \t]*(?"
       "\\(\\sw+\\)?")
      '(1 font-lock-keyword-face)
      '(2 font-lock-type-face nil t))

     ;; Type definition keywords.
     (list
      (concat
       "("
       (regexp-opt '("define-module" "define-type" "define-opaque-type"
                     "define-function-spec" "define-record") t)
       "\\>"
       ;; Any whitespace and declared object.
       "[ \t]*(?"
       "\\(\\sw+\\)?")
      '(1 font-lock-keyword-face)
      '(2 font-lock-type-face nil t))

     ;; Function definition forms.
     (list
      (concat
       "("
       (regexp-opt '("define-function" "define-macro" "define-syntax") t)
       "\\>"
       ;; Any whitespace and declared object.
       "[ \t]*(?"
       "\\(\\sw+\\)?")
      '(1 font-lock-keyword-face)
      '(2 font-lock-function-name-face nil t))

     ;; Core forms and macros without special handling.
     (list
      (concat
       "("
       (regexp-opt '( ;; Core forms.
                     "after" "call" "case" "catch"
                     "eval-when-compile" "extend-module"
                     "funcall" "if" "lambda"
                     "let" "let-function" "letrec-function" "let-macro"
                     "match-lambda" "progn" "receive" "try" "when"
                     ;; Core macro forms.
                     "andalso" "bc" "binary-comp" "cond" "do"
                     "dbg-ms" "ets-ms" "table-ms" "trace-ms"
                     "flet" "flet*" "fletrec"
                     "fun" "lc" "list-comp"
                     "let*" "match-spec" "macrolet" "orelse"
                     "prog1" "prog2" "qlc" "syntaxlet"
                     ":" "?" "++" "++*") t)
       "\\>")
      1 'font-lock-keyword-face)

     ;; Test macros.
     (list
      (concat
       "("
       (regexp-opt '("deftest" "deftestgen" "deftestskip" "deftestcase"
                     "deftestcases" "defsetup" "defteardown") t)
       "\\>"
       ;; Any whitespace and declared object.
       "[ \t]*(?"
       "\\(\\sw+\\)?")
      '(1 font-lock-keyword-face)
      '(2 font-lock-function-name-face nil t))

     ;; Type tests.
     (list
      (concat
       "("
       (regexp-opt '("is_atom" "is_binary" "is_bitstring" "is_boolean"
                     "is_float" "is_function" "is_integer" "is_list"
                     "is_map" "is_number" "is_pid" "is_port"
                     "is_record" "is_reference" "is_tuple") t)
       "\\>")
      1 'font-lock-builtin-face)

     ;; Type forms.
     (list
      (concat
       "("
       (regexp-opt '("abs" "float" "round" "trunc" "+" "-" "*" "/"
                     "==" "/=" "=:=" "=/=" ">" ">=" "<" "=<"
                     "iolist_size" "length" "make_ref" ;;"size"
                     "binary" "bit_size" "byte_size"
                     "tuple" "tuple_size" "tref" "tset" "element" "setelement"
                     "hd" "tl"
                     "cons" "car" "cdr" "caar" "cadr" "cdar" "cddr"
                     ;; Just for the fun of it.
                     "caaar" "caadr" "cadar" "caddr"
                     "cdaar" "cddar" "cdadr" "cdddr"
                     "function" "list" "list*"
                     "map" "mref" "mset" "mupd"
                     "map-get" "map-set" "map-update") t)
       "\\>")
      1 'font-lock-builtin-face)
     ))
  "Expressions to highlight in LFE modes.")

(defun lfe-font-lock-setup ()
  "Configures font-lock for editing LFE code."
  ;;   ;; For making font-lock case independent, which LFE isn't.
  ;;   (make-local-variable 'font-lock-keywords-case-fold-search)
  ;;   (setq font-lock-keywords-case-fold-search t)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '(lfe-font-lock-keywords
          nil nil (("+-*/.<>=!?$%_&~^:@" . "w")) beginning-of-defun
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function
           . lisp-font-lock-syntactic-face-function)))
  )

;;;###autoload
;; Associate ".lfe{s,sh}?" with LFE mode.
(add-to-list 'auto-mode-alist '("\\.lfe\\(s\\|sh\\)?\\'" . lfe-mode) t)

;;;###autoload
;; Ignore files ending in ".jam", ".vee", and ".beam" when performing
;; file completion.
(dolist (lfe-ext '(".beam" ".jam" ".vee"))
  (add-to-list 'completion-ignored-extensions lfe-ext))

;; The end.
(provide 'lfe-mode)

(defvar lfe-load-hook nil
  "*Functions to run when LFE mode is loaded.")

(run-hooks 'lfe-load-hook)
;;; lfe-mode.el ends here

;;; lfe-start.el --- Initialise the LFE mode package

;; Copyright (c) 2012-2020 Robert Virding
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Author: Robert Virding

;;; Commentary:

;;; Code:

;; Declare autoload functions in lfe-mode.el and inferior-lfe.el.
(autoload 'lfe-mode "lfe-mode" "Major mode for editing LFE code." t)
(autoload 'lfe-indent-function "lfe-indent" "Indent LFE." t)
(autoload 'inferior-lfe-mode "inferior-lfe"
  "Major mode for interacting with an inferior LFE process." t)
(autoload 'inferior-lfe "inferior-lfe" "Run an LFE process." t)
(autoload 'run-lfe "inferior-lfe" "Run an LFE process." t)

;; Associate ".lfe" with LFE mode.
(add-to-list 'auto-mode-alist '("\\.lfe\\'" . lfe-mode) t)

;; Ignore files ending in ".jam", ".vee", and ".beam" when performing
;; file completion.
(dolist (lfe-ext '(".beam" ".jam" ".vee"))
  (add-to-list 'completion-ignored-extensions lfe-ext))

(provide 'lfe-start)
;;; lfe-start.el ends here


;;; lfe-indent.el --- Lisp Flavoured Erlang indent mode

;; Copyright (c) 2015-2020 Robert Virding
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Author Robert Virding

;;; Commentary:
;; Copied from `lisp-mode' and modified for LFE.

;;; Code:

(require 'lisp-mode)
(require 'lfe-mode)

;;; Lisp indent

(defvar calculate-lisp-indent-last-sexp)

;;;###autoload
(defun lfe-indent-function (indent-point state)
  "This function is the normal value of the variable `lfe-indent-function'.

If this function is the value of the variable `lisp-indent-function' then
`calculate-lisp-indent' will call it to determine if the
arguments of a LFE function call should be indented specially.

INDENT-POINT is the position where the user typed TAB, or equivalent.
Point is located at the point to indent under;
`STATE' is the `parse-partial-sexp' state for that position.

Copied from function `lisp-indent-function', but with gets of
lfe-indent-{function,hook} and it uses `lfe-body-indent'."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
	;; Don't use function-get here for backwards compatibility.
        (setq method (or (get (intern-soft function) 'lfe-indent-function)
                         (get (intern-soft function) 'lfe-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lfe-indent-defform state indent-point))
              ((integerp method)
               (lfe-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method state indent-point normal-indent)))))))

(defcustom lfe-body-indent 2
  "Number of columns to indent the second line of a `(def...)' form."
  :group 'lfe
  :type 'integer)
(put 'lfe-body-indent 'safe-local-variable 'integerp)

(defun lfe-indent-specform (count state indent-point normal-indent)
  (let ((containing-form-start (elt state 1))
        (i count)
        body-indent containing-form-column)
    ;; Move to the start of containing form, calculate indentation
    ;; to use for non-distinguished forms (> count), and move past the
    ;; function symbol.  lfe-indent-function guarantees that there is at
    ;; least one word or symbol character following open paren of containing
    ;; form.
    (goto-char containing-form-start)
    (setq containing-form-column (current-column))
    (setq body-indent (+ lfe-body-indent containing-form-column))
    (forward-char 1)
    (forward-sexp 1)
    ;; Now find the start of the last form.
    (parse-partial-sexp (point) indent-point 1 t)
    (while (and (< (point) indent-point)
                (condition-case ()
                    (progn
                      (setq count (1- count))
                      (forward-sexp 1)
                      (parse-partial-sexp (point) indent-point 1 t))
                  (error nil))))
    ;; Point is sitting on first character of last (or count) sexp.
    (if (> count 0)
        ;; A distinguished form.  If it is the first or second form use double
        ;; lfe-body-indent, else normal indent.  With lfe-body-indent bound
        ;; to 2 (the default), this just happens to work the same with if as
        ;; the older code, but it makes unwind-protect, condition-case,
        ;; with-output-to-temp-buffer, et. al. much more tasteful.  The older,
        ;; less hacked, behavior can be obtained by replacing below with
        ;; (list normal-indent containing-form-start).
        (if (<= (- i count) 1)
            (list (+ containing-form-column (* 2 lfe-body-indent))
                  containing-form-start)
            (list normal-indent containing-form-start))
      ;; A non-distinguished form.  Use body-indent if there are no
      ;; distinguished forms and this is the first undistinguished form,
      ;; or if this is the first undistinguished form and the preceding
      ;; distinguished form has indentation at least as great as body-indent.
      (if (or (and (= i 0) (= count 0))
              (and (= count 0) (<= body-indent normal-indent)))
          body-indent
          normal-indent))))

(defun lfe-indent-defform (state indent-point)
  (goto-char (car (cdr state)))
  (forward-line 1)
  (if (> (point) (car (cdr (cdr state))))
      (progn
	(goto-char (car (cdr state)))
	(+ lfe-body-indent (current-column)))))

;;; Indentation rule helpers
;; Modified from `clojure-mode'.

(defun put-lfe-indent (sym indent)
  "Instruct `lfe-indent-function' to indent the body of `SYM' by `INDENT'."
  (put sym 'lfe-indent-function indent))

(defmacro define-lfe-indent (&rest kvs)
  "Call `put-lfe-indent' on a series, `KVS'."
  `(progn
     ,@(mapcar (lambda (x)
                 `(put-lfe-indent (quote ,(car x)) ,(cadr x)))
               kvs)))

;;; Special indentation rules
;; "def" anything is already fixed!

;; (define-lfe-indent (begin 0)), say, causes begin to be indented
;; like defun if the first form is placed on the next line, otherwise
;; it is indented like any other form (i.e. forms line up under first).

(define-lfe-indent
  (: 2)
  (after 1)
  (bc 1)
  (binary-comp 1)
  (call 2)
  (case 1)
  (catch 0)
  (define-function 1)
  (define-macro 1)
  (define-module 1)
  (extend-module 0)
  (do 2)
  (eval-when-compile 0)
  (flet 1)
  (flet* 1)
  (fletrec 1)
  (if 1)
  (lambda 1)
  (let 1)
  (let* 1)
  (let-function 1)
  (letrec-function 1)
  (let-macro 1)
  (lc 1)
  (list-comp 1)
  (macrolet 1)
  (match-lambda 0)
  (match-spec 0)
  (prog1 1)
  (prog2 2)
  (progn 0)
  (receive 0)
  (try 1)
  (when 0)
  (syntaxlet 1)

  (defflavor 3)				;This doesn't behave like other def's

  ;; Old style forms.
  (begin 0)
  (let-syntax 1)
  (syntax-rules 0)
  (macro 0))

(provide 'lfe-indent)
;;; lfe-indent.el ends here


;;; inferior-lfe.el --- Inferior Lisp Flavoured Erlang mode

;; Copyright (c) 2012-2020 Robert Virding
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Author Robert Virding

;;; Commentary:
;; Copied from inf-lisp and modified for LFE.

;;; Code:

(require 'comint)
(require 'lfe-mode)

(defvar inferior-lfe-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (set-keymap-parent map lisp-mode-shared-map)
    (define-key map "\C-x\C-e" 'lfe-eval-last-sexp)
    (define-key map "\C-c\M-o" 'inferior-lfe-clear-buffer)
    map)
  "Keymap for inferior LFE mode.")

;; (defvar inferior-lfe-mode-map nil)
;; (unless inferior-lfe-mode-map
;;   (setq inferior-lfe-mode-map (copy-keymap comint-mode-map))
;;   (set-keymap-parent inferior-lfe-mode-map lisp-mode-shared-map)
;;   (define-key inferior-lfe-mode-map "\C-x\C-e" 'lfe-eval-last-sexp))

(define-key lfe-mode-map "\C-x\C-e" 'lfe-eval-last-sexp) ; GNU convention
(define-key lfe-mode-map "\C-c\C-r" 'lfe-eval-region)
(define-key lfe-mode-map "\C-c\C-z" 'switch-to-lfe)

;; (defvar inferior-lfe-program "lfe -pa /Users/rv/erlang/lfe/ebin -env TERM vt100"
(defcustom inferior-lfe-program "lfe"
  "*Program name for invoking an inferior LFE in Inferior LFE mode."
  :group 'lfe
  :type 'string)

(defcustom inferior-lfe-program-options '("-pa" "/Users/rv/erlang/lfe/ebin")
  "*The options used when activating the LFE shell.

This must be a list of strings.
You may add the following command line options:
- \"-nobanner\"."
  :group 'lfe
  :type '(repeat string))

(defvar inferior-lfe-prompt "^[^>]*>+ *"
  "*Regexp to recognise prompts in the Inferior LFE mode.")

(defvar inferior-lfe-buffer nil
  "*The current inferior-lfe process buffer.")

(defvar inferior-lfe-mode-hook nil
  "*Hook for customizing Inferior LFE mode.")

(defvar inferior-lfe-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "*Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters.")

;;;###autoload
(defun inferior-lfe-mode ()
  "Major mode for interacting with an inferior LFE process.

\\{inferior-lfe-mode-map}

Customization: Entry to this mode runs the hooks on `comint-mode-hook' and
`inferior-lfe-mode-hook' (in that order)."
  (interactive)
  (delay-mode-hooks (comint-mode))
  (setq major-mode 'inferior-lfe-mode)
  (setq mode-name "Inferior LFE")
  (setq mode-line-process '(":%s"))
  (lfe-mode-variables)
  (use-local-map inferior-lfe-mode-map)
  (setq comint-prompt-regexp inferior-lfe-prompt)
  (setq comint-prompt-read-only t)
  (setq comint-input-filter (function lfe-input-filter))
  (setq comint-get-old-input (function lfe-get-old-input))
  (setq comint-process-echoes t)
  (run-mode-hooks 'inferior-lfe-mode-hook))

(defun lfe-input-filter (str)
  "Predicate for filtering additions to input history.
Return nil if `STR` matches `inferior-lfe-filter-regexp', otherwise t."
  (not (string-match inferior-lfe-filter-regexp str)))

(defun lfe-get-old-input ()
  "Return a string containing the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

;;;###autoload
(defun inferior-lfe (cmd)
  "Run an inferior LFE process, input and output via a buffer `*inferior-lfe*'.
If `CMD' is given, use it to start the shell, otherwise:
`inferior-lfe-program' `inferior-lfe-program-options' -env TERM vt100."
  ;; (interactive (list (if current-prefix-arg
  ;;                        (read-string "Run LFE: " inferior-lfe-program)
  ;;                      inferior-lfe-program)))
  ;; (if (not (comint-check-proc "*inferior-lfe*"))
  ;;     (let ((cmdlist (split-string cmd)))
  ;;       (set-buffer (apply (function make-comint)
  ;;                          "inferior-lfe" (car cmdlist) nil (cdr cmdlist)))
  ;;       (inferior-lfe-mode)))
  (interactive (list (if current-prefix-arg
                         (read-string "Run LFE: ")
                       ())))
  (let (prog opts)
    (if cmd
        (setq prog "sh"
              opts (list "-i" "-c" cmd))
      (setq prog inferior-lfe-program
            opts (append inferior-lfe-program-options
                         '("-env" "TERM" "vt100"))))
    (unless (comint-check-proc "*inferior-lfe*")
      (set-buffer (apply (function make-comint)
                         "inferior-lfe" prog nil opts))
      (inferior-lfe-mode))
    (setq inferior-lfe-buffer "*inferior-lfe*")
    (pop-to-buffer "*inferior-lfe*")))

;; (apply (function make-comint)
;;        "inferior-lfe" "sh" nil
;;        (quote ("-i" "-c" ". /Users/rv/.bashrc ; lfe -env TERM vt100")))

;;;###autoload
(defalias 'run-lfe 'inferior-lfe)

(defun lfe-eval-region (start end &optional and-go)
  "Send the current region (from `START' to `END') to the inferior LFE process.
`AND-GO' means switch to the LFE buffer afterwards."
  (interactive "r\nP")
  (comint-send-region (inferior-lfe-proc) start end)
  (comint-send-string (inferior-lfe-proc) "\n")
  (if and-go (switch-to-lfe t)))

(defun lfe-eval-last-sexp (&optional and-go)
  "Send the previous sexp to the inferior LFE process.
`AND-GO' means switch to the LFE buffer afterwards."
  (interactive "P")
  (lfe-eval-region (save-excursion (backward-sexp) (point)) (point) and-go))

(defun switch-to-lfe (eob-p)
  "Switch to the inferior Lisp process buffer.
When `EOB-P' is given, position cursor at end of buffer."
  (interactive "P")
  (if (get-buffer-process inferior-lfe-buffer)
      (let ((pop-up-frames
             ;; Be willing to use another frame
             ;; that already has the window in it.
             (or pop-up-frames
                 (get-buffer-window inferior-lfe-buffer t))))
        (pop-to-buffer inferior-lfe-buffer))
    (run-lfe inferior-lfe-program))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defun inferior-lfe-clear-buffer ()
  "Delete the output generated by the LFE process."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun inferior-lfe-proc ()
  "Get the LFE subprocess."
  (let ((proc (get-buffer-process (if (eq major-mode 'inferior-lfe-mode)
                                      (current-buffer)
                                    inferior-lfe-buffer))))
    (or proc
        (error "No LFE subprocess; see variable `inferior-lfe-buffer'"))))

;; The end.
(provide 'inferior-lfe)

(defvar inferior-lfe-load-hook nil
  "*Functions to run when Inferior LFE mode is loaded.")

(run-hooks 'inferior-lfe-load-hook)
;;; inferior-lfe.el ends here
