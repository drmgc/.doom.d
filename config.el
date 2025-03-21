;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;;
;; Adjust garbage collection thresholds during startup, and thereafter
;;
(let ((normal-gc-cons-threshold (* 32 1024 1024))
      (init-gc-cons-threshold (* 512 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  ;; (setq gc-cons-percentage 0.5)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(setq initial-frame-alist
      (append initial-frame-alist
              '((width  . 100)
                (height . 45))))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Sergey Naumov"
      user-mail-address "drmgc.drmgc@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;; (setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
(setq doom-font (font-spec :family "Fira Code" :size 18 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 19))

;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord-light)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(global-visual-line-mode 1)
(global-page-break-lines-mode 1)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq backup-by-copying t)



(use-package! blamer
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 48)
  (blamer-max-commit-message-length 120)
  (blamer-type 'postframe-popup)
  :config
  (map! :after blamer
        :map prog-mode-map
        :leader
        :prefix "g"
        "I" #'blamer-show-commit-info
        "i" #'blamer-show-posframe-commit-info)
  (global-blamer-mode 0))


;; accept completion from copilot and fallback to company
(use-package! copilot
  ;; :hook (prog-mode . copilot-mode)
  :config
  (map! :after copilot
        :leader
        :prefix "C-c"
        "C-t" #'copilot-mode)
  (map! :after copilot
        :map copilot-completion-map
        :leader
        :prefix "C-c"
        "C-c" #'copilot-accept-completion
        "C-w" #'copilot-accept-completion-by-word
        "C-l" #'copilot-accept-completion-by-line
        "C-p" #'copilot-accept-completion-by-paragraph

        "n" #'copilot-next-completion
        "p" #'copilot-previous-completion)
  :bind (:map copilot-completion-map
              ("C-S-<f12>" . 'copilot-accept-completion-by-word)))


(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
(define-key key-translation-map (kbd "C-M-h") (kbd "M-DEL"))
(define-key key-translation-map (kbd "C-?") (kbd "C-h"))


;;
;;; Lsp

(after! lsp-mode
  (setq lsp-print-performance t))

(after! lsp-ui
  (setq lsp-ui-sideline-diagnostic-max-lines 4)
  (setq lsp-ui-doc-show-with-cursor nil)

  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-doc-show-with-mouse t)

  (setq lsp-ui-imenu-auto-refresh t)

  (setq lsp-lens-enable t))

(map! :after lsp-ui
      :map  lsp-ui-mode-map
      :leader
      :prefix "c"
      "I" #'lsp-ui-imenu
      "h" #'lsp-ui-doc-show)



(after! company-mode
  (setq company-idle-delay 0.5))



(after! ispell
  (setq ispell-dictionary "en"))


;;
;;; Yasnippet

(map! :after yasnippet
      :map prog-mode-map
      "C-c TAB" #'yas-expand)


;;
;;; typescript-mode

(use-package! typescript-mode
  :custom
  (typescript-indent-level 2))



;;
;;; plantuml

(use-package! plantuml-mode
  :custom
  (plantuml-indent-level 2))


;;
;;; Subword-mode

(map! :leader
      "[" #'subword-mode
      "] g" #'glasses-mode)


;;
;;; cases

(defun drmgc/space-case (input)
  "Convert any string to foo bar (spaces separated) format."
  (let* ((case-fold-search nil)
         (case-separated (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" input))
         (delimiter-separated (replace-regexp-in-string "[-_\. ]+" " " case-separated)))
    (downcase delimiter-separated)))

(defun drmgc/space-to-pascal-case (input)
  "Convert a space-separated string like 'foo bar baz' to 'FooBarBaz' (PascalCase)."
  (let* ((words (split-string input " "))
         (capitalized-words (mapcar #'capitalize words)))
    (apply #'concat capitalized-words)))

(defun drmgc/pascal-case (input)
  "Convert any string string to PascalCase, e.g. 'FooBarBaz'."
  (drmgc/space-to-pascal-case (drmgc/space-case input)))



;;
;;; EOL symbols

(defun drmgc/insert-at-eol-and-return (str)
  "Insert a STR at the end of current line and return to the original position"
  (let ((original-pos (point)))
    (end-of-line)
    (insert str)
    (goto-char original-pos)))

(defun drmgc/delete-at-eol-and-return ()
  "Delete a char at the end of current line and return to the original position"
  (interactive)
  (let ((original-pos (point)))
    (end-of-line)
    (delete-char -1)
    (goto-char original-pos)))

(setq drmgc/eol-insertables
      '((";" . ";")
        ("," . ",")
        ("i" . " in")))

(defun drmgc/define-insert-at-eol (name shortcut char)
  (let ((fn-name (concat "drmgc/insert-at-eol-and-return/" name)))
    (fset (intern fn-name)
          (lambda ()
            (:documentation (format "Insert %s at the end of current line and return to the original position." char))
            (interactive)
            (drmgc/insert-at-eol-and-return char)))
    (map! :map prog-mode-map
          :prefix "C-c SPC"
          shortcut (symbol-function (intern fn-name)))))

(setq drmgc/eol-insertables;
      '(
        ("semicolon" . (";" ";"))
        ("comma" . ("," ","))
        ("in" . ("i" " in"))
        ))

(map! :map prog-mode-map
      :prefix "C-c SPC"
      "DEL" #'drmgc/delete-at-eol-and-return)

(mapc
 (lambda (args)
   (apply 'drmgc/define-insert-at-eol (car args) (cdr args)))
 drmgc/eol-insertables)


;;
;;; evil-textobj-tree-sitter
(after! evil-textobj-tree-sitter)


;;
;;; tree-surgeon
(use-package tree-surgeon
  :config
  (map! :map prog-mode-map
        :leader
        :prefix "H"
        "j" #'tree-surgeon-split-join)
  :custom
  (tree-surgeon-split-join-settings
   '((json-ts-mode
      (object)
      . none)
     (rust-ts-mode
      (parameters arguments)
      . trailing)
     (python-ts-mode
      (parameters argument_list)
      . trailing)
     (ruby-ts-mode
      (method_parameters argument_list)
      . none)
     (go-ts-mode
      (argument_list parameter_list literal_value)
      . trailing)
     (typescript-ts-mode
      (formal_parameters arguments)
      . trailing)
     (typescript-mode
      (object array arguments formal_parameters union_type type_parameters type_arguments named_imports)))))



(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
;; (setq major-mode-remap-alist
;;  '((yaml-mode . yaml-ts-mode)
;;    (bash-mode . bash-ts-mode)
;;    (js2-mode . js-ts-mode)
;;    (typescript-mode . typescript-ts-mode)
;;    (json-mode . json-ts-mode)))



(use-package scopeline
  :config (add-hook 'prog-mode-hook #'scopeline-mode))


;;
;;; Org-mode

(set-variable 'org-hide-emphasis-markers t)


;;
;;; sh-mode

(use-package! sh-script
  :hook (sh-mode . flymake-mode))


;;
;;; prisma-mode

(use-package! lsp
  :hook (prisma-mode . lsp)
  :init (setq lsp-auto-execute-action nil))


;;
;;; kusto-mode

(use-package! kusto-mode)


;;
;;; difft

(use-package difftastic
  :demand t
  :bind (:map magit-blame-read-only-mode-map
              ("D" . difftastic-magit-show)
              ("S" . difftastic-magit-show))
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)])))


;;
;;; gdscript

;; (defun lsp--gdscript-ignore-errors (original-function &rest args)
;;   "Ignore the error message resulting from Godot not replying to the `JSONRPC' request."
;;   (if (string-equal major-mode "gdscript-mode")
;;       (let ((json-data (nth 0 args)))
;;         (if (and (string= (gethash "jsonrpc" json-data "") "2.0")
;;                  (not (gethash "id" json-data nil))
;;                  (not (gethash "method" json-data nil)))
;;             nil ; (message "Method not found")
;;           (apply original-function args)))
;;     (apply original-function args)))
;; ;; Runs the function `lsp--gdscript-ignore-errors` around `lsp--get-message-type` to suppress unknown notification errors.
;; (advice-add #'lsp--get-message-type :around #'lsp--gdscript-ignore-errors)

;; (use-package gdscript-mode
;;   :custom
;;   (gdscript-use-tab-indents t)
;;   (gdscript-indent-offset 2))


;;
;;; hl-todo


                                        ; TODO
                                        ; FIXME
                                        ; REVIEW
                                        ; HACK
                                        ; DEPRECATED
                                        ; NOTE
                                        ; BUG
                                        ; XXX

(after! hl-todo)


;;
;;; dap-mode templates

(after! dap-mode
  (require 'dap-node)

  (dap-register-debug-template
   "Node::Attach to a running process"
   (list :type "node"
         :request "attach"
         :host "localhost"
         :port 9229
         :cwd "${workspaceFolder}"
         :program "__ignored"
         :continueOnAttach t
         :skipFiles (list "<node_internals>/**" "**/node_modules/**")
         :name "Node::Attach to a running process"))

  (setq dap-print-io t))
