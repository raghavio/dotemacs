(set-language-environment "UTF-8")


;; LSP Mode stuff
(use-package lsp-mode
  :ensure t
  :hook ((python-mode . lsp)
         (js-mode . lsp)
         (html-mode . lsp)
         (clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  ;; add paths to your local installation of project mgmt tools, like lein
  (setenv "PATH" (concat
                  "/usr/local/bin" path-separator
                  (getenv "PATH")))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  (setq lsp-clojure-server-command '("/path/to/clojure-lsp"))) ;; Optional: In case `clojure-lsp` is not in your $PATH

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package company
  :ensure t)

;; optional if you want which-key integration
(use-package which-key
  :config
  (which-key-mode))

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-minimum-prefix-length 1
                                        ; lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
                                        ; lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp
      )
