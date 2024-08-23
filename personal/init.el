(set-language-environment "UTF-8")

;; Prelude's line number setting enables it for all buffers (treemacs etc.). I only want it for programming buffers.
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Font and spacing
(set-frame-font "JetBrains Mono-13" nil t)
(setq-default line-spacing 0.2)

;; Theme
(require 'ef-themes)
(setq ef-themes-to-toggle '(ef-melissa-light ef-duo-dark))
(mapc #'disable-theme custom-enabled-themes)
(ef-themes-select 'ef-melissa-light)

;; Eglot
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp")))

(add-hook 'prog-mode-hook #'eglot-ensure) ;; Auto start Eglot
;; Jarchive teaches emacs how to open project dependencies that reside inside jar files.
(use-package jarchive
  :ensure t
  :after eglot
  :config
  (jarchive-mode))

;; Disable line length highlighting
(setq prelude-whitespace nil)

;; Enable YASnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Enable Solaire - Makes the code buffer slightly different to distinguish from popups, sidebars and log buffers.
(solaire-global-mode +1)

(use-package treemacs
  :ensure t
  :defer t
  :bind (:map global-map
              ("C-1" . 'treemacs)
              ("C-<f8>" . 'treemacs-select-window))
  :config (setq treemacs-width 40
                treemacs-file-event-delay 500 ; default is 2000
                treemacs-file-follow-delay 0.1 ; default is 0.2
                )
  (treemacs-project-follow-mode)
  (treemacs-follow-mode)
  (treemacs-filewatch-mode)) 

(use-package treemacs-projectile
  :after (treemacs projectile))
