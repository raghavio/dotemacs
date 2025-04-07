(setq auth-sources '("~/.authinfo"))
(add-to-list 'load-path "~/.emacs.d/packages/")
(set-language-environment "UTF-8")

;; Font and spacing
(set-frame-font "JetBrains Mono-16" nil t)
(setq-default line-spacing 0.2)

;; Theme
(use-package ef-themes
  :ensure t
  :config
  (setq ef-themes-to-toggle '(ef-melissa-light ef-duo-dark))
  (mapc #'disable-theme custom-enabled-themes)
  (ef-themes-select 'ef-melissa-light))

;; eglot
(use-package eglot
    ;;:hook (prog-mode . eglot-ensure)
    ;; The first 5 bindings aren't needed here, but are a good
    ;; reminder of what they are bound too
    :bind (("M-TAB" . completion-at-point)
           ("M-g i" . imenu)
           ("C-h ." . display-local-help)
           ("M-." . xref-find-definitions)
           ("M-," . xref-go-back)
           :map
           eglot-mode-map
           ("C-c e a" . eglot-code-actions)
           ("C-c e o" . eglot-code-actions-organize-imports)
           ("C-c e r" . eglot-rename)
           ("C-c e f" . eglot-format))
    :config
    (defvar complete-at-point--timer nil "Timer for triggering complete-at-point.")

    (defun auto-complete-at-point (&rest _)
      "Set a time to complete the current symbol at point in 0.1 seconds"
      (when (and (not (minibufferp)))
        ;; If a user inserts a character while a timer is active, reset
        ;; the current timer
        (when (timerp complete-at-point--timer)
          (cancel-timer complete-at-point--timer))
        (setq complete-at-point--timer
              (run-at-time 0.2 nil
                           (lambda ()
                             ;; Clear out the timer and run
                             ;; completion-at-point
                             (when (timerp complete-at-point--timer)
                               (cancel-timer complete-at-point--timer))
                             (setq complete-at-point--timer nil)
                             (completion-at-point))))))
    ;; Add a hook to enable auto-complete-at-point when eglot is enabled
    ;; this allows use to remove the hook on 'post-self-insert-hook if
    ;; eglot is disabled in the current buffer
    ;; (add-hook 'eglot-managed-mode-hook (lambda ()
    ;;                                      (if eglot--managed-mode
    ;;                                          (add-hook 'post-self-insert-hook #'auto-complete-at-point nil t)
    ;;                                       (remove-hook 'post-self-insert-hook #'auto-complete-at-point t))))
    )

;; Jarchive teaches emacs how to open project dependencies that reside inside jar files.
(use-package jarchive
  :ensure t
  :after eglot
  :config
  (jarchive-mode))

;; Disable line length highlighting
(setq prelude-whitespace nil)

;; Enable YASnippet
(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

;; Enable Solaire - Makes the code buffer slightly different to distinguish from popups, sidebars and log buffers.
;; (solaire-global-mode +1)

(use-package treemacs
  :ensure t
  :defer t
  :config (setq treemacs-width 40
                treemacs-file-event-delay 500 ; default is 2000
                treemacs-file-follow-delay 0.1 ; default is 0.2
                )
  (treemacs-project-follow-mode)
  (treemacs-follow-mode)
  (treemacs-filewatch-mode)) 

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     (ruby "https://github.com/tree-sitter/tree-sitter-ruby")))

(use-package ruby-ts-mode
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :hook (ruby-ts-mode . subword-mode)
  :custom
  (ruby-indent-level 2)
  (ruby-indent-tabs-mode nil))

(use-package inf-ruby
  :ensure t
  :config
  (add-hook 'after-init-hook 'inf-ruby-switch-setup)
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter-and-focus)
  (add-hook 'ruby-base-mode 'inf-ruby-minor-mode)
  (inf-ruby-enable-auto-breakpoint))

(use-package eldoc
  :init
  (global-eldoc-mode))

(use-package markdown-mode
  :ensure t
  :magic "\\.md\\'")

;; (use-package nano-modeline
;;   :ensure t
;;   :init
;;   (nano-modeline-prog-mode t)
;;   :custom
;;   (nano-modeline-position 'nano-modeline-footer)
;;   :hook
;;   (prog-mode           . nano-modeline-prog-mode)
;;   (text-mode           . nano-modeline-text-mode)
;;   (org-mode            . nano-modeline-org-mode)
;;   (pdf-view-mode       . nano-modeline-pdf-mode)
;;   (mu4e-headers-mode   . nano-modeline-mu4e-headers-mode)
;;   (mu4e-view-mode      . nano-modeline-mu4e-message-mode)
;;   (elfeed-show-mode    . nano-modeline-elfeed-entry-mode)
;;   (elfeed-search-mode  . nano-modeline-elfeed-search-mode)
;;   (term-mode           . nano-modeline-term-mode)
;;   (xwidget-webkit-mode . nano-modeline-xwidget-mode)
;;   (messages-buffer-mode . nano-modeline-message-mode)
;;   (org-capture-mode    . nano-modeline-org-capture-mode)
;;   (org-agenda-mode     . nano-modeline-org-agenda-mode))

;; Disable super keybindings
(setq prelude-super-keybindings nil)

(when (eq system-type 'darwin) ;; On mac, use command as ctrl modifier
    (setq mac-command-modifier 'control)
    (setq mac-control-modifier 'super))

(use-package vterm
  :ensure t)

(use-package rg
  :ensure t)

(use-package robe
  :ensure t
  :hook ((ruby-mode . robe-mode)
         (ruby-ts-mode . robe-mode)))

(use-package company
  :ensure t
  :config
  (add-to-list 'company-backends 'company-robe))

;; Add Projectile Rails package
(use-package projectile-rails
  :ensure t
  :after projectile
  :diminish projectile-rails-mode
  :hook ((ruby-mode . projectile-rails-mode)  ;; Enable for ruby-mode
         (ruby-ts-mode . projectile-rails-mode)  ;; Enable for ruby-ts-mode
         (projectile-mode . projectile-rails-global-mode))  ;; Auto-enable for Rails projects
  :config
  ;; Set to 'completing-read for Vertico compatibility
  (setq projectile-rails-completion-system 'completing-read)  ;; Can also be 'helm or 'ivy
  (define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map))

(use-package gptel
  :ensure t)

(use-package forge
  :ensure t
  :after magit)

(use-package rubocop
  :ensure t
  :config
  (add-hook 'ruby-mode-hook #'rubocop-mode)
  (setq rubocop-format-on-save t))

(use-package popper
  :ensure t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*rg\\*"
          "\\*xref\\*"
          "\\*Occur\\*"
          "\\*Backtrace\\*"
          help-mode
          compilation-mode
          "^\\*eshell.*\\*$" eshell-mode
          "^\\*shell.*\\*$"  shell-mode
          "^\\*term.*\\*$"   term-mode
          "^\\*vterm*\\*$"  vterm-mode))
  (popper-mode +1)
  (popper-echo-mode +1) ;; For echo area hints
  (setq popper-window-height 0.25)
  (setq popper-group-function #'popper-group-by-projectile))

(use-package magit-delta
  :ensure t
              :after magit
              :config
              (setq
               magit-delta-default-dark-theme "GitHub"
               magit-delta-default-light-theme "GitHub"
               magit-delta-hide-plus-minus-markers nil)
              (magit-delta-mode))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t))))

;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1))

;; (use-package nerd-icons
;;   :ensure t
;;   :custom (nerd-icons-font-family "RobotoMono Nerd Font"))

(setq projectile-create-missing-test-files t)

(defun run-server ()
  "Runs the Emacs server if it is not running"
  (require 'server)
  (unless (server-running-p)
    (server-start)))

(run-server)

;; (use-package ggtags
;;   :ensure t
;;   :config
;;   (add-hook 'c-mode-common-hook
;;             (lambda ()
;;               (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'ruby-base-mode 'ruby-mode 'ruby-ts-mode)
;;                 (ggtags-mode 1))))
;;   ;; Optional: Make ggtags the default backend for xref
;;   (add-to-list 'xref-backend-functions 'ggtags--xref-backend))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package aidermacs
  :straight (:host github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :config
  (setq aidermacs-default-model "sonnet")
  (global-set-key (kbd "C-c c") 'aidermacs-transient-menu) ; a is for agenda. remembering c for cursor, idk.
  ; Enable minor mode for Aider files
  (aidermacs-setup-minor-mode)
  ; See the Configuration section below
  (setq aidermacs-use-architect-mode t)
  (setq aidermacs-backend 'vterm))
