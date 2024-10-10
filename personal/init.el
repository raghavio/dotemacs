(set-language-environment "UTF-8")

(when scroll-bar-mode
  (scroll-bar-mode -1))

;; Prelude's line number setting enables it for all buffers (treemacs etc.). I only want it for programming buffers.
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Font and spacing
(set-frame-font "JetBrains Mono-13" nil t)
(setq-default line-spacing 0.2)

;; Theme
(use-package ef-themes
  :ensure t
  :config
  (setq ef-themes-to-toggle '(ef-melissa-light ef-duo-dark))
  (mapc #'disable-theme custom-enabled-themes)
  (ef-themes-select 'ef-duo-dark))

;; Eglot
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

(use-package nano-modeline
  :ensure t
  :init
  (nano-modeline-prog-mode t)
  :custom
  (nano-modeline-position 'nano-modeline-footer)
  :hook
  (prog-mode           . nano-modeline-prog-mode)
  (text-mode           . nano-modeline-text-mode)
  (org-mode            . nano-modeline-org-mode)
  (pdf-view-mode       . nano-modeline-pdf-mode)
  (mu4e-headers-mode   . nano-modeline-mu4e-headers-mode)
  (mu4e-view-mode      . nano-modeline-mu4e-message-mode)
  (elfeed-show-mode    . nano-modeline-elfeed-entry-mode)
  (elfeed-search-mode  . nano-modeline-elfeed-search-mode)
  (term-mode           . nano-modeline-term-mode)
  (xwidget-webkit-mode . nano-modeline-xwidget-mode)
  (messages-buffer-mode . nano-modeline-message-mode)
  (org-capture-mode    . nano-modeline-org-capture-mode)
  (org-agenda-mode     . nano-modeline-org-agenda-mode))

;; Disable super keybindings
(setq prelude-super-keybindings nil)

(when (eq system-type 'darwin) ;; On mac, use command as ctrl modifier
    (setq mac-command-modifier 'control)
    (setq mac-control-modifier 'super))

(use-package vterm
  :ensure t)

(defvar custom-vterm-buffer nil
  "Buffer used for the bottom vterm.")

(defun custom-toggle-vterm-bottom ()
  "Toggle vterm buffer at the bottom, occupying 20% of the frame.
   Set directory to projectile's project root if available."
  (interactive)
  (if (and custom-vterm-buffer
           (get-buffer-window custom-vterm-buffer))
      (delete-window (get-buffer-window custom-vterm-buffer))
    (let* ((buffer (or custom-vterm-buffer
                       (generate-new-buffer "vterm")))
           (window-height (floor (* 0.2 (frame-height))))
           (project-root (projectile-project-root)))
      (setq custom-vterm-buffer buffer)
      (with-current-buffer buffer
        (unless (eq major-mode 'vterm-mode)
          (vterm-mode))
        (when project-root
          (vterm-send-string (concat "cd " project-root "\n"))))
      (display-buffer-in-side-window
       buffer
       `((side . bottom)
         (slot . 0)
         (window-height . ,window-height)
         (window-parameters . ((no-delete-other-windows . t)))))
      (select-window (get-buffer-window buffer))
      (set-window-text-height (get-buffer-window buffer) window-height))))

(global-set-key (kbd "C-2") 'custom-toggle-vterm-bottom)

(use-package rg
  :ensure t)

;; (use-package robe
;;   :ensure t
;;   :hook ((ruby-mode . robe-mode)
;;          (ruby-ts-mode . robe-mode)))

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
