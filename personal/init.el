(set-language-environment "UTF-8")

;; Font and spacing
(set-frame-font "JetBrains Mono-13" nil t)
(setq-default line-spacing 0.2)

;; Eglot
(add-hook 'prog-mode-hook #'eglot-ensure) ;; Auto start Eglot for Clojure.
;; Jarchive teaches emacs how to open project dependencies that reside inside jar files.
(use-package jarchive
  :ensure t
  :after eglot
  :config
  (jarchive-mode))
