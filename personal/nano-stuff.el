(defface bookmark-menu-heading
  `((((class color) (min-colors 89)) (:foreground ";000000")))
  "workaround")

;; Display org properties in the agenda buffer (modified version)
(straight-use-package
 '(org-agenda-property :type git :host github :repo "Malabarba/org-agenda-property"
                       :fork (:host github :repo "rougier/org-agenda-property")))

;; NANO theme
;; (straight-use-package
;;  '(nano-theme :type git :host github :repo "rougier/nano-theme"))

;; NANO modeline
(straight-use-package
 '(nano-modeline :type git :host github :repo "rougier/nano-modeline" :branch "rewrite"))

;; NANO agenda
;; (straight-use-package
;;  '(nano-agenda :type git :host github :repo "rougier/nano-agenda"))

(setq-default window-divider-default-right-width 10
              window-divider-default-bottom-width 12 ;; idk why but 10 doesn't matches the right side's width.
              window-divider-default-places t
              left-margin-width 0
              right-margin-width 0
              window-combination-resize nil) ; Do not resize windows proportionally

(set-face-attribute 'window-divider nil :foreground (face-background 'default))
;; (set-face-attribute 'window-divider-first-pixel nil :foreground (face-background 'default))
;; (set-face-attribute 'window-divider-last-pixel nil :foreground (face-background 'default))

(window-divider-mode 1)

(setq default-frame-alist
      (append (list
               '(min-height . 1)  '(height . 45)
               '(min-width  . 1)  '(width  . 81)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 20)
               '(left-fringe . 0)
               '(right-fringe . 0)
               '(undecorated-round . t) ;; emacs-plu@29 only
               '(scroll-bar-mode . -1)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))

(scroll-bar-mode -1)                    ; No scroll bars
(tool-bar-mode -1)                      ; No toolbar
(menu-bar-mode 1)

(require 'nano-modeline)

(add-hook 'after-change-major-mode-hook
          (lambda ()
            (nano-modeline
             (cond
              ((derived-mode-p 'vterm-mode 'term-mode) nano-modeline-format-terminal)
              ((derived-mode-p 'calendar-mode) nano-modeline-format-calendar)
              ((derived-mode-p 'org-capture-mode) nano-modeline-format-org-capture)
              ((derived-mode-p 'org-mode) nano-modeline-format-org-lookup)
              ((derived-mode-p 'elpher-mode) nano-modeline-format-elpher)
              ((derived-mode-p 'nano-agenda-mode) nano-modeline-format-nano-agenda)
              ((derived-mode-p 'elfeed-search-mode) nano-modeline-format-elfeed-search)
              ((derived-mode-p 'elfeed-show-mode) nano-modeline-format-elfeed-entry)
              ((derived-mode-p 'mu4e-headers-mode) nano-modeline-format-mu4e-headers)
              ((derived-mode-p 'mu4e-view-mode) nano-modeline-format-mu4e-message)
              ((derived-mode-p 'mu4e-compose-mode) nano-modeline-format-mu4e-compose)
              (t nano-modeline-format-default)))))

(add-hook 'emacs-startup-hook
          (lambda ()
            (with-current-buffer "*Messages*"
              (nano-modeline nano-modeline-format-default))))

;; This adds a box around all buffer
(require 'nano-box)
(add-hook 'after-change-major-mode-hook #'nano-box-on)
