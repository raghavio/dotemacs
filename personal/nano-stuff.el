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
(set-face-attribute 'window-divider-first-pixel nil :foreground (face-background 'default))
(set-face-attribute 'window-divider-last-pixel nil :foreground (face-background 'default))

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

;;Install the modeline for all prog buffers:
(add-hook 'prog-mode-hook
   (lambda () (nano-modeline nano-modeline-format-default)))
(add-hook 'text-mode-hook (lambda () (nano-modeline nano-modeline-format-default)))
(add-hook 'org-mode-hook (lambda () (nano-modeline nano-modeline-format-org-lookup)))

;; (add-hook 'org-mode-hook             #'nano-modeline-org-mode)
;; (add-hook 'pdf-view-mode-hook        #'nano-modeline-pdf-mode)
;; (add-hook 'mu4e-headers-mode-hook    #'nano-modeline-mu4e-headers-mode)
;; (add-hook 'mu4e-view-mode-hook       #'nano-modeline-mu4e-message-mode)
;; (add-hook 'elfeed-show-mode-hook     #'nano-modeline-elfeed-entry-mode)
;; (add-hook 'elfeed-search-mode-hook   #'nano-modeline-elfeed-search-mode)
(add-hook 'vterm-mode-hook (lambda () (nano-modeline nano-modeline-format-terminal)))
(add-hook 'term-mode-hook (lambda () (nano-modeline nano-modeline-format-terminal)))
;; (add-hook 'xwidget-webkit-mode-hook  #'nano-modeline-xwidget-mode)
;; (add-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)
;; (add-hook 'org-capture-mode-hook     #'nano-modeline-org-capture-mode)
;; (add-hook 'org-agenda-mode-hook      #'nano-modeline-org-agenda-mode)

;; This adds a box around all buffer
(require 'nano-box)
(add-hook 'after-change-major-mode-hook #'nano-box-on)
