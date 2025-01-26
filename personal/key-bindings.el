(key-chord-define-global ",," 'execute-extended-command)

(defun my/treemacs-toggle ()
  "Toggle Treemacs. If Treemacs is open, select the window. If in Treemacs buffer, close it."
  (interactive)
  (require 'treemacs)
  (if (treemacs-is-treemacs-window-selected?)
      (delete-window)
    (if (treemacs-get-local-window)
        (treemacs-select-window)
      (treemacs))))

(global-set-key (kbd "C-1") 'my/treemacs-toggle)
(global-set-key (kbd "C-3") 'magit-status)

(define-key smartparens-mode-map (kbd "M-?") nil) ; Unbind smartparen's sp-convolute-sexp for xref-find-ref
(global-unset-key (kbd "C-z")) ; Unbind suspend frame as I accidentally press it a lot

(global-unset-key (kbd "M-.")) ; Unbind xref find def to robe's jump.
(global-set-key (kbd "M-.") 'robe-jump)

(global-unset-key (kbd "M-}")) ;; unbind next/previous paragraph to map to M-p, M-n.
(global-unset-key (kbd "M-{"))
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; Kill current buffer (instead of asking first buffer name)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
