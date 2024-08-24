(key-chord-define-global ",," 'execute-extended-command)

(defun my/treemacs-toggle ()
  "Toggle Treemacs. If Treemacs is open, select the window. If in Treemacs buffer, close it."
  (interactive)
  (if (treemacs-is-treemacs-window-selected?)
      (delete-window)
    (if (treemacs-get-local-window)
        (treemacs-select-window)
      (treemacs))))

(global-set-key (kbd "C-1") 'my/treemacs-toggle)

(global-set-key (kbd "C-3") 'magit-status)
