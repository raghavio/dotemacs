;; nano-emacs.el --- NANO Emacs (minimal version)     -*- lexical-binding: t -*-

;; Copyright (c) 2025  Nicolas P. Rougier
;; Released under the GNU General Public License 3.0
;; Author: Nicolas P. Rougier <nicolas.rougier@inria.fr>
;; URL: https://github.com/rougier/nano-emacs

;; This is NANO Emacs in 256 lines, without any dependency 
;; Usage (command line):  emacs -Q -l nano.el -[light|dark]

;; --- Speed benchmarking -----------------------------------------------------
(setq init-start-time (current-time))

;; --- Typography stack -------------------------------------------------------
(set-face-attribute 'default nil
                    :height 140 :weight 'light :family "JetBrains Mono")
(set-face-attribute 'bold nil :weight 'regular)
(set-face-attribute 'bold-italic nil :weight 'regular)
(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

;; --- Frame / windows layout & behavior --------------------------------------
(setq default-frame-alist
      '((left-fringe . 0) (right-fringe . 0)
        (internal-border-width . 16) (vertical-scroll-bars . nil)
        (bottom-divider-width . 0) (right-divider-width . 0)))
(modify-frame-parameters nil default-frame-alist)
(setq-default pop-up-windows nil)

;; --- Activate / Deactivate modes --------------------------------------------
(tool-bar-mode -1) (menu-bar-mode -1) (blink-cursor-mode -1)
(global-hl-line-mode 1)
;; (icomplete-vertical-mode 1) ;; DISABLED - conflicts with Vertico
(pixel-scroll-precision-mode 1)

;; --- Theme code removed - use ef-themes or other themes instead -------------
;; The NANO theme functionality has been removed to allow using external themes
;; like ef-themes while keeping the NANO modeline and UI features

;; --- Minibuffer completion --------------------------------------------------
;; DISABLED - conflicts with Vertico completion system
;; (setq tab-always-indent 'complete
;;       icomplete-delay-completions-threshold 0
;;       icomplete-compute-delay 0
;;       icomplete-show-matches-on-no-input t
;;       icomplete-hide-common-prefix nil
;;       icomplete-prospects-height 9
;;       icomplete-separator " . "
;;       icomplete-with-completion-tables t
;;       icomplete-in-buffer t
;;       icomplete-max-delay-chars 0
;;       icomplete-scroll t
;;       resize-mini-windows 'grow-only
;;       icomplete-matches-format nil)
;; (bind-key "TAB" #'icomplete-force-complete icomplete-minibuffer-map)
;; (bind-key "RET" #'icomplete-force-complete-and-exit icomplete-minibuffer-map)

;; --- Minimal key bindings ---------------------------------------------------
(defun nano-quit ()
  "Quit minibuffer from anywhere (code from Protesilaos Stavrou)"

  (interactive)
  (cond ((region-active-p) (keyboard-quit))
        ((derived-mode-p 'completion-list-mode) (delete-completion-window))
        ((> (minibuffer-depth) 0) (abort-recursive-edit))
        (t (keyboard-quit))))

(defun nano-kill ()
  "Delete frame or kill emacs if there is only one frame left"

  (interactive)
  (condition-case nil
      (delete-frame)
    (error (save-buffers-kill-terminal))))

(bind-key "C-x k" #'kill-current-buffer)
(bind-key "C-x C-c" #'nano-kill)
(bind-key "C-x C-r" #'recentf-open)
(bind-key "C-g" #'nano-quit)
(bind-key "M-n" #'make-frame)
(bind-key "C-z"  nil) ;; No suspend frame
(bind-key "C-<wheel-up>" nil) ;; No text resize via mouse scroll
(bind-key "C-<wheel-down>" nil) ;; No text resize via mouse scroll

;; --- Sane settings ----------------------------------------------------------
(set-default-coding-systems 'utf-8)
(setq-default indent-tabs-mode nil
              ring-bell-function 'ignore
              select-enable-clipboard t)

;; --- OSX Specific -----------------------------------------------------------
(when (eq system-type 'darwin)
  (select-frame-set-input-focus (selected-frame))
  (setq mac-option-modifier nil
        ns-function-modifier 'super
        mac-right-command-modifier 'hyper
        mac-right-option-modifier 'alt
        mac-command-modifier 'meta))

;; --- Header & mode lines ----------------------------------------------------
(setq-default mode-line-format "")

;; Define faces for header-line prefix indicators with backgrounds
(defface nano-header-rw
  '((t :foreground "white" :background "#B4A4DE"))
  "Face for RW (read-write) indicator in header-line")

(defface nano-header-ro
  '((t :foreground "white" :background "#90A4AE"))
  "Face for RO (read-only) indicator in header-line")

(defface nano-header-modified
  '((t :foreground "white" :background "#FF6F00"))
  "Face for ** (modified) indicator in header-line")

(setq-default header-line-format
  '(:eval
    (let* ((prefix (cond (buffer-read-only     '("RO" . nano-header-ro))
                         ((buffer-modified-p)  '("**" . nano-header-modified))
                         (t                    '("RW" . nano-header-rw))))
           (mode (concat "(" (downcase (cond ((consp mode-name) (car mode-name))
                                             ((stringp mode-name) mode-name)
                                             (t "unknow")))
                         " mode)"))
           ;; Get git branch if available from vc-mode
           ;; Strip the VC backend prefix (e.g., "Git-" or "Git:")
           (branch (when (and vc-mode buffer-file-name)
                     (let ((backend (vc-backend buffer-file-name)))
                       (when backend
                         (substring-no-properties vc-mode
                                                  (+ (if (eq backend 'Hg) 2 3) 2))))))
           (branch-info (if branch (concat " [#" branch "]") ""))
           (coords (format-mode-line "%c:%l ")))
      (list
       (propertize " " 'face (cdr prefix)  'display '(raise -0.25))
       (propertize (car prefix) 'face (cdr prefix))
       (propertize " " 'face (cdr prefix) 'display '(raise +0.25))
       (propertize (format-mode-line " %b ") 'face 'bold)
       (propertize mode 'face 'header-line)
       (propertize branch-info 'face 'font-lock-constant-face)
       (propertize " " 'display `(space :align-to (- right ,(length coords))))
       (propertize coords 'face 'shadow)))))

;; Note: Bottom mode-line hiding is handled in config.org after theme loads
;; to ensure theme colors are properly applied to header-line first

;; --- Minibuffer setup -------------------------------------------------------
;; Simplified to avoid conflicts with Vertico
(defun nano-minibuffer--setup ()
  (set-window-margins nil 3 0)
  (setq truncate-lines t))
(add-hook 'minibuffer-setup-hook #'nano-minibuffer--setup)

;; --- Speed benchmarking -----------------------------------------------------
(let ((init-time (float-time (time-subtract (current-time) init-start-time)))
      (total-time (string-to-number (emacs-init-time "%f"))))
  (message (concat
    (propertize "Startup time: " 'face 'bold)
    (format "%.2fs " init-time)
    (propertize (format "(+ %.2fs system time)"
                        (- total-time init-time)) 'face 'shadow))))
