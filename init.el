(setq package-enable-at-startup t)
(setq vc-follow-symlinks t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(sanityinc-tomorrow-night))
 '(fzf/git-files-args "--exclude-standard --cache --others")
 '(tab-bar-show 1))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defvar backup-directory-alist)
(setq backup-directory-alist `(("." . "/tmp/.emacs/backups")))
(defvar auto-save-default)
(setq auto-save-default nil)
(setq-default indent-tabs-mode nil)
(setq use-short-answers t)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default require-final-newline t)

;; Straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'helm)
(straight-use-package 'helm-ls-git)
(straight-use-package 'terraform-mode)
(straight-use-package 'go-mode)
(straight-use-package 'darkroom)
(straight-use-package 'evil)
(straight-use-package 'magit)
(straight-use-package 'yaml-mode)
(straight-use-package 'bnf-mode)
(straight-use-package 'vterm)
(straight-use-package 'switch-window)
(straight-use-package 'dired+)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'color-theme-sanityinc-tomorrow)
(straight-use-package 'tommyh-theme)
(straight-use-package 'diredfl)
(straight-use-package 'panda-theme)
(straight-use-package 'haskell-mode)
(straight-use-package 'pyvenv)
;; (straight-use-package 'elpy)
(straight-use-package 'bufler)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-pyright)
(straight-use-package 'lsp-ui)
(straight-use-package 'company)
(straight-use-package 'company-lsp)
(straight-use-package 'company-box)
(straight-use-package 'helm-lsp)
(straight-use-package 'helm-company)
;; (straight-use-package 'fzf)

(straight-use-package
 '(fzf :type git :host github :repo "bling/fzf.el"
       :fork (:host github :repo "verdude/fzf.el")))

(setq lsp-keymap-prefix "C-c l")

(setq diredp-hide-details-initially-flag nil)
(require 'dired+)
(diredp-toggle-find-file-reuse-dir t)
(diredfl-global-mode)

(setq custom-safe-themes t)
(setq custom-enabled-themes '(sanityinc-tomorrow-night))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)

(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-night))
  (reapply-themes))

(global-set-key (kbd "C-x C-p") 'fzf-git-files)
(global-set-key (kbd "C-c C-p") 'fzf-git-grep)
(global-set-key (kbd "C-x /") 'darkroom-increase-margins)
(global-set-key (kbd "C-x ,") 'darkroom-decrease-margins)
(global-set-key (kbd "C-x C-n") 'display-line-numbers-mode)
(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "C-x 4 0") 'switch-window-then-kill-buffer)
(global-set-key (kbd "C-x 4 d") 'switch-window-then-dired)
(global-set-key (kbd "C-x 4 f") 'switch-window-then-find-file)
(global-set-key (kbd "C-x 4 C-f") 'switch-window-then-find-file)
(global-set-key (kbd "C-x 0") 'switch-window-then-delete)

(global-set-key (kbd "C-x C-g") 'bury-buffer)
(global-set-key (kbd "C-x C-b") 'bufler)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)
(global-set-key (kbd "C-x C-r") 'helm-projects-history)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x RET RET") 'vterm)
(global-set-key (kbd "C-x a |") 'darkroom-tentative-mode)
(global-set-key (kbd "C-c l w s") 'lsp)

(helm-mode 1)
(setq helm-mode-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)
(setq tab-width 2)
(setq org-todo-keywords '("MAYBE" "PLANNING" "WAITING" "DEFERRED" "TODO" "NEXT" "IN-PROGRESS" "CANCELED" "DONE"))
(setq org-log-done 'note)

;; Start off warm and fuzzy
;; courtesy of mr purcell
(setq-default initial-scratch-message
              (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!!\n\n"))

(setq inhibit-startup-screen t)
(setq x-select-enable-clipboard t)
(if (eq system-type 'gnu/linux)
    (set-frame-font "Hermit" nil t)
  (set-face-attribute 'default nil :height 120))

(defun create-lang-repo (dirname)
  (make-directory dirname nil)
  (magit-init dirname)
  (cd dirname)
  (switch-to-buffer-other-frame (create-file-buffer "grammar.bnf")))

(put 'set-goal-column 'disabled nil)

(setq home-dir (expand-file-name "~"))
(add-to-list 'load-path (concat user-emacs-directory "/lib"))

(require 'cpt.el)
(require 'keychain-environment)
(keychain-refresh-environment)
(use-package totp :straight (:host github :repo "juergenhoetzel/emacs-totp" :files ("*.el")))
