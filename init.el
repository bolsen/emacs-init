;;; init.el --- Custom config for Brian
;;; Commentary:
;;;
;;; Code:

;; Start

;; customize variables_________________________________________________________

(defcustom bpo/emacs-d-path
  (file-name-directory user-init-file)
  "The path of the init dir."
  :type 'string
  :group 'bpo/init)

(defcustom bpo/temp-path
  (concat bpo/emacs-d-path "tmp/")
  "Where tmp files should go."
  :type 'string
  :group 'bpo/init)

(defcustom bpo/savehist-file
  (concat bpo/temp-path "savehist")
  "Where savehist should go."
  :type 'string
  :group 'bpo/init)

(defcustom bpo/backup-dir
  (concat bpo/temp-path ".saves")
  "Where savehist should go."
  :type 'string
  :group 'bpo/init)

(defcustom bpo/custom-file
  (concat bpo/emacs-d-path "custom.el")
  "Where savehist should go."
  :type 'string
  :group 'bpo/init)

(defcustom bpo/whitespace-cleanup-on-save
  t
  "Cleanup whitespace on save."
  :type 'boolean
  :group 'bpo/init)

(defcustom bpo/turn-off-display-line-number-modes
  '(org-mode-hook
    term-mode-hook
    shell-mode-hook
    eshell-mode-hook)
  "List of modes to not show lines."
  :type '(alist :type string)
  :group 'bpo/init)

(defcustom bpo/webjump-sites
  '(
    ("DuckDuckGo" .
     [simple-query "duckduckgo.com" "duckduckgo.com/?q=" ""])
    ("Emacs Wiki" .
     [simple-query "www.emacswiki.org" "www.emacswiki.org/cgi-bin/wiki/"
                   ""]))
  "List of sites for webjump."
  :type '(alist)
  :group 'bpo/init)

(defcustom bpo/theme
  'wombat
  "The theme to use."
  :type 'symbol
  :group 'bpo/init)

(defcustom bpo/projects
  "~/Projects"
  "The primary location for all projects that I am working on."
  :type 'string
  :group 'bpo/init)

(defcustom bpo/emacs-init-repo-path
  (expand-file-name (file-name-concat bpo/projects "emacs-init" "init.el"))
  "The primary location for all projects that I am working on."
  :type 'string
  :group 'bpo/init)

(defcustom bpo/hooks-to-remove-dos-eols
  '(csharp-mode-hook
  js-json-mode-hook
  js-mode-hook
  fundamental-mode-hook
  org-mode-hook
  nxml-mode-hook
  web-mode)
  "Hooks for modes to not show EOLs."
  :type 'list
  :group 'bpo/init)

;; setup required files_________________________________________________________
;; Files and directories need to be created if not they don't exist for fresh installs.

(defun bpo/install-init-file ()
  "Install init file into .emacs.d location.
Having to fumble with symlinks in Windows made me
just do copies of the init file."
  (interactive)
  (copy-file bpo/emacs-init-repo-path bpo/emacs-d-path :ok-if-already-exists t)
  (message "Copied %s to %s" bpo/emacs-init-repo-path bpo/emacs-d-path)
  )

(defun bpo/install-fonts ()
  "This must be done on a fresh install."
  (interactive)
  (all-the-icons-install-fonts t)
  (nerd-icons-install-fonts t))

(defun bpo/create-files (dirs files)
  "Create a LIST of DIRS and create a LIST of FILES."
  (dolist (directory dirs)
    (if (not (file-exists-p directory))
      (make-directory directory)))

  (dolist (file files)
    (if (not (file-exists-p file))
          (with-temp-buffer (write-file file))
          )))

(bpo/create-files (list bpo/temp-path bpo/backup-dir)
                  (list bpo/savehist-file bpo/custom-file))
;; Helper functions________________________________________________________________

;; key is below
(defun bpo/goto-projects-dir (&optional dir)
  "Send to the dired buffer of projects."
  (interactive "DProject: ")
  (if (not dir)
      (dired (expand-file-name bpo/projects))
    (dired dir)))

(defun bpo/copy-init-file ()
  (interactive)
  (let ((repo-path))))

(defun bpo/insert-shrug ()
  (interactive)
  (insert "¯\\_(ツ)_/¯"))

;; setup for package_______________________________________________________________
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; (Initialize use-package on non-linux)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; make sure all packages are downloaded and installed before going forward.
(setq use-package-always-ensure t)

;; setup first package which is GC control.
(use-package gcmh
   :init
   (gcmh-mode +1))
;; end setup for package___________________________________________________________

;; Simplify UI_____________________________________________________________________
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 16)
(menu-bar-mode -1)
(setq visible-bell 1)
(windmove-default-keybindings)
(setq auto-save-default nil)
(setq blink-cursor-mode nil)
(setq column-number-mode t)
(setq auto-save-default nil)
(setq global-visual-line-mode t)
(setq-default line-spacing .3)
(setq make-backup-files nil)

(setq backup-directory-alist `(("." . ,bpo/backup-dir)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      )

(global-auto-revert-mode t)
(column-number-mode t)
(global-display-line-numbers-mode t)

;; saveplace remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" bpo/temp-path))
(save-place-mode 1)


(setq savehist-additional-variables
      '(
        projectile-project-command-history
        kill-ring
        search-ring
        regexp-search-ring)
      savehist-autosave-interval 60
      savehist-file bpo/savehist-file)
(savehist-mode +1)

;; save recent files
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" bpo/temp-path)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)

(recentf-mode +1)
(desktop-save-mode 1)
(visual-line-mode 1)

(require 'tramp)
;; keep in mind known issues with zsh - see emacs wiki
(setq tramp-default-method "ssh")

;; display line numbers for some modes
(dolist (mode bpo/turn-off-display-line-number-modes)
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; text should look nice
(when (eq system-type 'haiku)
  (set-face-attribute :height 2500)
  (set-face-attribute :height 110))

(when (not (eq system-type 'windows-nt))
  (set-face-attribute 'default nil :font "Monospace"))

;; better theme
(load-theme bpo/theme)

;; clean up whitespace on save
(defun bpo/toggle-whitespace-cleanup ()
  "Turn on cleanup of whitespace."
  (if bpo/whitespace-cleanup-on-save
      (add-hook 'after-save-hook #'whitespace-cleanup)
    ))
(add-hook 'prog-mode-hook #'bpo/toggle-whitespace-cleanup)

(setq custom-file bpo/custom-file)
(load custom-file)

(winner-mode +1)

;; for C or C++, where you have to dig up header files.
(global-set-key (kbd "C-c f f") #'ff-find-other-file)

;; do webjumps
(global-set-key (kbd "C-c w") #'webjump)
(setq webjump-sites bpo/webjump-sites)

(when (eq system-type 'windows-nt)
  (setq inhibit-eol-conversion t))

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(when (eq system-type 'windows-nt)
  (dolist (hook bpo/hooks-to-remove-dos-eols)
    (add-hook hook 'remove-dos-eol)))

  (progn
    (add-hook 'csharp-mode-hook 'remove-dos-eol)
    (add-hook 'js-json-mode-hook 'remove-dos-eol)
    (add-hook 'js-mode-hook 'remove-dos-eol)
    (add-hook 'fundamental-mode-hook 'remove-dos-eol)
    (add-hook 'org-mode-hook 'remove-dos-eol)
    (add-hook 'nxml-mode-hook 'remove-dos-eol)
    (add-hook 'web-mode-hook 'remove-dos-eol)
    )

(setq-default indent-tabs-mode nil)

(setq display-buffer-alist
      ;; For sane use of SLY debugger in a contextual window
      '(("\\*sly-db"

         (display-buffer-reuse-mode-window
          display-buffer-below-selected)

         ;; parameters
         (window-height . fit-window-to-buffer)
         (dedicated . t))))

;; end Simplify UI______________________________________________________________
;; package configurations_______________________________________________________

(setq package-install-upgrade-built-in t)

(use-package command-log-mode)
(use-package ivy
  :diminish ;; doesn't show modename in list.
  :config (setq ivy-use-virtual-buffers t)
  :init (ivy-mode)
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)

         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-alt-done)
         ("C-d" . ivy-switch-buffer-kill)

         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill)
         )
  :init
  (ivy-mode +1))

(use-package ivy-rich
  :config
  (ivy-rich-mode 1))

(use-package counsel
  :bind (
         ("M-x" . counsel-M-x)
         ("C-x b" . counsel-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history
          )))

(use-package ivy-avy)

;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1)
;;   :custom (doom-modeline-height 34))

(use-package telephone-line
  :config (setq telephone-line-height 30
                telephone-line-primary-left-separator 'telephone-line-sin-left
                telephone-line-secondary-left-separator 'telephone-line-sin-hollow-left
                telephone-line-primary-right-separator 'telephone-line-sin-right
                telephone-line-secondary-right-separator 'telephone-line-sin-hollow-right
                )
  :init (telephone-line-mode +1))

(use-package all-the-icons)
(use-package nerd-icons)
(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode)
  )

(use-package rainbow-delimiters
  :hook (prog-mode lisp-mode sly-mrepl-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :custom (which-key-idle-delay 0.3))

(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-switch-project-action #'projectile-dired)
  :config
  (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package multiple-cursors
  :ensure t
  :init (multiple-cursors-mode +1)
  :bind (
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package company
  :init (global-company-mode t))

(use-package flycheck
  :init (global-flycheck-mode +1))

(use-package golden-ratio-mode)

(use-package whitespace
  :init
  (setq whitespace-style '(face tabs empty trailing lines-tail)
        whitespace-line-column 500)
  (global-whitespace-mode +1))

(use-package magit)
(use-package git-gutter)

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(defun bpo/treemacs-file-and-symbols ()
  (interactive)
  (treemacs)
  (lsp-treemacs-symbols))


(use-package treemacs)

(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(use-package valign)

(use-package org
  :hook
  (org-mode . valign-mode)
  :config
  (global-set-key (kbd "C-c l") #'org-store-link)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)
  (setq org-todo-keywords '("TODO" "IN-PROGRESS" "ON-HOLD" "DONE")
        org-todo-keyword-faces
        '(
          ("IN-PROGRESS" . (:weight bold :color "yellow"))
          ("ON-HOLD" . (:weight bold :color "white"))
        )
      )
  )

(use-package org-kanban)
(use-package org-download)
(use-package lua-mode)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-csharp-omnisharp-enable-decompilation-support t)
  :hook
  ;; all programming modes
  (python-mode . lsp-deferred)
  (c-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (shell-mode . lsp-deferred)
  (typescript-ts-mode . lsp-deferred)
  (csharp-mode . lsp-deferred)
  (lua-mode . lsp-deferred)

  ;; which-key integration
  (lsp-mode . lsp-enable-which-key-integration)

  :commands (lsp lsp-deferred))
(use-package lsp-ui
  :config
  (setq lsp-ui-sideline-show-code-actions t)
  (define-key lsp-ui-mode-map
              [remap xref-find-definitions]
              #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map
              [remap xref-find-references]
              #'lsp-ui-peek-find-references)
  (lsp-ui-doc-mode 1)
  (setq lsp-ui-doc-enable 1)
  (setq lsp-ui-doc-show-with-cursor 1)
  :commands lsp-ui-mode)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package dap-mode
  :init
  (require 'dap-netcore)
  (setq dap-netcore-download-url "https://github.com/Samsung/netcoredbg/releases/download/3.1.0-1031/netcoredbg-win64.zip")
  )

(use-package smartparens-mode
  :ensure smartparens
  :hook (prog-mode text-mode markdown-mode c-mode c++-mode python-mode emacs-lisp-mode lisp-mode sly-mrepl-mode)
  :init
  ;; load default config

  (setq sp-base-key-bindings 'sp)
  (require 'smartparens-config)

  ;; These are changes related to the fact that I have these bindings burnt into my brain and hate
  ;; that the sp-*slurp* functionality takes over it. Paredit/sp by default breaks ones head too much.
  (define-key smartparens-mode-map (kbd "C-<left>") 'left-word)
  (define-key smartparens-mode-map (kbd "C-<right>") 'right-word)
  (define-key smartparens-mode-map (kbd "C-c C-<left>") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-c C-<right>") 'sp-forward-slurp-sexp)

  (define-key smartparens-mode-map (kbd "M-<backspace>") 'backward-kill-word)
  (define-key smartparens-mode-map (kbd "C-c M-<backspace>") 'sp-unwrap-sexp))

;;  :init
  ;; load default config
;;  (setq sp-base-key-bindings 'sp)
;;  (require 'smartparens-config))

(use-package ag)

(use-package sharper
  :demand t
  :bind
  ("C-c n" . sharper-main-transient))

(use-package csproj-mode)
(use-package fsharp-mode)
(use-package sly)
;; (load "~/quicklisp/log4sly-setup.el")
;; (global-log4sly-mode 1)

(use-package rg)
(setq rg-executable "/usr/bin/rg")

(use-package wgrep)

(use-package vterm)
(use-package multi-vterm :ensure t)

;; Collected key bindings__________________________________________________________

(use-package general
  :config
  (general-define-key
   "C-M-j" 'counsel-switch-buffer
   "C-M-y" 'hydra-text-scale/body
   "C-M-t" 'bpo/treemacs-file-and-symbols
   "C-M-p" 'bpo/goto-projects-dir
   "C-M-i" 'bpo/install-init-file
   "C-M-o" 'bpo/insert-shrug
   ;; org-kanban
   "M-o s" 'org-kanban/shift
   "C-+" 'text-scale-increase
   "C--" 'text-scale-decrease
   "C-(" 'sp-forward-slurp-sexp
   "C-)" 'sp-forward-barf-sexp
   "C-c C-m v" 'multi-vterm
   ))


;; Finish package__________________________________________________________________


;; need:
;; - [x] projectile
;; - [x] magit
;; - [x] lsp/lsp-ui
;; - [] treemacs
;; - [x] gcmh
;; - [] lush?
;; - [x] multiple-cursors
;; - [] pyvenv
;; - [x] smartparens?
;; - [x] whitespace-mode
;; - [x] company
;; - [x] flycheck
;; - [] ag (and ivy help)
;; - [x] some pairing library
