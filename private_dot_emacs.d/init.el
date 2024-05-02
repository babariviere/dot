;; Init configuration
;;; Code:

(require 'package)

(load-file (expand-file-name "elpaca.el" user-emacs-directory))

(setq custom--inhibit-theme-enable nil)

(let (; We want bigger font on MacOS
      (height (if (memq window-system '(mac ns)) 100 90)))
  (custom-set-faces
   `(default ((nil :family "Commit Mono" :height ,height)))
   `(fixed-pitch ((nil :family "Commit Mono")))
   `(org-modern-symbol ((nil :family "Iosevka")))))


(mapc #'disable-theme custom-enabled-themes)

(use-package modus-themes
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil)

  (setq modus-themes-common-palette-overrides
      '((bg-mode-line-active bg-blue-subtle)
        (fg-mode-line-active fg-main)
        (border-mode-line-active bg-blue-subtle)))

  ;; Load the theme of your choice.
  (load-theme 'modus-operandi t)

  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(deftheme amber)
(enable-theme 'amber)
(setq custom-enabled-themes
      (remq 'amber custom-enabled-themes))
(custom-theme-set-variables
 'amber
 '(kaolin-git-gutter-solid t)
 '(kaolin-themes-git-gutter-solid t)
 ;; Fix warning about not being able to determine a suitable EmacsClient
 '(with-editor-emacsclient-executable "emacsclient")
 '(window-divider-default-right-width 8))

(setq-default indent-tabs-mode nil
              tab-width 4)

(when (or (memq window-system '(mac ns x))
	      (daemonp))
  (require 'exec-path-from-shell)
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(require 'auth-source)
(setq auth-sources '("~/.authinfo.gpg")
      user-full-name "Bastien Riviere"
      user-mail-address "me@babariviere.com")

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'amber-keys)             ; must be loaded first

(require 'amber-completion)
(require 'amber-dired)
(require 'amber-lisp)
(require 'amber-magit)
(require 'amber-project)
;; (require 'amber-company)
(require 'amber-corfu)
(require 'amber-eglot)
(require 'amber-tempel)
(require 'amber-electric)
;; (require 'amber-smartparens)
(require 'amber-direnv)
(require 'amber-eat)
;; (require 'amber-vterm)
(require 'amber-format)

(require 'amber-cc)
(require 'amber-common-lisp)
(require 'amber-csharp)
(require 'amber-data)
(require 'amber-docker)
(require 'amber-elisp)
;; (require 'amber-erlang)
(require 'amber-elixir)
(require 'amber-fish)
(require 'amber-go)
(require 'amber-haskell)
(require 'amber-org)
;; (require 'amber-notes)
(require 'amber-nix)
(require 'amber-python)
(require 'amber-rust)
;; (require 'amber-scheme)
(require 'amber-sql)

(require 'amber-ts)
(require 'amber-web)
;; (require 'amber-elfeed)

(require 'use-package)
(require 'diminish)

(use-package doom-modeline
  :disabled
  :config
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-buffer-file-name-style 'relative-from-project))

(use-package eldoc
  :hook (emacs-lisp-mode . eldoc-mode)
  :config
  (diminish 'eldoc-mode))

;; TODO: customize keyword faces
(use-package hl-todo
  :custom
  (hl-todo-highlight-punctuation ":")
  :config
  (global-hl-todo-mode 1))

(use-package diff-hl
  :disabled
  :hook ((dired-mode . diff-hl-dired-mode-unless-remote)
		 (prog-mode . diff-hl-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  ;; use margin instead of fringe
  (define-fringe-bitmap 'amber/diff-hl-fringe [224]
    nil nil '(center repeated))
  (setq diff-hl-fringe-bmp-function (lambda (type pos) 'amber/diff-hl-fringe)
		diff-hl-margin-symbols-alist '((insert . " ")
									   (delete . " ")
									   (change . " ")
									   (unknown . " ")
									   (ignored . " ")))
  (diff-hl-margin-mode))

;; HACK: Fix issue with browse-url and wayland
(defun amber/enforce-display-env (&rest args)
  "Enforce DISPLAY env to be correctly set.

ARGS are the arguments passed to `browse-url`."
  (setenv "DISPLAY" ":0"))

(advice-add 'browse-url :before #'amber/enforce-display-env)

(use-package editorconfig
  :config
  (editorconfig-mode 1)
  (diminish 'editorconfig-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package crux
  :bind
  (([remap move-beginning-of-line] . crux-move-beginning-of-line)
   ([remap kill-whole-line] . crux-kill-whole-line)
   ("<S-return>" . crux-smart-open-line)
   ("<C-S-return>" . crux-smart-open-line-above)))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-separator "/"))

(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame)
  :bind
  ("C-x o" . ace-window))

(use-package avy
  :bind
  ("C-;" . avy-goto-char)
  ("C-'" . avy-goto-char-2)
  ("M-g f" . avy-goto-line)
  ("M-g w" . avy-goto-word-1))

(global-unset-key (kbd "C-z"))

(require 'ediff)
(setq
  ediff-diff-options "-w"
  ediff-split-window-function 'split-window-horizontally
  ediff-window-setup-function 'ediff-setup-windows-plain)

(require 're-builder)
(setq reb-re-syntax 'string)

(setq frame-inhibit-implied-resize t)

(pixel-scroll-precision-mode)
(auto-insert-mode)
(diminish 'auto-revert-mode)
(diminish 'gcmh-mode)

(require 'amber-meow)
