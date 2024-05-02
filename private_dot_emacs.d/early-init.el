;; Early Init configuration

(show-paren-mode 1)

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(defun amber/remove-margin ()
  "Remove all margins for local buffer.  Meant to be used with a hook."
  (setq-local left-margin-width 0
              right-margin-width 0))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'display-line-numbers-mode-hook #'amber/remove-margin)
(column-number-mode)

(defalias 'yes-or-no-p #'y-or-n-p)

(setq inhibit-startup-screen t)

(let* ((cache-dir (or (getenv "XDG_CACHE_HOME")
                    (concat (getenv "HOME") "/.cache")))
        (auto-save-dir (concat cache-dir "/emacs/autosave/"))
        (backup-dir (concat cache-dir "/emacs/backup/")))
  (make-directory auto-save-dir t)
  (make-directory backup-dir t)
  (setq
    auto-save-list-file-prefix auto-save-dir
    auto-save-file-name-transforms
    `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,(concat auto-save-dir "/tramp-\\2") t)
       (".*" ,auto-save-dir t))
    backup-directory-alist `(("." . ,backup-dir))
    backup-by-copying t))

(setq create-lockfiles nil)

(setq read-process-output-max (* 1024 1024))
(require 'gcmh)
(gcmh-mode 1)

(setq-default fringes-outside-margins t)
(setq-default left-margin-width 1)
(setq-default right-margin-width 1)

(set-fringe-mode 0)

(setq use-dialog-box nil)
(setq use-file-dialog nil)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
