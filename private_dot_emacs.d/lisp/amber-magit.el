(require 'use-package)

(use-package transient)

(use-package magit
  :after transient
  :general
  (amber/leader-keys
    "g" '(:ignore t :wk "git")
    "gg" '(magit-status :wk "git status"))
  :config
  (setq magit-diff-refine-hunk 'all))

(use-package forge
  :after magit)

(provide 'amber-magit)
