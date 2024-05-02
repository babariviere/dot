(require 'use-package)

(setq read-extended-command-predicate
      #'command-completion-default-include-p)

(defun amber-corfu/on-meow-exit ()
  (corfu-quit))

(defun amber-corfu/smart-ret ()
  "Insert RET when no selection on corfu, otherwise insert selection."
  (interactive)
  (if (>= corfu--index 0)
    (corfu--insert 'finished)
    (progn
      (corfu-quit)
      (message "newline")
      (newline))))

(use-package corfu
  :ensure (corfu :files (:defaults "extensions/*"))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-on-exact-match nil)
  (corfu-preselect 'prompt)
  (corfu-preview-current nil)
  :hook
  ((meow-insert-exit . amber-corfu/on-meow-exit)
   (corfu-mode . corfu-popupinfo-mode))
  :bind
  (:map corfu-map
    ("TAB" . corfu-next)
    ([tab] . corfu-next)
    ("S-TAB" . corfu-previous)
    ([backtab] . corfu-previous)
    ("RET" . amber-corfu/smart-ret))
  :init
  (global-corfu-mode)
  :config
  (add-to-list 'corfu-continue-commands 'amber-corfu/smart-ret)
  (add-hook 'eshell-mode-hook
    (lambda ()
      (setq-local corfu-quit-at-boundary t
        corfu-quit-no-match t
        corfu-auto nil
        tab-always-indent 'complete)
      (corfu-mode))))

(use-package cape
  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)

  ;; Silence the pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package kind-icon
  :if (display-graphic-p)
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package pcmpl-args)

(provide 'amber-corfu)
