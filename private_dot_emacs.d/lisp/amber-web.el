;;; amber-web.el --- Web mode support                -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Bastien Riviere

;; Author: Bastien Riviere <me@babariviere.com>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun amber-web/setup-web-mode-for-emacs-client ()
  "Setup some values of web mode for emacs client."
  (if (display-graphic-p)
    (setq
      web-mode-enable-auto-closing t
      web-mode-enable-auto-pairing nil ;; we have electric-pair-mode
      web-mode-enable-auto-indentation t
      web-mode-enable-auto-opening t
      web-mode-enable-auto-quoting t
      web-mode-enable-css-colorization t)))

(add-hook 'server-after-make-frame-hook #'amber-web/setup-web-mode-for-emacs-client)

(use-package web-mode
  :mode ("\\.gohtml\\'" "\\.html?\\'")
  :config
  (setq web-mode-engines-alist
    '(("svelte" . "\\.svelte\\'"))))

(use-package emmet-mode
  :hook (web-mode . emmet-mode))

(use-package web-mode
  :init
  ;; Svelte
  (define-derived-mode amber-svelte-mode web-mode "Svelte+"
    "A major mode derived from web-mode, for editing .svelte files with LSP support.")
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . amber-svelte-mode))
  (add-hook 'amber-svelte-mode-hook #'eglot-ensure)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(amber-svelte-mode . ("svelteserver" "--stdio")))))

(provide 'amber-web)
;;; amber-web.el ends here
