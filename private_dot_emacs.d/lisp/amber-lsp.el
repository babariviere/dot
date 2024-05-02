;;; amber-lsp.el --- lsp-mode setup                  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Bastien Riviere

;; Author: Bastien Riviere <me@babariviere.com>
;; Keywords: tools

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

;; lsp-mode setup for Amber Emacs.

;;; Code:

(require 'amber-flycheck)

(defalias 'amber/lsp 'lsp)

(defun amber-lsp/corfu-setup ()
  (setq-local completion-styles '(orderless)
    completion-category-defaults nil))

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((lsp-mode . lsp-enable-which-key-integration)
          (lsp-mode . amber-lsp/corfu-setup))
  :bind
  (:map lsp-command-map
    ("a" . lsp-execute-code-action)
    ("f" . lsp-format-buffer))
  :commands (lsp lsp-deferred))

(use-package lsp-ui :commands lsp-ui-mode)

(provide 'amber-lsp)
;;; amber-lsp.el ends here
