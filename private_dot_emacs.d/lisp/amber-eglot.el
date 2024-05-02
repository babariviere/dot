;;; amber-eglot.el --- summary -*- lexical-binding: t -*-

;; Author:
;; Maintainer:
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs
;; This program is free software: you can redistribute it and/or modify
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

;; LSP support to Amber Emacs.

;;; Code:

(when (featurep 'amber-lsp)
  (error "`amber-eglot' cannot be loaded with `amber-lsp'"))

(require 'amber-check)

(defalias 'amber/lsp 'eglot-ensure)

(defun amber-eglot/eldoc ()
  "Setup `eldoc-documentation-strategy'."
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose))

(defun amber-eglot/project-try-lsp (dir)
  (when-let ((found (locate-dominating-file dir ".lsp")))
    (cons 'vc found)))

(defun amber-eglot/try-override-root (orig-fun &rest args)
  (let ((project-find-functions
          (cons 'amber-eglot/project-try-lsp project-find-functions)))
    (apply orig-fun args)))

(defun amber-eglot/capf ()
  "CAPF configuration for eglot."
  (setq-local completion-at-point-functions
    (list (cape-capf-super
            #'eglot-completion-at-point
            #'tempel-expand
            #'cape-file))))

(use-package eglot
  :hook
  ((eglot-managed-mode . amber-eglot/eldoc)
   (eglot-managed-mode . amber-eglot/capf))
  :config
  (setq completion-category-overrides '((eglot (styles orderless))))
  (advice-add 'eglot-ensure :around #'amber-eglot/try-override-root)
  (with-eval-after-load 'cape
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))
  :custom
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 1.0)
  (eglot-ignored-server-capabilities '(:documentHighlightProvider :documentOnTypeFormattingProvider))
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref t)
  :bind
  (:map eglot-mode-map
    ("C-c l R" . eglot-reconnect)
    ("C-c l S" . eglot-shutdown)
    ("C-c l a" . eglot-code-actions)
    ("C-c l f" . eglot-format)
    ("C-c l h" . eglot)
    ("C-c l o" . eglot-code-action-organize-imports)
    ("C-c l r" . eglot-rename)))

;; replace HOME from container by our current home if a file is not found during xref.
(eval-after-load "xref"
  '(defun xref-make-file-location (file line column)
     (if (not (file-exists-p file))
         (make-instance 'xref-file-location :file (format "%s/%s" (getenv "HOME") (string-trim file "/.+/.+/")) :line line :column column)
       (make-instance 'xref-file-location :file file :line line :column column))))

(provide 'amber-eglot)

;;; amber-eglot.el ends here
