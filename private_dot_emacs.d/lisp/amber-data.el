;;; amber-data.el --- support for data formats -*- lexical-binding: t -*-

;; Author: Bastien Rivière
;; Maintainer: Bastien Rivière
;; Package-Requires: (yaml-mode)


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

;; Add support for various data languages.

;;; Code:

(require 'use-package)

(use-package yaml-mode
  ;; :hook (yaml-mode . amber/lsp)
  )

(defun amber/gitlab-inject-api-token (fn &rest args)
  "Inject gitlab-ci-api-token using authinfo."
  (let* ((auth-source (car (auth-source-search :host "gitlab.com/api/v4" :requires '(secret))))
		 (gitlab-ci-api-token (funcall (plist-get auth-source :secret))))
	(apply fn args)))

(use-package gitlab-ci-mode
  :mode ("\\.gitlab-ci.yml\\'" . gitlab-ci-mode)
  :general
  (amber/leader-keys gitlab-ci-mode
	"C-l" '(gitlab-ci-lint :wk "lint"))
  :init
  (advice-add 'gitlab-ci-lint :around #'amber/gitlab-inject-api-token))

(use-package caddyfile-mode
  :mode (("Caddyfile\\'" . caddyfile-mode)
         ("caddy\\.conf\\'" . caddyfile-mode)))

(define-derived-mode tiltfile-mode bazel-starlark-mode
  "Tiltfile")

(add-to-list 'auto-mode-alist '("Tiltfile$" . tiltfile-mode))

(provide 'amber-data)

;;; amber-data.el ends here
