;;; amber-cc.el --- summary -*- lexical-binding: t -*-

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

;; Add suport for CC in Amber Emacs.

;;; Code:

(use-package cmake-mode)

(use-package disaster
  :general
  (amber/leader-keys c-mode-base-map
	"C-d" '(disaster :wk "dissasemble")))

(use-package demangle-mode
  :hook ((llvm-mode . demangle-mode)
		 (compilation-minor-mode . demangle-mode)))

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(provide 'amber-cc)

;;; amber-cc.el ends here
