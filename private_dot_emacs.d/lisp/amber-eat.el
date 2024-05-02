;;; amber-eat.el --- Embed terminal emulator in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Bastien Riviere

;; Author: Bastien Riviere <me@babariviere.com>
;; Keywords: terminals

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

;; eat package into amber-emacs

;;; Code:

(use-package eat
  :hook ((eshell-load . eat-eshell-mode))
  :config
  (setq eshell-visual-commands nil))

(provide 'amber-eat)
;;; amber-eat.el ends here
