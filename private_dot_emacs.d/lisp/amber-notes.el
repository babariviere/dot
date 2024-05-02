;;; amber-notes.el --- Notes for amber-org           -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Bastien Riviere

;; Author: Bastien Riviere <me@babariviere.com>
;; Keywords: docs

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

(require 'use-package)
(require 'amber-org)

;; TODO: install org-gtd

(defconst amber-notes/capture-template
  "#+title: ${title}\n#+created_at: <%<%Y-%m-%d %a %H:%M>>")

(use-package org-gtd
  :after org
  :demand t
  :custom
  (org-gtd-directory (concat org-directory "gtd"))
  (org-edna-use-inheritance t)
  :bind
  (("C-c d A" . org-gtd-archive-completed-items)
   ("C-c d c" . org-gtd-capture)
   ("C-c d e" . org-gtd-engage)
   ("C-c d p" . org-gtd-process-inbox)
   ("C-c d n" . org-gtd-show-all-next)
   ("C-c d s" . org-gtd-show-stuck-projects)
   :map org-gtd-process-map
   ("C-c c" . org-gtd-choose)))

(with-eval-after-load 'org-roam
  (cl-defmethod org-roam-node-zk-type ((node org-roam-node))
    (file-name-nondirectory
     (directory-file-name
      (file-name-directory (org-roam-node-file node))))))

(use-package org-roam
  :demand t
  :custom
  (org-roam-completion-everywhere nil)
  (org-roam-directory org-directory)
  (org-roam-file-exclude-regexp '("data/" "archive/" "journal/" "refs/"))
  (org-roam-node-display-template "${zk-type}: ${title}")
  (org-roam-capture-templates
   `(("f" "fleeting" plain "%?" :target
      (file+head "fleeting/%<%Y%m%d%H%M>-${slug}.org" ,amber-notes/capture-template))
     ("l" "literature" plain "%?" :target
      (file+head "literature/%<%Y%m%d%H%M>-${slug}.org" ,amber-notes/capture-template))
     ("p" "permanent" plain "%?" :target
      (file+head "permanent/%<%Y%m%d%H%M>-${slug}.org" ,amber-notes/capture-template))
     ("P" "project" plain "%?" :target
      (file+head "project/%<%Y%m%d%H%M>-${slug}.org" ,amber-notes/capture-template))))
  :init
  (setq org-roam-v2-ack t)
  :config
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
		         (display-buffer-in-side-window)
		         (side . right)
		         (slot . 0)
		         (window-width . 0.33)
		         (window-parameters . ((no-delete-other-windows . t)))))
  (org-roam-db-autosync-mode 1)
  :bind
  (("C-c n f" . org-roam-node-find)
   :map org-mode-map
   ("C-c n i" . org-roam-node-insert)))

(use-package org-roam-dailies
  :after org-roam
  :custom
  (org-roam-dailies-directory "journal/"))

(use-package org-roam-protocol
  :after org-roam
  :custom
  (org-roam-capture-ref-templates
   '(("r" "Reference" plain
      "%?"
      :target (file+head "references/${slug}.org"
						 "#+title: ${title}\n\n")
      :unnarrowed t)))
  :init
  (make-directory (concat org-roam-directory "references") t))

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(provide 'amber-notes)
;;; amber-notes.el ends here
