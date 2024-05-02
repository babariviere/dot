;;; amber-org.el --- Org support -*- lexical-binding: t -*-

;; Author: Bastien Rivière
;; Maintainer: Bastien Rivière
;; Version: version
;; Package-Requires: (org evil-org org-contrib org-agenda org-superstar org-capture org-appear)
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

;; Add org-mode support to amber emacs.

;;; Code:

(require 'use-package)

;;
;; Functions
;;

;; A slightly modified version of doom's one.
;; Source: https://github.com/hlissner/doom-emacs/blob/bf8495b4122701fb30cb6cea37281dc8f3bedcd0/modules/lang/org/autoload/org.el#L125
(defun amber-org/dwin-at-point (&optional arg)
  "Do-what-I-mean at point (org mode)."
  (interactive "P")
  (if (button-at (point))
      (call-interactively #'push-button)
    (let* ((context (org-element-context))
		   (type (org-element-type context)))
      ;; skip over unimportant contexts
      (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
		(setq context (org-element-property :parent context)
			  type (org-element-type context)))
      (pcase type
		(`headline
		 (cond ((memq (bound-and-true-p org-goto-map)
					  (current-active-maps))
				(org-goto-ret))
			   ((and (fboundp 'toc-org-insert-toc)
					 (member "TOC" (org-get-tags)))
				(toc-org-insert-toc)
				(message "Updating table of contents"))
			   ((string= "ARCHIVE" (car-safe (org-get-tags)))
				(org-force-cycle-archived))
			   ((or (org-element-property :todo-type context)
					(org-element-property :scheduled context))
				(let ((org-use-fast-todo-selection nil))
				  (org-todo))))
		 ;; Update any metadata
		 (org-update-checkbox-count)
		 (org-update-parent-todo-statistics)
		 (when (and (fboundp 'toc-org-insert-toc)
					(member "TOC" (org-get-tags)))
		   (toc-org-insert-toc)
		   (message "Updating table of contents")))

		(`clock (org-clock-update-time-maybe))

		(`footnote-reference
		 (org-footnote-goto-definition (org-element-property :label context)))

		(`footnote-definition
		 (org-footnote-goto-previous-reference (org-element-property :label context)))

		((or `planning `timestamp)
		 (org-follow-timestamp-link))

		((or `table `table-row)
		 (if (org-at-TBLFM-p)
			 (org-table-calc-current-TBLFM)
		   (ignore-errors
			 (save-excursion
			   (goto-char (org-element-property :contents-begin context))
			   (org-call-with-arg 'org-table-recalculate (or arg t))))))

		(`table-cell
		 (org-table-blank-field)
		 (org-table-recalculate arg)
		 (when (and (string-empty-p (string-trim (org-table-get-field)))
					(bound-and-true-p evil-local-mode))
		   (evil-change-state 'insert)))

		(`babel-call
		 (org-babel-lob-execute-maybe))

		(`statistics-cookie
		 (save-excursion (org-update-statistics-cookies arg)))

		((or `src-block `inline-src-block)
		 (org-babel-execute-src-block arg))

		((or `latex-fragment `latex-environment)
		 (org-latex-preview arg))

		(`link
		 (let* ((lineage (org-element-lineage context '(link) t))
				(path (org-element-property :path lineage)))
		   (if (or (equal (org-element-property :type lineage) "img")
				   (and path (image-type-from-file-name path)))
			   (+org--toggle-inline-images-in-subtree
				(org-element-property :begin lineage)
				(org-element-property :end lineage))
			 (org-open-at-point arg))))

		((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
		 (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
		   (org-toggle-checkbox (if (equal match "[ ]") '(16)))))))))

(defun amber-org/babel-lazy-load-h ()
  "Load babel libraries lazily when babel blocks are executed."
  (defun amber-org/-babel-lazy-load (lang &optional async)
    (cl-check-type lang (or symbol null))
    (unless (cdr (assq lang org-babel-load-languages))
      (when async
        ;; ob-async has its own agenda for lazy loading packages (in the child
        ;; process), so we only need to make sure it's loaded.
        (require 'ob-async nil t))
      (prog1 (or (require (intern (format "ob-%s" lang)) nil t)
                 (require lang nil t))
        (add-to-list 'org-babel-load-languages (cons lang t)))))

  (defun amber-org/-export-lazy-load-library-h (&optional element)
    "Lazy load a babel package when a block is executed during exporting."
    (amber-org/-babel-lazy-load-library-a (org-babel-get-src-block-info nil element)))

  (advice-add 'org-babel-exp-src-block :before #'amber-org/-export-lazy-load-library-h)

  (defun amber-org/-src-lazy-load-library-a (lang)
    "Lazy load a babel package to ensure syntax highlighting."
    (or (cdr (assoc lang org-src-lang-modes))
		(amber-org/-babel-lazy-load lang)))

  (advice-add 'org-src--get-lang-mode :before #'amber-org/-src-lazy-load-library-a)

  (defun amber-org/-babel-lazy-load-library-a (info)
    "Load babel libraries lazily when babel blocks are executed."
    (let* ((lang (nth 0 info))
           (lang (cond ((symbolp lang) lang)
                       ((stringp lang) (intern lang))))
		   ;; useful if we want to add aliases
           ;; (lang (or (cdr (assq lang +org-babel-mode-alist))
           ;;           lang))
		   )
      (amber-org/-babel-lazy-load
       lang (and (not (assq :sync (nth 2 info)))
                 (assq :async (nth 2 info))))
      t))

  (advice-add 'org-babel-confirm-evaluate :after-while #'amber-org/-babel-lazy-load-library-a)

  (advice-add #'org-babel-do-load-languages :override #'ignore))

(defun amber-org/yank-id-link ()
  "Copy an ID link to killring.  It will create an ID if there is none."
  (interactive)
  (when (eq major-mode 'org-agenda-mode) ; if we are in agenda, goto target
    (org-agenda-show)
    (org-agenda-goto))
  (when (eq major-mode 'org-mode)
    (let* ((tmp-head (nth 4 (org-heading-components)))
           (tmp-id (org-id-get-create))
           (tmp-link (format "[[id:%s][%s]]" tmp-id tmp-head)))
      (kill-new tmp-link)
      (message "Copied %s to killring" tmp-link))))

;;
;; Packages setup
;;

;; TODO: to handle meeting:
;; when meeting done, move subtree to meeting/%{date}-%{slug}

(use-package org
  :hook ((org-mode . visual-line-mode)
		 (org-mode . amber-org/babel-lazy-load-h))
  :demand t
  :custom
  (org-hide-emphasis-marker t)
  (org-hide-leading-stars t)
  (org-indent-mode-turns-on-hiding-stars t)
  (org-startup-indented t)
  (org-confirm-babel-evaluate nil)
  (org-edit-src-content-indentation 0)
  (org-src-preserve-indentation nil)
  (org-src-tab-acts-natively t)
  (org-capture-bookmark nil)
  (org-enforce-todo-checkbox-dependencies t)
  (org-enforce-todo-dependencies t)
  (org-log-into-drawer "LOGBOOK")
  (org-log-reschedule 'time)
  (org-track-ordered-property-with-tag t)
  (org-use-property-inheritance t)
  (org-directory (expand-file-name "~/src/forge.babariviere.com/babariviere/notes/"))
  (org-global-properties '(("Effort_ALL" . "0:05 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00")))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-attach-dir-relative t)
  (org-link-file-path-type 'relative)
  :config
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("n" . "note"))
  (use-package htmlize)

  ;; Define org todo keywords
  (setq org-todo-keywords
        '((sequence "NEXT(n!)" "TODO(t@/!)" "WAITING(w@/!)"
                    "SOMEDAY(s@/!)" "DELEGATED(D@/!)" "PROJ(p)"
                    "|" "DONE(d)" "CANCELLED(c@)")
          (sequence "REPEAT(r)" "|" "DONE(d!)")))

  (define-key org-mode-map (kbd "C-c y") #'amber-org/yank-id-link)
  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "C-c y") #'amber-org/yank-id-link))

  (add-to-list 'org-modules 'org-habit t)
  :general
  ;; ('normal org-mode-map
  ;; 		   "RET" #'amber-org/dwin-at-point
	;; 	   [ret] #'amber-org/dwin-at-point)
  (amber/leader-keys
    "n" '(:ignore t :wk "notes")
	"nc" '(org-roam-capture :wk "roam capture")
	"no" '(org-clock-out :wk "clock out")
	"nt" '(org-roam-dailies-goto-today :wk "today's note")
	"nT" '(org-roam-dailies-goto-tomorrow :wk "tomorrow's note")
	"ny" '(org-roam-dailies-goto-yesterday :wk "yesterday's note")
    "ng" '(org-roam-dailies-goto-date :wk "goto date's note")))

(use-package org-capture
  :ensure nil
  :after org)

(use-package org-protocol
  :ensure nil
  :after org)

(use-package org-agenda
  :ensure nil
  :after org
  :demand t
  ;; :hook (after-init . amber-org/agenda-view)
  :custom
  ;; (org-agenda-files (mapcar (-partial #'concat org-directory)
	;; 						(list org-private-file org-work-file)))
  (org-agenda-window-setup 'other-window)
  (org-agenda-custom-commands
   '(("A" . "Agendas")
     ("AT" "Daily overview"
      ((tags-todo "URGENT"
		          ((org-agenda-overriding-header "Urgent Tasks")))
       (tags-todo "RADAR"
		          ((org-agenda-overriding-header "On my radar")))
       (tags-todo "PHONE+TODO=\"NEXT\""
		          ((org-agenda-overriding-header "Phone Calls")))
       (tags-todo "Depth=\"Deep\"/NEXT"
                  ((org-agenda-overriding-header "Next Actions requiring deep work")))
       (agenda "" ((org-agenda-overriding-header "Today")
		           (org-agenda-span 1)
		           (org-agenda-sorting-strategy
                    '(time-up priority-down))))
       nil nil))
     ("AW" "Weekly overview" agenda ""
      ((org-agenda-overriding-header "Weekly overview")))
     ("AM" "Monthly overview" agenda ""
      ((org-agenda-overriding-header "Monthly overview"))
      (org-agenda-span 'month)
      (org-deadline-warning-days 0)
      (org-agenda-sorting-strategy '(time-up priority-down tag-up)))
     ("W" . "Weekly Review Helper")
     ("Wn" "New tasks" tags "NEW"
      ((org-agenda-overriding-header "NEW Tasks")))
     ("Wd" "Check DELEGATED tasks" todo "DELEGATED"
      ((org-agenda-overriding-header "DELEGATED tasks")))
     ("Ww" "Check WAITING tasks" todo "WAITING"
      ((org-agenda-overriding-header "WAITING tasks")))
     ("Ws" "Check SOMEDAY tasks" todo "SOMEDAY"
      ((org-agenda-overriding-header "SOMEDAY tasks")))
     ("Wf" "Check finished tasks" todo "DONE|CANCELLED|FORWARDED"
      ((org-agenda-overriding-header "Finished tasks")))
     ("WP" "Planing Todos (unscheduled) only" todo "TODO|NEXT"
      ((org-agenda-overriding-header "Today planning")
       (org-agenda-skip-function
	    '(org-agenda-skip-entry-if 'scheduled 'deadline)))))))

;; TODO: use hydra for refiling as in http://www.howardism.org/Technical/Emacs/getting-more-boxes-done.html
(use-package org-refile
  :ensure nil
  :after org
  :custom
  (org-refile-targets '((nil :maxlevel . 3)
						(org-agenda-files :maxlevel . 3)))
  (org-refile-use-outline-path 'file)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-reverse-note-order nil)
  (org-outline-path-complete-in-steps nil)
  ;; :general
  ;; (amber/leader-keys org-mode-map
  ;;   "r" '(org-refile :wk "refile"))
  )

(use-package org-clock
  :ensure nil
  :after org
  :custom
  (org-clock-out-remove-zero-time-clocks t))

;; Allows for trigger and blocker
;; See: https://www.nongnu.org/org-edna-el/
(use-package org-edna
  :ensure nil
  :after org
  :hook (org-mode . org-edna-mode))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks nil)
  (org-appear-autosubmarkers t)
  (org-appear-autoemphasis t))

(use-package verb
  :after org
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(setq org-plantuml-exec-mode 'plantuml)

(setq org-export-with-broken-links t
      org-export-with-sub-superscripts '{}
      org-use-sub-superscripts '{})

(provide 'amber-org)

;;; amber-org.el ends here
