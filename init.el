;;; init.el --- Main Emacs configuration file
;; Copyright © 2019-2022 Aabm <aabm@disroot.org>

;; Author: Aabm <aabm@disroot.org>
;; Keywords: literate programming, Emacs configuration
;; Homepage: https://gitlab.com/aabm/emacs

;;; License:
;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file LICENSE.  If not, you can visit
;; https://www.gnu.org/licenses/gpl-3.0.html or write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:
;; Aabm's init settings for Emacs.
;; This file has been automatically generated by `org-babel'. Do not
;; modify it, as your changes might be overwritten. Instead, modify
;; the `emacs.org' file.

(when (version< emacs-version "27.1")
  (error "This requires Emacs 27.1 and above! Preferably 29 (master), but 27 should be fine..."))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq use-package-always-ensure nil)
(setq use-package-always-defer t)
(setq use-package-hook-name-suffix nil)

(straight-use-package 'diminish)

(setq load-prefer-newer t)

(load-file (expand-file-name "personal/auth.el" user-emacs-directory))

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq disabled-command-function nil)

(setq inhibit-startup-screen t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(tooltip-mode -1)
(setq use-dialog-box nil)

(setq ring-bell-function 'ignore)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package which-key
  :straight t
  :diminish which-key-mode
  :init
  (which-key-mode)
  :custom
  (which-key-idle-delay 0.4))

(straight-use-package 'orderless)

(use-package vertico
  :straight t
  :init
  (vertico-mode)
  :custom
  (completion-styles '(substring orderless))
  (completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (enable-recursive-minibuffers t))

(use-package consult
  :straight t
  :custom
  (consult-narrow-key "<")
  :bind
  (("M-y" . consult-yank-replace)
   ("C-x b" . consult-buffer)
   ("M-g g" . consult-ripgrep)
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g M-g" . consult-goto-line)
   ("M-g s" . consult-line)
   ("M-g f" . consult-file-externally)
   ("C-x r b" . consult-bookmark)))

(use-package embark
  :straight t
  :bind
  (("C-," . embark-act))
  :custom
  (embark-action-indicator
   (lambda (map &optional _target)
     (which-key--show-keymap "Embark" map nil nil 'no-paging)
     #'which-key--hide-popup-ignore-command)
   embark-become-indicator embark-action-indicator))

(use-package marginalia
  :straight t
  :init
  (marginalia-mode)
  :bind
  ((:map minibuffer-local-map
	 ("M-a" . marginalia-cycle))))

(add-hook 'text-mode-hook #'auto-fill-mode)
(diminish 'auto-fill-function)

(setq electric-pair-pairs '((?\{ . ?\})
                            (?\( . ?\))
                            (?\[ . ?\])
                            (?\" . ?\")))
(electric-pair-mode t)

(show-paren-mode t)

(global-set-key (kbd "M-c") 'capitalize-dwim)
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)

(setq sentence-end-double-space nil)

(global-set-key (kbd "M-SPC") 'cycle-spacing) 

(global-set-key (kbd "C-\\") 'indent-region)

(line-number-mode t)
(column-number-mode t)

(use-package expand-region
  :straight t
  :bind
  (("C-=" . er/expand-region)))

(use-package multiple-cursors
  :straight t
  :bind
  ("C-x /" . mc/edit-lines)
  ("C-x ." . mc/mark-next-like-this))

(setq org-src-window-setup 'current-window)

(use-package org
  :custom
  (org-cycle-global-at-bob t)
  (org-startup-folded t)
  :bind
  (:map org-mode-map
	(("M-n" . org-forward-element)
	 ("M-p" . org-backward-element)
	 ("C-M-n" . org-metadown)
	 ("C-M-p" . org-metaup)
	 ("C-M-f" . org-metaright)
	 ("C-M-b" . org-metaleft)
	 ("C-c C-x l" . org-cycle-list-bullet))))

(use-package org
  :custom
  (org-capture-templates
   '(("j" "Journal entry"
      entry
      (file+datetree "agenda/journal.org")
      "* %?"
      :empty-lines 1)))
  :bind
  (("C-c w" . org-capture)))

(use-package org-superstar
  :straight t
  :hook
  (org-mode-hook . org-superstar-mode))

(use-package org-roam
  :straight t
  :init
  (setq org-roam-directory "~/org/roam/")
  (setq org-roam-v2-ack t)
  :config
  (org-roam-setup)
  :custom
  (org-roam-db-location
   (expand-file-name "roam.db" org-roam-directory))
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+title: ${title}\n#+date: %t\n#+filetags: \n")
      :unnarrowed t)))
  (org-capture-bookmark nil)
  (org-roam-db-update-on-save t)
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n l" . org-roam-buffer-toggle)
   ("C-c n w" . org-roam-capture)
   ("C-c n o" . org-roam-buffer-display-dedicated)
   ("C-c n c" . org-id-get-create)
   ("C-c n a" . org-roam-alias-add)
   ("C-c n u" . org-roam-db-sync)))

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui"
	 :branch "main" :files ("*.el" "out"))
    :after org-roam
    :custom
    (org-roam-ui-sync-theme t)
    (org-roam-ui-follow t)
    (org-roam-ui-update-on-save t)
    (org-roam-ui-open-on-start t)
    :bind
    (("C-c n g" . org-roam-ui-mode)))

(use-package olivetti
  :straight t
  :custom
  (olivetti-body-width 0.7)
  :bind
  (("C-c o" . olivetti-mode)))

(use-package pdf-tools
  :straight t
  :init
  (pdf-loader-install)
  :custom
  (pdf-view-resize-factor 1.1)
  (pdf-view-continuous nil)
  (pdf-view-display-size 'fit-page)
  :bind
  (:map pdf-view-mode-map
	(("M-g g" . pdf-view-goto-page))))

(use-package elfeed
  :straight t
  :config
  (load-file (expand-file-name "personal/feeds.el" user-emacs-directory))
  :hook
  ((elfeed-show-mode-hook . olivetti-mode))
  :bind
  (("C-c e" . elfeed)))

(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/bin/msmtp")
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq message-sendmail-f-is-evil 't)

(use-package mu4e
  :straight t
  :commands mu4e mu4e-compose-new
  :custom
  (mail-user-agent 'mu4e-user-agent)
  (mu4e-maildir "~/.mail/disroot/")
  (mu4e-get-mail-command "/usr/bin/mbsync -a")
  (mu4e-update-mail-and-index t)
  (mu4e-update-interval 300)
  (mu4e-view-show-images t)
  (mu4e-view-show-addresses t)
  (mu4e-use-fancy-chars nil)
  (mu4e-drafts-folder "/drafts")
  (mu4e-sent-folder "/sent")
  (mu4e-trash-folder "/trash")
  (message-send-mail-function 'message-send-mail-with-sendmail)
  (sendmail-program "/usr/bin/msmtp")
  (message-sendmail-extra-arguments '("--read-envelope-from"))
  (message-sendmail-f-is-evil t)
  (mu4e-completing-read-function 'completing-read)
  (mu4e-confirm-quit nil)
  (message-kill-buffer-on-exit t)
  (mu4e-attachment-dir "~/")
  (mu4e-compose-signature
   '(user-full-name))
  :hook
  (message-send-hook .
		     (lambda ()
		       (unless (yes-or-no-p "Sure you want to send this?")
			 (signal 'quit nil))))
  :bind
  ((("C-c m" . mu4e)
    ("C-x m" . mu4e-compose-new))))

(use-package vc
  :config
  (defvar vc-shell-output "*vc-output*")
  (defun vc-git-log-grep (pattern &optional diff)
    "Run ’git log --grep’ for PATTERN.
    With optional DIFF as a prefix (\\[universal-argument])
    argument, also show the corresponding diffs. 

  This function was taken from prot."
    (interactive
     (list (read-regexp "Search git log for PATTERN: ")
	   current-prefix-arg))
    (let* ((buf-name vc-shell-output)
	   (buf (get-buffer-create buf-name))
	   (diffs (if diff "-p" ""))
	   (type (if diff 'with-diff 'log-search))
	   (resize-mini-windows nil))
      (shell-command (format "git log %s --grep=%s -i -E --" diffs pattern) buf)
      (with-current-buffer buf
	(setq-local vc-log-view-type type)
	(setq-local revert-buffer-function nil)
	(vc-git-region-history-mode))))
  :bind
  (:map vc-prefix-map
	(("S" . vc-git-log-grep))))

(use-package magit
  :straight t
  :commands
  (magit-status magit)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (defun magit-commit-all ()
    (interactive)
    (start-process-shell-command
     "" nil "git add .")
    (magit-commit-create))
  :bind
  (("C-x g" . magit-status)
   ("C-x v c" . magit-commit-all)
   ("C-x v P" . magit-push-current-to-pushremote)))

(use-package dired
  :custom
  (dired-listing-switches "-alh --group-directories-first")
  (dired-dwim-target t)
  (wdired-allow-to-change-permissions t)
  :config
  (defun dired-xdg-open ()
    "Open the marked files using xdg-open."
    (interactive)
    (let ((file-list (dired-get-marked-files)))
      (mapc
       (lambda (file-path)
	 (let ((process-connection-type nil))
	   (start-process "" nil "xdg-open" file-path)))
       file-list)))
  :bind
  (:map dired-mode-map
	(("v" . dired-xdg-open)
	 ("l" . dired-up-directory))))

(use-package dired-hide-dotfiles
  :straight t
  :diminish dired-hide-dotfiles-mode
  :hook
  ((dired-mode-hook . dired-hide-dotfiles-mode))
  :bind
  (:map dired-mode-map
	(("h" . dired-hide-dotfiles-mode))))

(use-package time
  :init
  (display-time-mode)
  :custom
  (display-time-format "%a, %b %d %H:%M")
  (display-time-default-load-average nil))

(use-package time
  :custom
  (world-clock-list
   '(("America/Los_Angeles" "Seattle")
     ("America/New_York" "New York")
     ("America/Sao_Paulo" "São Paulo")
     ("Europe/London" "London")
     ("Europe/Paris" "Paris")
     ("Africa/Cairo" "Cairo")
     ("Asia/Baghdad" "Baghdad")
     ("Asia/Dushanbe" "Malé")
     ("Asia/Beijing" "Beijing"))))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package ibuffer-project
  :straight t
  :hook
  (ibuffer-mode-hook . (lambda ()
			 (setq ibuffer-filter-groups
			       (ibuffer-project-generate-filter-groups)))))

(global-set-key (kbd "M-o") 'other-window)

(defun split-window-below-and-switch ()
  "A simple replacement for `split-window-below', which automatically focuses the new window."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun split-window-right-and-switch ()
  "A simple replacement for `split-window-right', which automatically focuses the new window."
  (interactive)
  (split-window-right)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'split-window-right-and-switch)

(defun kill-this-buffer+ ()
  "Kill the current buffer. More reliable alternative to `kill-this-buffer'"
  (interactive)
  (kill-buffer))

(global-set-key (kbd "C-x k") 'kill-this-buffer+)

(add-to-list 'default-frame-alist '(font . "Iosevka 11"))
(set-frame-font "Iosevka 11" nil t)

(use-package doom-themes
  :straight t)

(load-theme 'doom-Iosvkem t)

(add-to-list 'default-frame-alist '(alpha 100 100))
(set-frame-parameter (selected-frame) 'alpha '(100 100))
