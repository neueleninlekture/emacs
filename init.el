;;; init.el --- Main Emacs configuration file
;; Copyright © 2019-2021 Aabm <aabm@disroot.org>

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
;; This file was automatically generated by `org-babel-tangle'. Do not
;; change this file. The real configuration is found in the `emacs.org'
;; file.

(when (version< emacs-version "27.1")
  (error "This requires Emacs 27.1 and above! Preferably 28 (master), but 27 should be fine..."))

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

(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("elisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "elisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(add-hook 'emacs-startup-hook #'update-load-path)
(add-hook 'emacs-startup-hook #'add-subdirs-to-load-path)

(setq load-prefer-newer t)

(defvar machine-desktop-p (equal (system-name) "station"))
(defvar machine-laptop-p (equal (system-name) "deck"))
(defvar machine-phone-p (equal (system-name) "runner"))

(load-file (expand-file-name "personal/creds.el" user-emacs-directory))

(setq disabled-command-function nil)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(setq use-dialog-box nil)
(setq ring-bell-function 'ignore)

(setq inhibit-startup-screen t)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq large-file-warning-threshold nil)

(auto-revert-mode t)
(diminish 'auto-revert-mode)

(defalias 'yes-or-no-p 'y-or-n-p)

(setf mouse-wheel-scroll-amount '(3 ((shift) . 3))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      scroll-step 1
      disabled-command-function nil)

(setq focus-follows-mouse t
      mouse-autoselect-window t)

(blink-cursor-mode -1)

(setq electric-pair-pairs '((?\{ . ?\})
                            (?\( . ?\))
                            (?\[ . ?\])
                            (?\" . ?\")))
(electric-pair-mode t)

(show-paren-mode t)

(global-visual-line-mode t)
(diminish 'visual-line-mode)

(add-hook 'text-mode-hook #'auto-fill-mode)
(diminish 'auto-fill-function)

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

(defun fill-or-unfill-paragraph (&optional unfill region)
  "Fill paragraph (or REGION).
With the prefix argument UNFILL, unfill it instead."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list (if current-prefix-arg 'unfill) t)))
  (let ((fill-column (if unfill (point-max) fill-column)))
    (fill-paragraph nil region)))

(global-set-key (kbd "M-q") 'fill-or-unfill-paragraph)

(use-package avy
  :straight t
  :bind
  (("M-s" . avy-goto-char-2)
   ("C-." . avy-goto-char-timer)))

(setq sentence-end-double-space nil)

(global-set-key (kbd "M-c") 'capitalize-dwim)
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)

(use-package expand-region
  :straight t
  :bind
  (("C-=" . er/expand-region)))

(use-package move-text
  :straight t
  :bind
  (("C-S-p" . move-text-up)
   ("C-S-n" . move-text-down)))

(use-package isearch-mb
  :straight t
  :diminish isearch-mb-mode
  :init
  (isearch-mb-mode))

(global-set-key (kbd "M-SPC") 'cycle-spacing) 

(global-set-key (kbd "C-\\") 'indent-region)

(setq select-enable-clipboard t)
(setq save-interprogram-paste-before-kill t)

(use-package olivetti
  :straight t
  :custom
  (olivetti-body-width 102))

(defvar better-reading-mode-map (make-sparse-keymap))

(define-minor-mode better-reading-mode
  "Minor Mode for better reading experience."
  :init-value nil
  :group aabm
  :keymap better-reading-mode-map
  (if better-reading-mode
      (progn
        (and (fboundp 'olivetti-mode) (olivetti-mode 1))
        (and (fboundp 'variable-pitch-mode) (variable-pitch-mode 1))
        (text-scale-set +1))
    (progn
      (and (fboundp 'olivetti-mode) (olivetti-mode -1))
      (and (fboundp 'variable-pitch-mode) (variable-pitch-mode -1))
      (text-scale-set 0))))

(global-set-key (kbd "C-c o") 'better-reading-mode)
(define-key better-reading-mode-map (kbd "M-n") 'scroll-up-line)
(define-key better-reading-mode-map (kbd "M-p") 'scroll-down-line)

(use-package pdf-tools
  :straight t
  :init
  (pdf-loader-install)
  :custom
  (pdf-view-resize-factor 1.1)
  (pdf-view-continuous nil)
  (pdf-view-display-size 'fit-page)
  :config
  (defun pdf-view-open-in-zathura ()
    "Open the current PDF with ‘zathura’."
    (interactive)
    (save-window-excursion
      (let ((current-file (buffer-file-name))
	    (current-page (number-to-string (pdf-view-current-page))))
	(async-shell-command
	 (format "zathura -P %s \"%s\"" current-page current-file))))
    (message "Sent to Zathura"))
  :bind
  (:map pdf-view-mode-map
	(("M-g g" . pdf-view-goto-page)
	 ("C-c C-z" . pdf-view-open-in-zathura))))

(use-package nov
  :straight t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :custom
  (nov-text-width 80)
  (nov-text-width t)
  (visual-fill-column-center-text t)  
  :hook
  ((nov-mode-hook . better-reading-mode)))

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
  (completion-styles '(orderless))
  (completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (enable-recursive-minibuffers t))

(use-package consult
  :straight t
  :custom
  (consult-narrow-key "<")
  :bind
  (("M-y" . consult-yank)
   ("C-x b" . consult-buffer)
   ("M-g g" . consult-grep)
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g M-g" . consult-goto-line)))

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
  :bind
  ((:map minibuffer-local-map
	 ("M-a" . marginalia-cycle))))

(use-package miniedit
  :straight t
  :init
  (miniedit-install))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(diminish 'eldoc-mode)

(use-package corfu
  :straight t
  :diminish corfu-mode
  :hook
  ((prog-mode-hook . corfu-mode)
   (eshell-mode-hook . corfu-mode))
  :bind
  (:map corfu-map
	(("TAB" . corfu-next)
	 ("S-TAB" . corfu-previous)))
  :custom
  (corfu-cycle t)
  (corfu-auto t))

(use-package eglot
  :straight t
  :bind
  (:map eglot-mode-map
	(("C-c r" . eglot-rename)
	 ("C-c o" . eglot-code-actions)
	 ("C-c f" . eglot-format)
	 ("C-c h" . eldoc))))

(use-package markdown-mode
  :straight t)

(use-package muse
  :straight t)

(use-package geiser
  :straight geiser-guile
  :init
  (setq geiser-active-implementations '(guile)))

(use-package sicp
  :straight t)

(add-hook 'mhtml-mode-hook
	  (lambda ()
	    (interactive)
	    (auto-fill-mode -1)))

(use-package ess
  :straight t
  :hook
  ((ess-r-mode-hook . eglot-ensure)))

(use-package eshell
  :init
  (defvar eshell-minor-mode-map (make-sparse-keymap))

  (define-minor-mode eshell-minor-mode
    "Minor mode that enables various custom keybindings for `eshell'."
    nil " esh" eshell-minor-mode-map)
  :hook
  ((eshell-mode-hook . eshell-minor-mode))
  :custom
  (eshell-cd-on-directory t)
  (eshell-banner-message "In the beginning was the command line.\n")
  :config
  (defun eshell-find-file-at-point ()
    "Finds file under point. Will open a dired buffer if file is a directory."
    (interactive)
    (let ((file (ffap-guess-file-name-at-point)))
      (if file
          (find-file file)
        (user-error "No file at point"))))

  (defun eshell-copy-file-path-at-point ()
    "Copies path to file at point to the kill ring"
    (interactive)
    (let ((file (ffap-guess-file-name-at-point)))
      (if file
          (kill-new (concat (eshell/pwd) "/" file))
        (user-error "No file at point"))))

  (defun eshell-cat-file-at-point ()
    "Outputs contents of file at point"
    (interactive)
    (let ((file (ffap-guess-file-name-at-point)))
      (if file
          (progn
            (goto-char (point-max))
            (insert (concat "cat " file))
            (eshell-send-input)))))

  (defun eshell-put-last-output-to-buffer ()
    "Produces a buffer with output of last `eshell' command."
    (interactive)
    (let ((eshell-output (kill-ring-save (eshell-beginning-of-output)
                                         (eshell-end-of-output))))
      (with-current-buffer (get-buffer-create  "*last-eshell-output*")
        (erase-buffer)
        (yank)
        (switch-to-buffer-other-window (current-buffer)))))

  (defun eshell-mkcd (dir)
    "Make a directory, or path, and switch to it."
    (interactive)
    (eshell/mkdir "-p" dir)
    (eshell/cd dir))

  (defun eshell-sudo-open (filename)
    "Open a file as root in Eshell, using TRAMP."
    (let ((qual-filename (if (string-match "^/" filename)
                             filename
                           (concat (expand-file-name (eshell/pwd)) "/" filename))))
      (switch-to-buffer
       (find-file-noselect
        (concat "/sudo::" qual-filename)))))

  (defalias 'mkcd 'eshell-mkcd)
  (defalias 'open 'find-file-other-window)
  (defalias 'sopen 'eshell-sudo-open)
  (defalias 'clean 'eshell/clear-scrollback)
  (defalias 'mkcd 'eshell-mkcd)
  :bind
  (("C-x s" . eshell)
   (:map eshell-minor-mode-map
         (("C-c C-f" . eshell-find-file-at-point)
          ("C-c C-w" . eshell-copy-file-path-at-point)
          ("C-c C-o" . eshell-cat-file-at-point)
          ("C-c C-b" . eshell-put-last-output-to-buffer)
          ("C-c C-m" . mkdir)
          ("C-c C-t" . chmod)))))

(defun split-window-below-and-follow ()
  "A simple replacement for `split-window-below', which automatically focuses the new window."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun split-window-right-and-follow ()
  "A simple replacement for `split-window-right', which automatically focuses the new window."
  (interactive)
  (split-window-right)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'split-window-below-and-follow)
(global-set-key (kbd "C-x 3") 'split-window-right-and-follow)

(global-set-key (kbd "M-o") 'other-window)

(defun kill-this-buffer+ ()
  "Kill the current buffer. More reliable alternative to `kill-this-buffer'"
  (interactive)
  (kill-buffer))

(global-set-key (kbd "C-x k") 'kill-this-buffer+)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package ibuffer-project
  :straight t
  :hook
  (ibuffer-mode-hook . (lambda ()
			 (setq ibuffer-filter-groups
			       (ibuffer-project-generate-filter-groups)))))

(defun restore-scratch-buffer ()
  "Restores the scratch buffer, in case it has been killed."
  (interactive)
  (switch-to-buffer "*scratch*")
  (get-buffer "*scratch*")
  (with-current-buffer "*scratch*"
    (when (zerop (buffer-size))
      (insert (substitute-command-keys initial-scratch-message))
      (set-buffer-modified-p nil))))

(use-package dired
  :custom
  (dired-listing-switches "-alNF --group-directories-first")
  (dired-dwim-target t)
  (wdired-allow-to-change-permissions t)
  :config
  (defun dired-up-alternate-directory ()
    (interactive)
    (find-alternate-file ".."))

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
	(("l" . dired-up-alternate-directory)
	 ("RET" . dired-find-alternate-file)
	 ("M-RET" . dired-find-file)
	 ("v" . dired-xdg-open)
	 ("<mouse-2>" . dired-mouse-find-file))))

(use-package dired-hide-dotfiles
  :straight t
  :diminish dired-hide-dotfiles-mode
  :hook
  ((dired-mode-hook . dired-hide-dotfiles-mode))
  :bind
  (:map dired-mode-map
	(("h" . dired-hide-dotfiles-mode))))

(use-package dired-subtree
  :straight t
  :bind
  (:map dired-mode-map
	(("TAB" . dired-subtree-toggle)
	 ("M-n" . dired-subtree-down)
	 ("M-p" . dired-subtree-up))))

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
      (shell-command (format "git log %s --grep=%s -E --" diffs pattern) buf)
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

(defun delete-this-file-and-buffer ()
  "Deletes the file visited by the current buffer, then kills the buffer."
  (interactive)
  (delete-file (buffer-file-name))
  (kill-buffer))

(use-package org
  :custom
  (org-cycle-global-at-bob t)
  (org-hide-leading-stars t)
  (org-startup-folded t)
  :bind
  (:map org-mode-map
	(("M-n" . org-forward-element)
	 ("M-p" . org-backward-element)
	 ("C-M-n" . org-metadown)
	 ("C-M-p" . org-metaup)
	 ("C-M-f" . org-metaright)
	 ("C-M-b" . org-metaleft)
	 ("<mouse-3>" . org-cycle)
	 ("C-c C-x l" . org-cycle-list-bullet))))

(setq org-adapt-indentation nil)

(use-package org
  :custom
  (org-src-tab-acts-natively t)
  (org-src-fontify-natively t)
  (org-src-window-setup 'current-window)
  (org-structure-template-alist
   '(("a" . "export ascii")
     ("c" . "center")
     ("C" . "comment")
     ("ee" . "export")
     ("eh" . "export html")
     ("el" . "export latex")
     ("E" . "example")
     ("q" . "quote")
     ("ss" . "src")
     ("sS" . "src scheme\n")
     ("se" . "src emacs-lisp\n")
     ("v" . "verse"))))

(setq org-babel-confirm-babel-evaluate nil)
(setq org-babel-load-languages
      '((R . t)
	(python . t)
	(emacs-lisp . t)
	(scheme . t)
	(shell . t)
	(org . t)
	(latex . t)))

(use-package org
  :custom
  (org-agenda-files '("~/org/agenda/inbox.org"
		      "~/org/agenda/projects.org"
		      "~/org/agenda/time.org"))
  (org-archive-location "~/org/agenda/archive.org::")
  (org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "DROP(c)")))
  (org-capture-bookmark nil)
  (org-capture-templates
   '(("t" "GTD Inbox"
      entry
      (file+headline "agenda/inbox.org" "Tasks")
      "** TODO %?\n%i\n%a")
     ("T" "GTD Time-sensitive"
      entry
      (file+headline "agenda/time.org" "Time-sensitive Tasks")
      "** TODO %?\n%i\n%a")))
  (org-refile-targets '(("~/org/agenda/projects.org" :maxlevel . 1)
			("~/org/agenda/someday.org" :level . 1)
			("~/org/agenda/time.org" :maxlevel . 2)))
  :bind
  (("C-c w" . org-capture)
   ("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   (:map org-mode-map
	 ("C-," . nil))))

(defun tomato ()
  (interactive)
  (let ((org-timer-countdown-timer-title "Tomato"))
    (org-timer-set-timer tomato-next-time))
  (if (= tomato-next-time 5)
      (setq tomato-next-time 25)
    (setq tomato-next-time 5)))

(defvar tomato-next-time 5)
(add-hook 'org-timer-done-hook 'tomato)

(use-package org
  :config
  (defun org-insert-link-dwim ()
    "Like `org-insert-link' but with personal dwim preferences."
    (interactive)
    (let* ((point-in-link (org-in-regexp org-link-any-re 1))
	   (clipboard-url (when (string-match-p "^http" (current-kill 0))
			    (current-kill 0)))
	   (region-content (when (region-active-p)
			     (buffer-substring-no-properties (region-beginning)
							     (region-end)))))
      (cond ((and region-content clipboard-url (not point-in-link))
	     (delete-region (region-beginning) (region-end))
	     (insert (org-make-link-string clipboard-url region-content)))
	    ((and clipboard-url (not point-in-link))
	     (insert (org-make-link-string
		      clipboard-url
		      (read-string "title: "
				   (with-current-buffer (url-retrieve-synchronously clipboard-url)
				     (dom-text (car
						(dom-by-tag (libxml-parse-html-region
							     (point-min)
							     (point-max))
							    'title))))))))
	    (t
	     (call-interactively 'org-insert-link)))))
  :bind
  (:map org-mode-map
	(("C-c C-l" . org-insert-link-dwim))))

(use-package ox-epub
  :straight t)

(use-package org
  :init
  (setq org-export-backends '(ascii beamer epub html latex md))
  :custom
  (org-export-html-postamble nil))

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
  (org-roam-db-update-on-save nil)
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

(use-package elfeed
  :straight t
  :config
  (load-file (expand-file-name "personal/feeds.el" user-emacs-directory))
  :hook
  ((elfeed-show-mode-hook . better-reading-mode))
  :bind
  (("C-c e" . elfeed)))

(defun ytdl ()
  "Download videos from the web using the `youtube-dl' command line tool.

Prompts you for a target directory and a url, downloading the url to the path."
  (interactive)
  (let ((default-directory (read-file-name "Download to: "))
	(link (read-string "URL: " nil nil "https://youtu.be/dQw4w9WgXcQ")))
    (start-process "ytdl" "*ytdl*" "youtube-dl" link)))

(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/bin/msmtp")
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq message-sendmail-f-is-evil 't)

(use-package mu4e
  :straight t
  :commands mu4e mu4e-compose-new
  :custom
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
  (message-sendmail-f-is-evil 't)
  (mu4e-completing-read-function 'completing-read)
  (mu4e-confirm-quit nil)
  (message-kill-buffer-on-exit t)
  (mu4e-html2text-command "/usr/bin/w3m -T text/html")
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

(use-package eww
  :hook
  ((eww-mode-hook . olivetti-mode)))

(use-package erc
  :custom
  (erc-nick '("aabm" "aaabm"))
  (erc-default-server "irc.rizon.net")
  :bind
  (("C-c i" . erc)))

(use-package doom-themes
  :straight t)

(load-theme 'doom-Iosvkem t)

(define-minor-mode serif-font-mode
  "Minor mode which sets the default buffer face to the serif font, using `buffer-face-mode'."
  :init-value nil
  :group aabm
  (if serif-font-mode
      (progn
	(setq buffer-face-mode-face '(:family "IBM Plex Serif" :height 110))
	(and (fboundp 'buffer-face-mode) (buffer-face-mode 1)))
    (and (fboundp 'buffer-face-mode) (buffer-face-mode -1))))

(custom-set-faces
 '(fixed-pitch ((t (:family "Iosevka" :height 110))))
 '(variable-pitch ((t :family "IBM Plex Sans" :height 110)))
 '(bookmark-face ((t :background nil))))

(add-to-list 'default-frame-alist '(font . "Iosevka 11"))
(set-frame-font "Iosevka 11" nil t)

(line-number-mode t)
(column-number-mode t)

(add-to-list 'default-frame-alist '(alpha 95 93))
(set-frame-parameter (selected-frame) 'alpha '(95 93))
