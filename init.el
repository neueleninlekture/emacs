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
;; modify it, as your changes might be overwritter. Instead, modify
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

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq disabled-command-function nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(tooltip-mode -1)
(setq use-dialog-box nil)

(setq ring-bell-function 'ignore)

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
   ("M-g g" . consult-grep)
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g M-g" . consult-goto-line)
   ("M-g f" . consult-file-externally)))

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

(setq org-src-window-setup 'current-window)

(setq org-cycle-global-at-bob t)
(setq org-startup-folded t)

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
  (dired-listing-switches "-alhNF --group-directories-first")
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

(add-to-list 'default-frame-alist '(font . "Iosevka 11"))

(load-theme 'modus-vivendi t)
