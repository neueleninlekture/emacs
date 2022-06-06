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

;;; Startup
;; Check version
(when (version< emacs-version "27.1")
  (error "This requires Emacs 27.1 and above! Preferably 29 (master), but 27 should be fine..."))

;; Bootstrap straight.el
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

;; Install use-package
(straight-use-package 'use-package)

(setq use-package-always-ensure nil)
(setq use-package-always-defer t)
(setq use-package-hook-name-suffix nil)

(straight-use-package 'diminish)

;; Prefer compiled files
(setq load-prefer-newer t)

;;; Quality of life changes
;; Disable backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Enable all disabled commands
(setq disabled-command-function nil)

;; Disable default startup screen
(setq inhibit-startup-screen t)

;; Disable some UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(tooltip-mode -1)
(setq use-dialog-box nil)

(setq ring-bell-function 'ignore)

;; Automatically reload when a file changes on disk
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)

;; Better 'yes or no' prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;;; User files
;; Do not produce a custom.el file
(setq custom-file "/dev/null")

;; Load user files
(load-file (expand-file-name "auth.el" user-emacs-directory))
(load-file (expand-file-name "config.el" user-emacs-directory))

(defun find-emacs-config ()
  (interactive)
  (find-file "~/.emacs.d/config.org"))
