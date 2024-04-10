;;; init.el --- Main Emacs configuration file
;; Copyright © 2019-2024 abe <aabm@disroot.org>

;; Author: abe <aabm@disroot.org>
;; Keywords: literate programming, Emacs configuration
;; Homepage: https://github.com/neueleninlekture/emacs

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
;; abe's init settings for Emacs.

;;; Startup
;; Check version
(when (version< emacs-version "29.3")
  (error "This configuration requires Emacs 29.3 and above!"))

;; Prefer compiled files
(setq load-prefer-newer t)

;;; Quality of life changes
;; Disable backup files that litter your directories
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Automatically reload when a file changes on disk
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)

;; Enable all disabled commands (stuff emacs hides from you)
(setq disabled-command-function nil)

;; Disable default startup screen
;; (setq inhibit-startup-screen t)

;; Disable some UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(setq use-dialog-box nil)
(setq ring-bell-function 'ignore)

;; Better 'yes or no' prompts
;; (defalias 'yes-or-no-p 'y-or-n-p)

;;; User files
;; Do not produce a custom.el file
(setq custom-file "/dev/null")

;; Load main config file
(load-file (expand-file-name "config.el" user-emacs-directory))
