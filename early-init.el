;;; early-init.el --- Emacs early configuration file
;; Copyright © 2019-2024 abe <aabm@disroot.org>

;; Author: abe <aabm@disroot.org>
;; Keywords: Emacs configuration, literate programming
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
;; abe's early-init settings for Emacs.

(defvar startup-file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

(defun startup-reset-defaults ()
  (setq gc-cons-threshold 16777216
	gc-cons-percentage 0.1
	file-name-handler-alist startup-file-name-handler-alist))

(add-hook 'emacs-startup-hook #'startup-reset-defaults)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs ready in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract
			       after-init-time
			       before-init-time)))
		     gcs-done)))
