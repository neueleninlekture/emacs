;;; genbuffer.el --- generate new scratch buffers with ease

;; Copyright (C) 2019-2021 aabm

;; Author: aabm <aabm@disroot.org>
;; Created: 2019
;; Version: 0.1
;; Keywords: convenience, usability, buffers

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Source code
;;
;; genbuffer's code can be found here, among other things:
;;   http://gitlab.com/aabm/emacs

(defun genbuffer-org ()
  "Create and switch to a temporary org mode buffer with a random name."
  (interactive)
  (switch-to-buffer (make-temp-name "org-"))
  (org-mode))

(defun genbuffer-scratch ()
  "Create and switch to a temporary scratch buffer with a random name."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-"))
  (lisp-interaction-mode))

(defun genbuffer-text ()
  "Create and switch to a temporary text buffer with a random name."
  (interactive)
  (switch-to-buffer (make-temp-name "text-"))
  (fundamental-mode))

(defun genbuffer-html ()
  "Create and switch to a temporary html buffer with a random name."
  (interactive)
  (switch-to-buffer (make-temp-name "html-"))
  (mhtml-mode))

(provide 'genbuffer)
