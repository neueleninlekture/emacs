(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq disabled-command-function nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq electric-pair-pairs '((?\{ . ?\}) (?\( . ?\))
			    (?\[ . ?\]) (?\" . ?\")))
(electric-pair-mode t)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

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

(defun kill-this-buffer+ ()
  "Kill the current buffer. More reliable alternative to `kill-this-buffer'"
  (interactive)
  (kill-buffer))

(global-set-key (kbd "C-x k") 'kill-this-buffer+)

(defun dired-up-alternate-directory ()
  (interactive)
  (find-alternate-file ".."))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "l") 'dired-up-alternate-directory)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "M-RET") 'dired-find-file))

(with-eval-after-load 'org
  (setq org-src-tab-acts-natively t)
  (setq org-src-fontify-natively t)
  (setq org-src-window-setup 'current-window))

(with-eval-after-load 'org
  (setq org-cycle-global-at-bob t))

(load-theme 'wheatgrass t)

(add-to-list 'default-frame-alist '(font . "Iosevka medium extended 10"))
(set-frame-font "Iosevka medium extended 10" nil t)

(line-number-mode t)
(column-number-mode t)
