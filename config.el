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
  (consult-narrow-key "<"))

(use-package embark
  :straight t
  :defer nil
  :custom
  (embark-indicators
   '(embark-highlight-indicator
     embark-verbose-indicator
     embark-isearch-highlight-indicator))
   :bind
   (:map minibuffer-local-map
	 ("C-." . embark-act)))

(use-package embark-consult
  :straight t)

(use-package marginalia
  :straight t
  :init
  (marginalia-mode)
  :bind
  ((:map minibuffer-local-map
	 ("M-a" . marginalia-cycle))))

(use-package isearch-mb
  :straight t
  :diminish isearch-mb-mode
  :init
  (isearch-mb-mode))

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

(line-number-mode t)
(column-number-mode t)

(setq display-line-numbers-type 'relative)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package expand-region
  :straight t
  :bind
  (("C-=" . er/expand-region)))

(use-package multiple-cursors
  :straight t)

(setq org-src-window-setup 'current-window)

(use-package org
  :custom
  (org-cycle-global-at-bob t)
  (org-startup-folded t))

(use-package org
  :custom
  (org-bookmark-names-plist nil)
  (org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)" "MOVE(m)" "DROP(c)")))
  (org-refile-targets nil)
  (org
   -agenda-files '("agenda.org" "journal.org"))
  (org-archive-location (format "archive.org::* %s" (format-time-string "%Y")))
  (org-capture-templates
   '(("j" "Journal entry"
      entry
      (file+datetree "journal.org")
      "* %?"
      :empty-lines 1)
     ("t" "TODO"
      entry
      (file+headline "agenda.org" "Inbox")
      "* TODO %?"
      :empty-lines 1)
     ("d" "Deadline TODO"
      entry
      (file+headline "agenda.org" "Inbox")
      "* TODO %?\nDEADLINE: %^{Deadline: }T"
      :empty-lines 1)
     ("s" "Scheduled TODO"
      entry
      (file+headline "agenda.org" "Inbox")
      "* TODO %?\nSCHEDULED: %^{Scheduled: }T"
      :empty-lines 1)))
  :config
  (defun aabm/mark-done-and-archive ()
    "Mark the state of an org-mode item as DONE, archive it, and
save the Org buffers."
    (interactive)
    (org-todo 'done)
    (org-archive-subtree)
    (org-save-all-org-buffers)))

(use-package org-superstar
  :straight t
  :hook
  (org-mode-hook . org-superstar-mode))

(use-package org
  :custom
  (org-pretty-entities t))

(defun org-find-notes-index-file ()
  (interactive)
  (find-file (expand-file-name "index.org" org-directory)))

(use-package org-roam
  :straight t
  :init
  (setq org-roam-directory "~/org/")
  (setq org-roam-v2-ack t)
  :config
  (org-roam-setup)
  :custom
  (org-roam-db-location
   (expand-file-name "roam.db" org-roam-directory))
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "${slug}.org"
			 "#+title: ${title}\n#+date: %t\n")
      :unnarrowed t)))
  (org-roam-db-update-on-save t))

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui"
	 :branch "main" :files ("*.el" "out"))
    :after org-roam
    :custom
    (org-roam-ui-sync-theme t)
    (org-roam-ui-follow t)
    (org-roam-ui-update-on-save t)
    (org-roam-ui-open-on-start t))

(use-package markdown-mode
  :straight t)

(use-package org
  :custom
  (org-cite-global-bibliography "~/doc/tex/bib/main.bib"))

(use-package citar
  :straight t
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography))

(use-package olivetti
  :straight t
  :custom
  (olivetti-body-width 0.72))

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

(use-package rainbow-delimiters
  :straight t
  :hook
  ((prog-mode-hook . rainbow-delimiters-mode)))

(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-preselect-first nil)
  :hook
  ((prog-mode-hook . corfu-mode)
   (shell-mode-hook . corfu-mode)
   (eshell-mode-hook . corfu-mode)))

(use-package esh-mode
  :commands eshell eshell-toggle
  :config
  (defun with-face (str &rest face-plist)
    (propertize str 'face face-plist))

  (defun eshell-sudo-open (filename)
    "Open a file as root in Eshell, using TRAMP."
    (let ((qual-filename (if (string-match "^/" filename)
			     filename
			   (concat (expand-file-name (eshell/pwd)) "/" filename))))
      (switch-to-buffer
       (find-file-noselect
	(concat "/sudo::" qual-filename)))))

  (defun eshell-copy-file-path-at-point ()
    "Copies path to file at point to the kill ring"
    (interactive)
    (let ((file (ffap-file-at-point)))
      (if file
	  (kill-new (concat (eshell/pwd) "/" file))
	(user-error "No file at point"))))

  (defun eshell-find-file-at-point ()
    "Finds file under point. Will open a dired buffer if file is a directory."
    (interactive)
    (let ((file (ffap-file-at-point)))
      (if file
	  (find-file file)
	(user-error "No file at point"))))

  (defun eshell-cat-file-at-point ()
    "Outputs contents of file at point"
    (interactive)
    (let ((file (ffap-file-at-point)))
      (if file
	  (progn
	    (goto-char (point-max))
	    (insert (concat "cat " file))
	    (eshell-send-input)))))

  (defun eshell-mkcd (dir)
    "Make a directory, or path, and switch to it."
    (interactive)
    (eshell/mkdir "-p" dir)
    (eshell/cd dir))

  (defun eshell-put-last-output-to-buffer ()
    "Produces a buffer with output of last `eshell' command."
    (interactive)
    (let ((eshell-output (kill-ring-save (eshell-beginning-of-output)
					 (eshell-end-of-output))))
      (with-current-buffer (get-buffer-create  "*last-eshell-output*")
	(erase-buffer)
	(yank)
	(switch-to-buffer-other-window (current-buffer)))))

  (defun dired-here ()
    (interactive)
    (dired "."))

  (defalias 'open 'find-file-other-window)
  (defalias 'clean 'eshell/clear-scrollback)
  (defalias 'mkcd 'eshell-mkcd)
  (defalias 'sopen 'eshell-sudo-open)
  (defalias 'dr 'dired-here)

  :custom
  (eshell-prompt-regexp "^[^αλ\n]*[αλ] ")
  (eshell-prompt-function
   (lambda ()
     (let ((header-bg "#1d1d1e"))
       (concat
	(if (string= (eshell/pwd) (getenv "HOME"))
	    (propertize "~" 'face `(:foreground "#FC20BB"))
	  (replace-regexp-in-string
	   (getenv "HOME")
	   (propertize "~" 'face `(:foreground "#6688AA"))
	   (propertize (eshell/pwd) 'face `(:foreground "#6688AA"))))
	(if (= (user-uid) 0)
	    (with-face " #" :foreground "#FC20BB"))
	(with-face " λ" :foreground "#FC20BB")
	" "))))
  (eshell-highlight-prompt nil)
  (eshell-cd-on-directory t))

(use-package eshell-toggle
  :straight t
  :custom
  (eshell-toggle-run-command nil)
  :bind
  (("C-c e" . eshell-toggle)))

(use-package elfeed
  :straight t
  :config
  (load-file (expand-file-name "personal/feeds.el" user-emacs-directory))
  :hook
  ((elfeed-show-mode-hook . olivetti-mode)))

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
			 (signal 'quit nil)))))

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
	(vc-git-region-history-mode)))))

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
    (magit-commit-create)))

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
	(("v" . dired-xdg-open))))

(use-package dired-hide-dotfiles
  :straight t
  :diminish dired-hide-dotfiles-mode
  :hook
  ((dired-mode-hook . dired-hide-dotfiles-mode))
  :bind
  (:map dired-mode-map
	(("H" . dired-hide-dotfiles-mode))))

(use-package diredfl
  :straight t
  :hook
  ((dired-mode-hook . diredfl-mode)))

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

(defun aabm/other-buffer ()
  (interactive)
  (switch-to-buffer nil))

(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil)
  (evil-mode)
  :custom
  (evil-undo-system 'undo-redo)
  :hook
  (with-editor-mode-hook . evil-insert-state)
  (org-src-mode-hook . evil-insert-state))

(use-package evil-collection
  :straight t
  :init
  (evil-collection-init))

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode t))

(use-package org-evil
  :straight t)

(use-package evil-org
  :straight t
  :hook
  ((org-mode-hook . evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-escape
  :straight t
  :custom
  (evil-escape-key-sequence "jk"))

(use-package general
  :straight t)

(general-evil-setup)

(general-nmap
  :states 'normal
  :keymaps 'override
  :prefix "SPC"
  :prefix-map 'leader-map
  ;; files
  "ff" 'find-file
  "fd" 'dired
  "fj" 'dired-jump
  "fc" 'find-emacs-config
  "fo" 'consult-file-externally
  ;; buffers
  "bb" 'consult-buffer
  "bk" 'kill-this-buffer+
  "bK" 'kill-buffer
  "bm" 'consult-bookmark
  "bi" 'ibuffer
  "bo" 'aabm/other-buffer
  ;; git
  "gg" 'magit-status
  "gc" 'magit-commit-all
  "gp" 'magit-push-current-to-pushremote
  "gs" 'vc-git-log-grep
  ;; search
  "sr" 'consult-ripgrep
  "so" 'consult-outline
  "sm" 'consult-mark
  "ss" 'consult-line
  "sl" 'consult-goto-line
  ;; text editing
  "ti" 'indent-region
  "ty" 'consult-yank-from-kill-ring
  "to" 'olivetti-mode
  ;; windows
  "wo" 'other-window
  "wd" 'delete-other-windows
  "wk" 'delete-window
  "wj" 'split-window-below-and-switch
  "wl" 'split-window-right-and-switch
  ;; major modes
  "xe" 'elfeed
  "xs" 'eshell-toggle
  ;; mail
  "mm" 'mu4e
  "mc" 'mu4e-compose-new
  ;;; notes
  ;; basic org
  "nw" 'org-capture
  "na" 'org-agenda
  "nb" 'org-switchb
  ;; roam
  "nn" 'org-find-notes-index-file
  "nf" 'org-roam-node-find
  "ni" 'org-roam-node-insert
  "nB" 'org-roam-buffer-toggle
  "nc" 'org-roam-capture
  "no" 'org-roam-buffer-display-dedicated
  "nI" 'org-id-get-create
  "nA" 'org-roam-alias-add
  "nu" 'org-roam-db-sync
  "ng" 'org-roam-ui-mode
  ;; kill stuff (yes these keys are redundant)
  "kk" 'kill-this-buffer+
  "kb" 'kill-buffer
  "kw" 'delete-window
  ;; other
  "." 'embark-act)

(general-nmap
  :states 'normal
  :keymaps 'eshell-mode-map
  :prefix ","
  :prefix-map 'local-leader-map
  "y" 'eshell-copy-file-path-at-point
  "f" 'eshell-find-file-at-point
  "o" 'eshell-cat-file-at-point
  "b" 'eshell-put-last-output-to-buffer)

(add-to-list 'default-frame-alist '(font . "Iosevka 11"))
(set-frame-font "Iosevka 11" nil t)

(use-package doom-themes
  :straight t
  :custom
  (doom-gruvbox-dark-variant 'hard))

(load-theme 'doom-Iosvkem t)

(use-package doom-modeline
  :straight t
  :init
  (doom-modeline-mode)
  :custom
  (doom-modeline-enable-word-count t)
  (doom-modeline-minor-modes nil))
