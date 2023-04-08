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
(add-hook 'html-mode-hook #'turn-off-auto-fill)

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

(setq org-directory "~/Documents/Org/")

(setq org-src-window-setup 'current-window)

(use-package org
  :custom
  (org-cycle-global-at-bob t)
  (org-startup-folded t))

(use-package org
  :custom
  (org-bookmark-names-plist nil)
  (org-todo-keywords '((sequence "TODO(t)" "MOVE(m)" "WAIT(w)" "|" "DONE(d)" "DROP(c)")))
  (org-refile-targets nil)
  (org-agenda-files '("journal.org"))
  (org-archive-location (format "archive.org::* %s" (format-time-string "%Y")))
  (org-capture-templates
   '(("t" "Daily TODO"
      entry
      (file+datetree "journal.org")
      "* %?\n%t")
     ("f" "Future TODO"
      entry
      (file+datetree+prompt "journal.org")
      "* %?\n%t"
      :time-prompt t))))

(use-package org
  :custom
  (org-pretty-entities t))

(defun org-find-notes-index-file ()
  (interactive)
  (find-file (expand-file-name "index.org" org-directory)))

(defun org-find-notes-journal-file ()
  (interactive)
  (find-file (expand-file-name "journal.org" org-directory)))

(use-package org-roam
  :straight t
  :init
  (setq org-roam-directory org-directory)
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
  (org-cite-global-bibliography "~/Documents/tex/bib/main.bib"))

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

(use-package rainbow-delimiters
  :straight t
  :hook
  ((prog-mode-hook . rainbow-delimiters-mode)))

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

(use-package ibuffer-project
  :straight t
  :hook
  (ibuffer-mode-hook . (lambda ()
			 (setq ibuffer-filter-groups
			       (ibuffer-project-generate-filter-groups)))))

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
  "ow" 'org-capture
  "oa" 'org-agenda
  "ob" 'org-switchb
  "oj" 'org-find-notes-journal-file
  "oi" 'org-find-notes-index-file
  "of" 'org-roam-node-find
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
  "kK" 'kill-buffer-and-window
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
