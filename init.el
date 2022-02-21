(add-to-list 'default-frame-alist '(font . "Iosevka 11"))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(load-theme 'modus-vivendi t)

(setq make-backup-files nil)
(setq auto-save-default nil)

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
