(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq electric-pair-pairs '((?\{ . ?\}) (?\( . ?\))
			    (?\[ . ?\]) (?\" . ?\")))
(electric-pair-mode t)

(load-theme 'wheatgrass t)

(add-to-list 'default-frame-alist '(font . "Iosevka medium extended 10"))
(set-frame-font "Iosevka medium extended 10" nil t)
