(defun aabm/eshell-sudo-open (filename)
  "Open a file as root in Eshell, using TRAMP."
  (let ((qual-filename (if (string-match "^/" filename)
                           filename
                         (concat (expand-file-name (eshell/pwd)) "/" filename))))
    (switch-to-buffer
     (find-file-noselect
      (concat "/sudo::" qual-filename)))))

(defun aabm/eshell-copy-file-path-at-point ()
  "Copies path to file at point to the kill ring"
  (interactive)
  (let ((file (ffap-file-at-point)))
    (if file
        (kill-new (concat (eshell/pwd) "/" file))
      (user-error "No file at point"))))

(defun aabm/eshell-find-file-at-point ()
  "Finds file under point. Will open a dired buffer if file is a directory."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (if file
        (find-file file)
      (user-error "No file at point"))))

(defun aabm/eshell-cat-file-at-point ()
  "Outputs contents of file at point"
  (interactive)
  (let ((file (ffap-file-at-point)))
    (if file
        (progn
          (goto-char (point-max))
          (insert (concat "cat " file))
          (eshell-send-input)))))

(defun aabm/eshell-mkcd (dir)
  "Make a directory, or path, and switch to it."
  (interactive)
  (eshell/mkdir "-p" dir)
  (eshell/cd dir))

(defun aabm/eshell-put-last-output-to-buffer ()
  "Produces a buffer with output of last `eshell' command."
  (interactive)
  (let ((eshell-output (kill-ring-save (eshell-beginning-of-output)
                                       (eshell-end-of-output))))
    (with-current-buffer (get-buffer-create  "*last-eshell-output*")
      (erase-buffer)
      (yank)
      (switch-to-buffer-other-window (current-buffer)))))

(defalias 'open 'find-file-other-window)
(defalias 'clean 'eshell/clear-scrollback)
(defalias 'mkcd 'aabm/eshell-mkcd)
(defalias 'sopen 'aabm/eshell-sudo-open)

(defvar aabm/eshell-minor-mode-map
  (let ((map (make-sparse-keymap))) map)
  "Key map with custom commands for `eshell'.")

(define-minor-mode aabm/eshell-minor-mode
  "Special minor mode to enable custom keys in `eshell'.

\\{aabm/eshell-minor-mode-map}"
  :init-value nil
  :keymap aabm/eshell-minor-mode-map)

(add-hook 'eshell-mode-hook 'aabm/eshell-minor-mode-map)
