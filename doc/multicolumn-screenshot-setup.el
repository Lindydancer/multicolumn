;; multicolumn-screenshot-setup.el --- prepare Emacs for screenshot.

(defvar multicolumn-screenshot-setup-file-name
  (or load-file-name
      (buffer-file-name)))

(let ((dir (file-name-directory multicolumn-screenshot-setup-file-name)))
  (load (concat dir "../multicolumn.el"))
  (find-file (concat dir "quote.txt")))

(setq fill-column 15)
(setq multicolumn-min-width 20)

(global-set-key (kbd "<f7>")
                'multicolumn-delete-other-windows-and-split-with-follow-mode)

(set-frame-size (selected-frame) 100 10)

(message "")

;; multicolumn-screenshot-setup.el ends here
