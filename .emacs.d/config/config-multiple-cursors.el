(require 'multiple-cursors)

;; List of allowed functions in mc-mode
(setq mc/list-file (expand-file-name "mc-lists.el" config-settings-dir))
(load mc/list-file t)

;; Disable multiple cursons on save
(add-hook 'before-save-hook 'mc/keyboard-quit)

;;; borrowed from http://www.jeffchu.com/posts/2013/01/2013-01-29-multiple-cursors-mode.html
(defvar mc-search--last-term nil)

(defun mc-search (search-command)
  ;; Read new search term when not repeated command or applying to fake cursors
  (when (and (not mc--executing-command-for-fake-cursor)
             (not (eq last-command 'mc-search-forward))
             (not (eq last-command 'mc-search-backward)))
    (setq mc-search--last-term (read-from-minibuffer "Search: ")))
  (funcall search-command mc-search--last-term))

(defun mc-search-forward ()
  "Simplified version of forward search that supports multiple cursors"
  (interactive)
  (mc-search 'search-forward))

(defun mc-search-backward ()
  "Simplified version of backward search that supports multiple cursors"
  (interactive)
  (mc-search 'search-backward))

(define-key mc/keymap (kbd "C-s") 'mc-search-forward)
(define-key mc/keymap (kbd "C-r") 'mc-search-backward)

(provide 'config-multiple-cursors)
