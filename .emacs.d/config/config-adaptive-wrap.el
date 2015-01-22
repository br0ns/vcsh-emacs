(require 'adaptive-wrap)

(defun turn-on-adaptive-wrap-prefix-mode (&optional arg)
  (interactive)
  (adaptive-wrap-prefix-mode (or arg 1)))

(defun turn-off-adaptive-wrap-prefix-mode (&optional arg)
  (interactive)
  (adaptive-wrap-prefix-mode (or arg -1)))

(defun adaptive-wrap-initialize ()
  (unless (minibufferp)
    (setq adaptive-wrap-extra-indent 2)
    (setq adaptive-wrap-extra-indent-char ?.)
    (adaptive-wrap-prefix-mode 1)))

;;;###autoload (autoload 'adaptive-wrap-mode "adaptive-wrap")
(define-globalized-minor-mode adaptive-wrap-mode
  adaptive-wrap-prefix-mode adaptive-wrap-initialize)

(adaptive-wrap-mode)

(provide 'config-adaptive-wrap)
