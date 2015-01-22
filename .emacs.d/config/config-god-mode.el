(defun my-update-cursor ()
  (setq cursor-type (if god-local-mode
                        'hbar
                      t)
        )
  )
(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

(provide 'config-god-mode)
