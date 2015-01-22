(require 'smart-mode-line)
(display-time)
(column-number-mode t)
(setq sml/no-confirm-load-theme t)
(setq sml/shorten-directory t)
(sml/apply-theme 'powerline)

(sml/setup)

(provide 'config-smart-mode-line)
