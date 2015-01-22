(require 'flyspell)

(defun config-enable-flyspell ()
  "Enable command `flyspell-mode' if `config-flyspell' is not nil."
  (when config-flyspell
    (flyspell-mode t)
    )
  )

(define-key flyspell-mode-map (kbd "C-.") nil)
(define-key flyspell-mode-map (kbd "C-;") nil)

(add-hook 'text-mode-hook 'config-enable-flyspell)

(provide 'config-flyspell)
