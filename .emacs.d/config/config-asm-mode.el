(require 'settings)

(defun my-asm-mode-hook ()
  ;; you can use `comment-dwim' (M-;) for this kind of behaviour anyway
  (local-unset-key (vector asm-comment-char))
  ;; asm-mode sets it locally to nil, to "stay closer to the old TAB behaviour".
  (setq tab-always-indent (default-value 'tab-always-indent))

  ;; Stop indenting everything!
  (define-key asm-mode-map (kbd "<return>") 'newline)
  (electric-indent-mode -1)
  )


(add-hook 'asm-mode-hook #'my-asm-mode-hook)

(provide 'config-asm-mode)
