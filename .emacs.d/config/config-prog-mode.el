(defun config-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  )

(defun config-font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\|XXX\\):\\)"
          1 'font-lock-warning-face t)
         )
   )
  )

(defun config-prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  (when (and (executable-find ispell-program-name)
             config-flyspell)
    (flyspell-prog-mode)
    )
  (config-local-comment-auto-fill)
  (config-font-lock-comment-annotations)
  )

(setq config-prog-mode-hook 'config-prog-mode-defaults)

(mapc (lambda (hook)
(add-hook hook (lambda () (run-hooks 'config-prog-mode-hook)))
        )
      (cons 'prog-mode-hook config-prog-mode-hook-extra-hooks)
      )

;; enable on-the-fly syntax checking
(if (fboundp 'global-flycheck-mode)
    (global-flycheck-mode t)
  (add-hook 'prog-mode-hook 'flycheck-mode)
  )

(when config-diminish-flycheck
  (add-hook 'flycheck-mode-hook (lambda ()
                                  (diminish 'flycheck-mode)
                                  )
            )
)

(provide 'config-prog-mode)
