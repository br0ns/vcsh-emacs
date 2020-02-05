(defun config-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  )

(defface comment-annotation-success
  '((t (:weight bold :foreground "#8ae234")))
  ""
  )
(defface comment-annotation-failure
  '((t (:weight bold :foreground "#ef2929")))
  ""
  )
(defface comment-annotation-comment
  '((t (:weight bold :foreground "#729fcf")))
  ""
  )
(defface comment-annotation-warning
  '((t (:weight bold :foreground "#f57900")))
  ""
  )

(defun config-font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(
         ;; NOTE: Mucho      NB: importante     HACK: I use this marker often...
         ("\\<\\(\\(NOTE\\|NB\\|HACK\\):\\)"
          1 'comment-annotation-warning t)

         ;; TODO: Later      XXX: I said, later!
         ("\\<\\(\\(TODO\\|XXX\\)\\(\\[[^]]*\\]\\)?:\\)"
          1 'comment-annotation-failure t)

         ;; TODO[OK]: A-ok   XXX[fixed]: No, really
         ("\\<\\(\\(TODO\\|XXX\\)\\[\\([^]]*\\(OK\\|Ok\\|ok\\|FIXED\\|Fixed\\|fixed\\)[^]]*\\)\\]:\\)"
          1 'comment-annotation-success t)
         ("\\<\\(\\(TODO\\|XXX\\)\\[\\([^]]*\\)\\]:\\)"
          3 'comment-annotation-comment t)

         ;; CONSIDER: Rewriting .emacs'n'friends
         ("\\<\\(\\(CONSIDER\\):\\)"
          1 'comment-annotation-comment t)

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
  (add-hook 'prog-mode-hook 'flycheck-prog-mode)
  )

(when config-diminish-flycheck
 (add-hook 'flycheck-mode-hook (lambda ()
                                 (diminish 'flycheck-mode)
                                 )
           )
)

(provide 'config-prog-mode)
