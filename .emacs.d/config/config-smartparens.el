;;; Advice that shows off-screen matching parens
;; (require 's)
;; (require 'cl)

;; (defadvice sp-show--pair-function
;;   (after sp-show--pair-function-off-screen activate)
;;   (interactive)
;;   (let* ((beg (window-start))
;;          (end (window-end))
;;          (pt (point))
;;          (other (flet
;;                     ;; suppress `message' inside `sp-get-sexp'
;;                     ((message (&rest args) nil)
;;                      )
;;                   (if (= pt
;;                          (sp-get (sp-get-sexp nil) :beg)
;;                          )
;;                       (1- (sp-get (sp-get-sexp nil) :end))
;;                     (if (= pt
;;                            (sp-get (sp-get-sexp t) :end)
;;                            )
;;                         (sp-get (sp-get-sexp t) :beg)
;;                       nil
;;                       )
;;                     )
;;                   )
;;                 )
;;          )
;;     (message (format "%d %d %d %d" beg end pt other))
;;     (when (and other
;;                (or (> beg other)
;;                    (< end other)
;;                    )
;;                )
;;       (message
;;        (concat
;;         (format "%d: " (line-number-at-pos other))
;;         (save-excursion
;;           (goto-char other)
;;           (s-trim
;;            (let ((line (thing-at-point 'line))
;;                  (idx (- other (line-beginning-position)))
;;                  )
;;              (add-text-properties idx (1+ idx) '(face highlight) line)
;;              line
;;              )
;;            )
;;           )
;;         )
;;        )
;;       )
;;     )
;;   )

;; TODO: write `wrap-region' and `close-parens'

(show-smartparens-global-mode t)

(provide 'config-smartparens)
