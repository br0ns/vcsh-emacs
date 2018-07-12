(require 'whole-line-or-region)
(require 'diminish)
(diminish 'whole-line-or-region-mode)

(defun whole-line-or-region-comment-dwim-3 (prefix)
  "Call `comment-dwim' on region or PREFIX whole lines."
  (interactive "*p")
  (progn
    (whole-line-or-region-kill-ring-save prefix)
    (whole-line-or-region-call-with-prefix 'comment-dwim prefix nil t)
    )
  )

(defun whole-line-or-region-comment-line (prefix)
  "Call `comment-dwim' on region or PREFIX whole lines."
  (interactive "*p")
  (progn
    (whole-line-or-region-kill-ring-save prefix)
    (whole-line-or-region-call-with-prefix 'comment-line prefix nil t)
    )
  )

(add-to-list 'whole-line-or-region-extensions-alist
             '(comment-dwim whole-line-or-region-comment-dwim-3 nil)
             )

(whole-line-or-region-mode 1)

(provide 'config-whole-line-or-region)
