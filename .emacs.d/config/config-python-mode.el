(require 'settings)

(when (boundp 'company-backends)
  (add-to-list 'company-backends 'company-anaconda)
  )

(defun prelude-python--encoding-comment-required-p ()
  (re-search-forward "[^\0-\177]" nil t))

(defun prelude-python--detect-encoding ()
  (let ((coding-system
         (or save-buffer-coding-system
             buffer-file-coding-system)))
    (if coding-system
        (symbol-name
         (or (coding-system-get coding-system 'mime-charset)
             (coding-system-change-eol-conversion coding-system nil)))
      "ascii-8bit")))

(defun prelude-python--insert-coding-comment (encoding)
  (let ((newlines (if (looking-at "^\\s *$") "\n" "\n\n")))
    (insert (format "# coding: %s" encoding) newlines)))

(defun prelude-python-mode-set-encoding ()
  "Insert a magic comment header with the proper encoding if necessary."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (prelude-python--encoding-comment-required-p)
      (goto-char (point-min))
      (let ((coding-system (prelude-python--detect-encoding)))
        (when coding-system
          (if (looking-at "^#!") (beginning-of-line 2))
          (cond ((looking-at "\\s *#\\s *.*\\(en\\)?coding\\s *:\\s *\\([-a-z0-9_]*\\)")
                 ;; update existing encoding comment if necessary
                 (unless (string= (match-string 2) coding-system)
                   (goto-char (match-beginning 2))
                   (delete-region (point) (match-end 2))
                   (insert coding-system)))
                ((looking-at "\\s *#.*coding\\s *[:=]"))
                (t (prelude-python--insert-coding-comment coding-system)))
          (when (buffer-modified-p)
            (basic-save-buffer-1)))))))

(when (fboundp 'exec-path-from-shell-copy-env)
  (exec-path-from-shell-copy-env "PYTHONPATH"))

(defun config-python-mode-defaults ()
  "Defaults for Python programming."
  (subword-mode t)
  (anaconda-mode)
  (defadvice anaconda-mode-goto (after anaconda-mode-goto-recenter activate compile)
    (recenter)
    )
  (diminish 'anaconda-mode)
  (add-hook 'after-save-hook 'prelude-python-mode-set-encoding nil 'local)
  (setq tab-width config-tab-width)
  (setq python-indent config-tab-width)
  (setq electric-indent-chars (delq ?: electric-indent-chars))
  )

(add-hook 'python-mode-hook 'config-python-mode-defaults)

(provide 'config-python-mode)
