;; Core editor settings

;; Don't indent with tabs
(setq-default indent-tabs-mode nil)

;; Set default tab width; defined in `settings.el'
(setq-default tab-width config-tab-width)

;; Unbind `suspend-frame'
(global-unset-key [(control x)(control z)])

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Newline at end of file
(setq require-final-newline t)

;; Save all backup files here
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "auto-backup" config-savefiles-dir))
        )
      version-control t                ; Use version numbers for backups
      kept-new-versions 16             ; Number of newest versions to keep
      kept-old-versions 2              ; Number of oldest versions to keep
      delete-old-versions t            ; Ask to delete excess backup versions?
      backup-by-copying-when-linked t) ; Copy linked files, don't rename.

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Keep auto-save lists here
(setq auto-save-list-file-prefix
      (expand-file-name "auto-save-list/saves-" config-savefiles-dir)
      )
;; Keep auto-save files here
(setq auto-save-file-name-transforms
      `(("\\(?:[^/]*/\\)*\\(.*\\)"
         ,(expand-file-name "auto-save/\\1" config-savefiles-dir) t)
        )
      )

;; Save every n keystrokes
(setq auto-save-interval 100)

;; Save every n idle seconds
(setq auto-save-timeout 10)

;; Autosave the undo-tree history
(setq undo-tree-history-directory-alist
      `((".*" . ,(expand-file-name "undo-tree" config-savefiles-dir))
        )
      )
(setq undo-tree-auto-save-history t)

;; Enable syntax highlighting everywhere
(global-font-lock-mode t)

;; disable annoying blink-matching-paren
(setq blink-matching-paren nil)

;; Create dir if non-existing
(add-hook 'before-save-hook
          '(lambda ()
             (or (file-exists-p (file-name-directory buffer-file-name))
                 (make-directory (file-name-directory buffer-file-name) t))))

;; Delete trailing white space when saving
(when config-clean-whitespace-on-save
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'after-save-hook
            '(lambda ()
               (message (concat
                         "Wrote "
                         (buffer-file-name)
                         " (and removed trailing whitespace)"))))
  )


;; Remove minor modes from the mode line
(require 'diminish)

;; Unique names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; Saveplace remembers your location in a file when saving files
(require 'saveplace)
(setq save-place-file (expand-file-name "saveplace" config-savefiles-dir))
;; Activate it for all buffers
(setq-default save-place t)

;; Savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" config-savefiles-dir))
(savehist-mode t)

;; Save recent files
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" config-savefiles-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)

(defun prelude-recentf-exclude-p (file)
  "A predicate to decide whether to exclude FILE from recentf."
  (let ((file-dir (file-truename (file-name-directory file)))
        )
    (-any-p (lambda (dir)
              (string-prefix-p dir file-dir))
            (mapcar 'file-truename (list config-savefiles-dir package-user-dir))
            )
    )
  )

(add-to-list 'recentf-exclude 'prelude-recentf-exclude-p)
(add-to-list 'recentf-exclude config-recentf-exclude)
(recentf-mode t)

;; Highlight current line
(global-hl-line-mode t)
(set-face-background hl-line-face config-current-line-color)

;; Unto tree
(require 'undo-tree)
(diminish 'undo-tree-mode)
(global-undo-tree-mode)

;; Saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

;; Always end searches at the beginning of the matching expression.
(defun custom-goto-match-beginning ()
  "Use with isearch hook to end search at first char of match."
  (when isearch-forward (goto-char isearch-other-end)))
(add-hook 'isearch-mode-end-hook 'custom-goto-match-beginning)

;; Anzu mode enhances isearch & query-replace by showing total matches
;; and current match position
(require 'anzu)
(diminish 'anzu-mode)
(global-anzu-mode)

;; ;; smart-forward.el
;; (require 'smart-forward)

;; Set fill column
(set-default 'fill-column config-fill-column)

;; Fill rules
;; TODO: fix this
;; (require 'filladapt)
;; (diminish 'filladapt-mode)
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Enable functions that are disabled by default
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Mouse yank at point
(setq mouse-yank-at-point t)

;; Default to text mode
(setq-default major-mode config-default-major-mode)

;; Multifiles
(require 'multifiles)

;; Browse kill ring
(require 'browse-kill-ring)

;; Make zooming affect frame instead of buffers
(require 'zoom-frm)

;; Mark lines that are too long
(when config-show-whitespace
  (require 'whitespace)
  (diminish 'global-whitespace-mode)
  (setq whitespace-line-column config-fill-column)
  (setq whitespace-style
        (quote
         (face tabs trailing lines-tail)
         )
        )
  (set-face-attribute 'whitespace-tab nil
                      :background config-whitespace-color
                      )
  (set-face-attribute 'whitespace-trailing nil
                      :background config-whitespace-color
                      )
  (set-face-attribute 'whitespace-line nil
                      :background config-whitespace-color
                      :foreground 'unspecified
                      )
  ;; XXX: this doesn't work for some reason...
  ;; (setq whitespace-line 'highlight)
  ;; (setq whitespace-tab 'highlight)
  ;; (setq whitespace-trailing 'highlight)
  (global-whitespace-mode t)
  )

;; Use projectile mode everywhere
(require 'projectile)
(diminish 'projectile-mode)
;; XXX: this doesn't seem to work
;; (setq projectile-known-projects-file
;;       (expand-file-name  "projectile-bookmarks.eld" config-savefiles-dir)
;;       )
(setq projectile-cache-file
      (expand-file-name  "projectile.cache" config-savefiles-dir)
      )
(projectile-global-mode)

(defadvice exchange-point-and-mark (before deactivate-mark activate compile)
  "When called with no active region, do not activate mark."
  (interactive
   (list (not (region-active-p)))))

(defmacro with-region-or-buffer (func)
  "When called with no active region, call FUNC on current buffer."
  `(defadvice ,func (before with-region-or-buffer activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (point-min) (point-max))))))

(with-region-or-buffer indent-region)
(with-region-or-buffer untabify)

(defmacro advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.

The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command (,class ,(intern (concat (symbol-name command) "-" advice-name)) activate)
                    ,@body))
               commands)))

;; automatically indenting yanked text if in programming-modes
(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) config-yank-indent-threshold)
      (indent-region beg end nil)))

(advise-commands "indent" (yank yank-pop) after
  "If current mode is one of `prelude-yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (not (member major-mode config-indent-sensitive-modes))
           (or (derived-mode-p 'prog-mode)
               (member major-mode config-yank-indent-modes)))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(setq abbrev-file-name (expand-file-name "abbrev-defs" config-savefiles-dir))
(setq save-abbrevs t)
(setq-default abbrev-mode t)
(diminish 'abbrev-mode)
(when (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file)
    )

;; Enable semantic mode everywhere
(semantic-mode t)

(provide 'config-editor)
