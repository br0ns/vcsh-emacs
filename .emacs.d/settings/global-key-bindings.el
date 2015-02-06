;;; Navigation
(global-set-key [remap next-line]           'nav-next-line)
(global-set-key [remap previous-line]       'nav-previous-line)
(global-set-key [remap forward-paragraph]   'nav-forward-blank-line)
(global-set-key [remap backward-paragraph]  'nav-backward-blank-line)
(global-set-key (kbd "M-<down>")            'nav-scroll-next-line)
(global-set-key (kbd "M-<up>")              'nav-scroll-previous-line)
(global-set-key (kbd "C-M-<down>")          'nav-scroll-forward-blank-line)
(global-set-key (kbd "C-M-<up>")            'nav-scroll-backward-blank-line)
(global-set-key (kbd "M-S-<down>")          'nav-adjust-next-line)
(global-set-key (kbd "M-S-<up>")            'nav-adjust-previous-line)
(global-set-key [remap scroll-down-command] 'nav-half-page-up)
(global-set-key [remap scroll-up-command]   'nav-half-page-down)
(global-set-key [remap move-beginning-of-line]
                'prelude-move-beginning-of-line)
(global-set-key (kbd "C-}")                 'sp-forward-sexp)
(global-set-key (kbd "C-{")                 'sp-backward-sexp)
;; (global-set-key (kbd "M-{")                 'smart-backward)
;; (global-set-key (kbd "M-}")                 'smart-forward)
;; (global-set-key (kbd "M-]")              'close-parens)
;; (global-set-key (kbd "(")                'wrap-parens)
;; (global-set-key (kbd "[")                'wrap-parens)
;; (global-set-key (kbd "{")                'wrap-parens)
;; (global-set-key (kbd "<")                'wrap-parens)
(global-set-key (kbd "M-s")              'prelude-goto-symbol)
(global-set-key (kbd "M-g")              'goto-line-with-feedback)

;;; File functions
(global-set-key (kbd "C-x t")            'touch-buffer-file)
(global-set-key (kbd "C-x C-r")          'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k")          'delete-current-buffer-file)

;;; File finding
(global-set-key (kbd "C-x M-f")          'ido-find-file-other-window)
(global-set-key (kbd "H-r")              'prelude-recentf-ido-find-file)
(global-set-key (kbd "C-c y")            'bury-buffer)

;;; Bookmarks
(global-set-key (kbd "<f2>")             'bm-toggle)
(global-set-key (kbd "C-S-<right>")      'bm-next)
(global-set-key (kbd "C-S-<left>")       'bm-previous)

;;; Move text (line or region)
(global-set-key (kbd "C-S-<up>")         'move-text-up)
(global-set-key (kbd "C-S-<down>")       'move-text-down)

;;; smex
(global-set-key (kbd "M-x")              'smex)
(global-set-key (kbd "M-X")              'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x")      'execute-extended-command)

;; Clever newlines
(global-set-key (kbd "<S-return>")       'open-line-above)
(global-set-key (kbd "<M-return>")       'open-line-below)
(global-set-key (kbd "<M-S-return>")     'break-line)
(global-set-key (kbd "H-b")              'break-line)

;;; Company mode
(require 'company)
(define-key company-mode-map   (kbd config-auto-complete-key)
  'company-complete)
(define-key company-active-map (kbd config-auto-complete-key)
  'company-select-next)
(define-key company-active-map (kbd (concat "S-" config-auto-complete-key))
  'company-select-previous)
(define-key company-active-map (kbd "SPC")
  'company-complete-selection)
(define-key company-active-map (kbd "<escape>")
  'company-abort)
(define-key company-active-map (kbd "?")
  'company-show-doc-buffer)

;;; Anaconda
(global-set-key (kbd "M-,")              'anaconda-nav-pop-marker)

;;; Misc
(global-set-key (kbd "M-/")              'hippie-expand)
(global-set-key (kbd "M-d")              'duplicate-current-line-or-region)
;; move `kill-word' since it used to be at `M-d'
(global-set-key (kbd "C-S-d")            'kill-word)
(global-set-key (kbd "C-z")              'ido-switch-buffer)
(global-set-key (kbd "C-q")              'comment-dwim)
(global-set-key (kbd "<f5>")             'revert-buffer-noconfirm)
(global-set-key (kbd "C->")              'increase-indentation)
(global-set-key (kbd "C-<")              'decrease-indentation)
(global-set-key (kbd "<escape>")         'god-local-mode)
(global-set-key (kbd "C-c e")            'eval-and-replace)

(global-set-key [remap kill-line]        'kill-or-join-next-line)
(global-set-key (kbd "H-j")              'join-next-line)

;;; evil-numbers
(global-set-key (kbd "C-M-<right>")      'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-M-<left>")       'evil-numbers/dec-at-pt)

;;; Anzu
(global-set-key (kbd "M-%")              'anzu-query-replace)
(global-set-key (kbd "C-M-%")            'anzu-query-replace-regexp)

;;; Projectile
(require 'projectile)
(setq projectile-keymap-prefix (kbd "H-p"))
(define-key projectile-mode-map (kbd "H-d") 'projectile-find-dir)
(define-key projectile-mode-map (kbd "H-p") 'projectile-switch-project)
(define-key projectile-mode-map (kbd "H-f") 'projectile-find-file)
(define-key projectile-mode-map (kbd "H-g") 'projectile-grep)

;;; Flyspell
(require 'flyspell)
(define-key flyspell-mode-map (kbd "<f6>")   'flyspell-correct-word-before-point)
(define-key flyspell-mode-map (kbd "C-<f6>") 'ispell-change-dictionary)

;;; discover-my-major.el
(global-set-key (kbd "C-h C-m")          'discover-my-major)

;; ace-jump-mode.el
(define-key global-map (kbd "C-\\")      'ace-jump-word-mode)
(define-key global-map (kbd "C-M-\\")    'ace-jump-buffer)

;; multifiles.el
(global-set-key (kbd "C-!")              'mf/mirror-region-in-multifile)

;; multiple-cursors.el
(global-set-key (kbd "C-S-SPC")          'set-rectangular-region-anchor)
(global-set-key (kbd "C-S-c C-c")        'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-.")              'mc/mark-next-like-this)
(global-set-key (kbd "C-M-.")            'mc/skip-and-mark-next-like-this)
(global-set-key (kbd "C-,")              'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-,")            'mc/skip-and-mark-previous-like-this)
(global-set-key (kbd "C-;")              'mc/mark-all-in-region)
(global-set-key (kbd "C-:")              'mc/mark-more-like-this-extended)

;; expand-region.el
(global-set-key (kbd "C-'")              'er/expand-region)
(global-set-key (kbd "C-M-'")            'er/contract-region)

;; browse-kill-ring.el
(global-set-key (kbd "C-x C-y")          'browse-kill-ring)
(global-set-key (kbd "H-y")              'browse-kill-ring)

;; zoom-frm.el
(global-set-key (kbd "s--")              'zoom-frm-out)
(global-set-key (kbd "s-=")              'zoom-frm-in)
(global-set-key (kbd "C-0")              'zoom-frm-unzoom)

;; For each complete line between point and mark, move to the beginning
;; of the line, and run the last keyboard macro.
(global-set-key (kbd "C-<f4>")           'apply-macro-to-region-lines)

;;; Window switching
;; Bindings set up by `windmove':
;; S-<left>  : windmove-left
;; S-<right> : windmove-right
;; S-<up>    : windmove-up
;; S-<down>  : windmove-down
(windmove-default-keybindings)
(global-set-key (kbd "C-o")              'other-window)
(global-set-key (kbd "C-M-o")            (lambda ()
                                           (interactive)
                                           (other-window -1)
                                           )
                )
(global-set-key (kbd "C-|")              'toggle-window-split)
(global-set-key (kbd "C-S-o")            'defuns-windows-rotate-windows)
(global-set-key (kbd "C-S-M-o")          (lambda ()
                                           (interactive)
                                           (defuns-windows-rotate-windows -1)
                                           )
                )

;;; Toggle visual bling
(global-set-key (kbd "<f12>")            'minimap-mode)
(global-set-key (kbd "<f11>")            'toggle-mode-line)
(global-set-key (kbd "M-<f11>")          'menu-bar-mode)

(provide 'global-key-bindings)
