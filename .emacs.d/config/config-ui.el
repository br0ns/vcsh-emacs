;; Clean up the UI

;; Reclaim some screen space
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1)
  )
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1)
  )
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1)
  )

;; Please don't blink
(blink-cursor-mode -1)

;; Disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 5)
(setq scroll-conservatively 10)
(setq scroll-preserve-screen-position 1)

;; Disable the fring
(if (fboundp 'fringe-mode)
    (fringe-mode 0)
  )

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; More useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("E: " (:eval (if (buffer-file-name)
                         (abbreviate-file-name (buffer-file-name))
                       "%b"))
        )
      )

;; Set the default theme
(load-theme config-theme t)

;; Set color of vertical borders between windows
(set-face-attribute 'vertical-border
                    nil
                    :foreground config-border-color)

(provide 'config-ui)
