(require 'minimap)
(require 'settings)
(setq minimap-enlarge-certain-faces 'as-fallback
      minimap-hide-fringes t
      minimap-show-mode-line nil
      minimap-major-modes '(prog-mode text-mode)
      minimap-minimum-width 30
      minimap-semantic-location 'top
      minimap-width-fraction 0.0001
      minimap-window-location 'right
      )

(set-face-attribute 'minimap-active-region-background nil
                    :background "#221520"
                    )
(set-face-attribute 'minimap-font-face nil
                    :height 20
                    )
(set-face-attribute 'minimap-highlight-line-face nil
                    :background config-current-line-color
                    :foreground 'unspecified
                    )
(set-face-attribute 'minimap-semantic-function-face nil
                    :inherit 'font-lock-function-face
                    :background (face-background 'default)
                    :box (list :line-width 1 :color config-border-color)
                    :height 3.75
                    )
(set-face-attribute 'minimap-semantic-type-face nil
                    :inherit 'font-lock-type-face
                    :background (face-background 'default)
                    :box (list :line-width 1 :color config-border-color)
                    :height 3.75
                    )
(set-face-attribute 'minimap-semantic-variable-face nil
                    :inherit 'font-lock-variable-name-face
                    :background (face-background 'default)
                    :box (list :line-width 1 :color config-border-color)
                    :height 3.75
                    )

(provide 'config-minimap)
