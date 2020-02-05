;; Customized filter: don't mark *all* identifiers
(defun amitp/rainbow-identifiers-filter (beg end)
  "Only highlight standalone words or those following 'this.' or 'self.'"
  (let ((curr-char (char-after beg))
        (prev-char (char-before beg))
        (prev-self (buffer-substring-no-properties
                    (max (point-min) (- beg 5)) beg)))
    (and (not (member curr-char
                    '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ??)))
         (or (not (equal prev-char ?\.))
             (equal prev-self "self.")
             (equal prev-self "this.")))))

;; Filter: don't mark identifiers inside comments or strings
(setq rainbow-identifiers-faces-to-override
      '(font-lock-type-face
        font-lock-variable-name-face
        font-lock-function-name-face))

;; Set the filter
(add-hook 'rainbow-identifiers-filter-functions 'amitp/rainbow-identifiers-filter)

(setq rainbow-identifiers-choose-face-function
      'rainbow-identifiers-cie-l*a*b*-choose-face)
(setq rainbow-identifiers-cie-l*a*b*-lightness 75)
(setq rainbow-identifiers-cie-l*a*b*-saturation 45)
(setq rainbow-identifiers-cie-l*a*b*-color-count 15)

(provide 'config-rainbow-identifiers)
