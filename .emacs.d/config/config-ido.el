(require 'ido)
(require 'ido-ubiquitous)
(require 'flx-ido)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-save-directory-list-file (expand-file-name
                                    "ido.hist" config-savefiles-dir)
      ido-enable-tramp-completion nil
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1
      ido-file-extensions-order config-preferred-extensions
      ido-ignore-extensions t
      completion-ignored-extensions config-ignored-extensions
      )

                                        ; sort ido filelist by mtime instead of alphabetically
(add-hook 'ido-make-file-list-hook 'ido-custom-sort)
(add-hook 'ido-make-dir-list-hook 'ido-custom-sort)
(defun ido-custom-sort ()
  ;; sort by mtime
  (setq ido-temp-list
        (sort ido-temp-list
              (lambda (a b)
                (time-less-p
                 (sixth (file-attributes (concat ido-current-directory b)))
                 (sixth (file-attributes (concat ido-current-directory a)))
                 )
                )
              )
        )
  ;; move .-files and current file out of the way
  (ido-to-end
   (delq nil
         (mapcar
          (lambda (x) (and (or
                            (equal (buffer-file-name) (file-truename x))
                            (char-equal (string-to-char x) ?.)
                            )
                           x)
            )
          ido-temp-list
          )
         )
   )
  )

;; Enable ido mode nearly everywhere
(ido-mode t)
(ido-ubiquitous-mode t)

;;; Smarter fuzzy matching for ido
(flx-ido-mode t)
;; Disable ido faces to see flx highlights
(setq ido-use-faces nil)

;;; smex, remember recently and most frequently used commands
(require 'smex)
(setq smex-save-file (expand-file-name "smex-items" config-savefiles-dir))
(smex-initialize)

;; Do not confirm a new file or buffer
(setq-default confirm-nonexistent-file-or-buffer nil)

(provide 'config-ido)
