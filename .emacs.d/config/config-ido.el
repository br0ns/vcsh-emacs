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
