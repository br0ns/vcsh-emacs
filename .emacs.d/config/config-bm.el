;; Bookmars
;; Loading the repository from file when on start up.
(setq bm-restore-repository-on-load t)

(setq bm-repository-file (expand-file-name "bm-repository"
                                           config-savefiles-dir))

(require 'bm)

;; Make bookmarks persistent as default
(setq-default bm-buffer-persistence t)

;; Restoring bookmarks when on file find.
(add-hook 'find-file-hooks 'bm-buffer-restore)

;; Saving bookmark data on killing a buffer
(add-hook 'kill-buffer-hook 'bm-buffer-save)

;; Saving the repository to file when on exit.
;; kill-buffer-hook is not called when emacs is killed, so we
;; must save all bookmarks first.
(add-hook 'kill-emacs-hook '(lambda nil
                              (bm-buffer-save-all)
                              (bm-repository-save)))

;; Set background color of bookmarks
(set-face-background bm-persistent-face config-bookmark-color)

(provide 'config-bm)
