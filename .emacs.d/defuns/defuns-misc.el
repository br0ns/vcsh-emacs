(defun toggle-mode-line () "toggles the modeline on and off"
  (interactive)
  (setq mode-line-format
        (if (equal mode-line-format nil)
            (default-value 'mode-line-format)) )
  (redraw-display))

(defun revert-buffer-noconfirm ()
  "Call `revert-buffer' with the NOCONFIRM argument set."
  (interactive)
  (progn
    (revert-buffer nil t)
    (message "Buffer reverted")
    )
  )

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively 'goto-line))
    (linum-mode -1)))

;; Add spaces and proper formatting to linum-mode. It uses more room than
;; necessary, but that's not a problem since it's only in use when going to
;; lines.
(setq linum-format (lambda (line)
  (propertize
   (format (concat " %"
                   (number-to-string
                    (length (number-to-string
                             (line-number-at-pos (point-max)))))
                   "d ")
           line)
   'face 'linum)))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


(require 'imenu)
(defun prelude-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (cond
   ((not symbol-list)
    (let (name-and-pos symbol-names position)
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (prelude-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (completing-read "Symbol? " (reverse symbol-names)))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))
      (recenter)))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (prelude-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names (substring-no-properties name))
          (add-to-list 'name-and-pos (cons (substring-no-properties name) position))))))))

;; Copy'n'paste of `flyspell-check-previous-highlighted-word`, but call
;; `flyspell-correct-word-before-point` instead of `ispell-word` to get Ace
;; popup.
(defun flyspell-check-previous-highlighted-word-2 (&optional arg)
  "Correct the closest previous word that is highlighted as misspelled.
This function scans for a word which starts before point that has been
highlighted by Flyspell as misspelled.  If it finds one, it proposes
a replacement for that word.  With prefix arg N, check the Nth word
before point that's highlighted as misspelled."
  (interactive "P")
  (let ((pos1 (point))
	(pos  (point))
	(arg  (if (or (not (numberp arg)) (< arg 1)) 1 arg))
	ov ovs)
    (if (catch 'exit
	  (while (and (setq pos (previous-overlay-change pos))
		      (not (= pos pos1)))
	    (setq pos1 pos)
	    (if (> pos (point-min))
		(progn
		  (setq ovs (overlays-at pos))
		  (while (consp ovs)
		    (setq ov (car ovs))
		    (setq ovs (cdr ovs))
		    (if (and (flyspell-overlay-p ov)
			     (= 0 (setq arg (1- arg))))
			(throw 'exit t)))))))
	(save-excursion
	  (goto-char pos)
      (flyspell-correct-word-before-point)
      ;; (ispell-word)
	  (setq flyspell-word-cache-word nil) ;; Force flyspell-word re-check
	  (flyspell-word))
      (error "No word to correct before point"))))

(setq-default shell-file-name "/bin/bash")

(defun insert-current-date () (interactive)
       (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(defun insert-current-time () (interactive)
       (insert (shell-command-to-string "echo -n $(date +%T)")))

(defun insert-current-date-long () (interactive)
       (insert (shell-command-to-string "echo -n $(date +'%A, %B %e, %Y')")))

(provide 'defuns-misc)
