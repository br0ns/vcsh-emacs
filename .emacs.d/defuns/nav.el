;; Remember temporary-goal-column even when scrolling and jumping by
;; blank-lines

;; TODO: get to work with `track-eol.el'
;; TODO: make a proper package, like `whole-line-or-region.el'
;; TODO: make work with `line-move-visual'

(defcustom nav-keep-going nil
  "Non-nil to let `nav-scroll-*' commands keep moving even though
the buffer can't scroll further."
  :type 'boolean
  )

(defconst nav-keep-column-commands
  '(nav-scroll-forward-paragraph
    nav-scroll-backward-paragraph
    nav-scroll-forward-blank-line
    nav-scroll-backward-blank-line
    nav-scroll-next-line
    nav-scroll-previous-line

    nav-forward-paragraph
    nav-backward-paragraph
    nav-forward-blank-line
    nav-backward-blank-line
    nav-next-line
    nav-previous-line

    nav-page-down
    nav-page-up
    nav-half-page-down
    nav-half-page-up
    )
  )

(defvar nav-temporary-goal-column 0
  )
(make-variable-buffer-local 'nav-temporary-goal-column)

(defun forward-blank-line (&optional arg)
  "Move cursor forward to the beginning of next text block.
A text block is separated by 2 empty lines (or line with just whitespace).
In most major modes, this is similar to `forward-paragraph', but this command's behavior is the same regardless of syntax table."
  (interactive)
  (if (search-forward-regexp "\n[[:blank:]\n]*\n+" nil "NOERROR")
      (progn (backward-char))
    (progn (goto-char (point-max)) )
    )
  )

(defun backward-blank-line (&optional arg)
  "Move cursor backward to previous text block.
See: `ergoemacs-forward-block'"
  (interactive)
  (if (search-backward-regexp "\n[\t\n ]*\n+" nil "NOERROR")
      (progn
        (skip-chars-backward "\n\t ")
        (forward-char 1)
        )
    (progn (goto-char (point-min)) )
    )
  )

(defun nav-is-nav-function (fn)
  (memq fn nav-keep-column-commands)
  )

(defun nav-update-goal-column ()
  "Update `nav-temporary-goal-column' if necessary."
  (unless (nav-is-nav-function last-command)
    (when (< (current-column) (1- (window-width)))
      (setq nav-temporary-goal-column (current-column))
      )
    )
  )

(defun nav-move-to-column (column)
  (when (< (current-column) (1- (window-width)))
    (move-to-column column)
    )
  )

(defun nav-move-to-goal-column ()
  (nav-move-to-column nav-temporary-goal-column)
  )

(defun nav-can-scroll-up ()
  (> (line-number-at-pos (window-start)) 1)
  )

(defun nav-can-scroll-down ()
  (< (window-end) (buffer-end 1))
  )

(defun nav-can-scroll (lines)
  (let ((avail (if (< lines 0)
                   (count-screen-lines (point-min) (window-start))
                 (count-screen-lines (point-max) (window-end))
                 )
               )
        (lines (abs lines))
        )
    (<= lines avail)
    )
  )

(defun nav-call-abstract (fn args)
  (nav-update-goal-column)
  (let ((tgc temporary-goal-column)
        )
    (apply 'funcall fn args)
    (setq temporary-goal-column tgc)
    )
  (nav-move-to-goal-column)
  )

(defun nav-call (fn &rest args)
  (nav-call-abstract fn args)
)

(defun nav-scroll-call (fn &rest args)
  (let* ((point-before (point))
         (point-after (save-excursion
                        (nav-call-abstract fn args)
                        (point)
                        )
                      )
         (going-up (< point-after point-before))
         (scroll-lines
          (1- (count-screen-lines point-after point-before t))
          )
         (scroll-lines (if going-up
                           (- scroll-lines)
                         scroll-lines
                         )
                       )
         (can-scroll (if going-up
                         (nav-can-scroll-up)
                       (nav-can-scroll-down)
                       )
                     )
         )
    (when (or nav-keep-going can-scroll)
      (condition-case nil
          (scroll-up scroll-lines)
        (error nil)
        )
      (goto-char point-after)
      )
    )
  )

(defun nav-adjust-call (fn &rest args)
  (let* ((point-before (point))
         (point-after (save-excursion
                        (nav-call-abstract fn args)
                        (point)
                        )
                      )
         (going-up (< point-after point-before))
         (scroll-lines
          (1- (count-screen-lines point-after point-before t))
          )
         (scroll-lines (if going-up
                           (- scroll-lines)
                         scroll-lines
                         )
                       )
         (can-scroll (if going-up
                         (nav-can-scroll-up)
                       (nav-can-scroll-down)
                       )
                     )
         )
    (when (or nav-keep-going can-scroll)
      (condition-case nil
          (scroll-up scroll-lines)
        (error nil)
        )
      (goto-char point-before)
      )
    )
  )

(defun nav-adjust-forward-paragraph (&optional arg)
  "Adjust down ARG blank-lines keeping point fixed."
  (interactive "p")
  (nav-adjust-call 'forward-paragraph arg)
  )

(defun nav-adjust-backward-paragraph (&optional arg)
  "Adjust down ARG blank-lines keeping point fixed."
  (interactive "p")
  (nav-adjust-call 'backward-paragraph arg)
  )

(defun nav-adjust-forward-blank-line (&optional arg)
  "Adjust down ARG blank-lines keeping point fixed."
  (interactive "p")
  (nav-adjust-call 'forward-blank-line arg)
  )

(defun nav-adjust-backward-blank-line (&optional arg)
  "Adjust down ARG blank-lines keeping point fixed."
  (interactive "p")
  (nav-adjust-call 'backward-blank-line arg)
  )

(defun nav-adjust-next-line (&optional arg)
  (interactive "p")
  (nav-adjust-call 'next-line arg)
  )

(defun nav-adjust-previous-line (&optional arg)
  (interactive "p")
  (nav-adjust-call 'previous-line arg)
  )

(defun nav-scroll-forward-paragraph (&optional arg)
  "Scroll down ARG blank-lines keeping point fixed."
  (interactive "p")
  (nav-scroll-call 'forward-paragraph arg)
  )

(defun nav-scroll-backward-paragraph (&optional arg)
  "Scroll down ARG blank-lines keeping point fixed."
  (interactive "p")
  (nav-scroll-call 'backward-paragraph arg)
  )

(defun nav-scroll-forward-blank-line (&optional arg)
  "Scroll down ARG blank-lines keeping point fixed."
  (interactive "p")
  (nav-scroll-call 'forward-blank-line arg)
)

(defun nav-scroll-backward-blank-line (&optional arg)
  "Scroll down ARG blank-lines keeping point fixed."
  (interactive "p")
  (nav-scroll-call 'backward-blank-line arg)
)

(defun nav-scroll-next-line (&optional arg)
  (interactive "p")
  (nav-scroll-call 'next-line arg)
  )

(defun nav-scroll-previous-line (&optional arg)
  (interactive "p")
  (nav-scroll-call 'previous-line arg)
  )

(defun nav-next-line (&optional arg)
  (interactive "p")
  (nav-call 'next-line arg)
  )

(defun nav-previous-line (&optional arg)
  (interactive "p")
  (nav-call 'previous-line arg)
  )

(defun nav-backward-blank-line (&optional arg)
  (interactive "p")
  (nav-call 'backward-blank-line arg)
  )

(defun nav-forward-blank-line (&optional arg)
  (interactive "p")
  (nav-call 'forward-blank-line arg)
  )

(defun nav-backward-paragraph (&optional arg)
  (interactive "p")
  (nav-call 'backward-paragraph arg)
  )

(defun nav-forward-paragraph (&optional arg)
  (interactive "p")
  (nav-call 'forward-paragraph arg)
  )

(defun nav-half-page-up ()
  "Moves buffer half a page, without moving point"
  (interactive)
  (nav-scroll-call
   '(lambda ()
      (previous-line (/ (window-height) 2))
      )
   )
  )

(defun nav-half-page-down ()
  "Moves buffer half a page, without moving point"
  (interactive)
  (nav-scroll-call
   '(lambda ()
      (next-line (/ (window-height) 2))
      )
   )
  )

(defun nav-page-up ()
  "Moves buffer a page, without moving point"
  (interactive)
  (nav-scroll-call
   '(lambda ()
      (previous-line (window-height))
      )
   )
  )

(defun nav-page-down ()
  "Moves buffer a page, without moving point"
  (interactive)
  (nav-scroll-call
   '(lambda ()
      (next-line (window-height))
      )
   )
  )

(provide 'nav)
