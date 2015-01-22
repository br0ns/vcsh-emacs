(defun increase-indentation ()
  "Increase identation of region if mark is active"
  (interactive)
  (if mark-active
      (let ((deactivate-mark nil))
        (indent-rigidly (region-beginning) (region-end) tab-width)
        )
    (indent-rigidly (line-beginning-position) (line-end-position) tab-width)
    )
  )

(defun decrease-indentation ()
  "Decrease indentation of region if mark is active"
  (interactive)
  (if mark-active
      (let ((deactivate-mark nil))
        (indent-rigidly (region-beginning) (region-end) (- tab-width))
        )
    (indent-rigidly (line-beginning-position) (line-end-position) (- tab-width))
    )
  )

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command)
)

(defun open-line-above ()
  (interactive)
  (move-beginning-of-line nil)
  (newline)
  (indent-for-tab-command)
  (forward-line -1)
  (indent-for-tab-command)
  )

(defun break-line ()
  (interactive)
  (newline)
  (save-excursion
    (newline)
    (indent-for-tab-command)
    )
  (indent-for-tab-command)
  )

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated."
  (interactive "p")
  (if (region-active-p)
      (duplicate-region arg)
    (duplicate-current-line arg))
  )

(defun duplicate-region (num &optional start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used."
  (interactive "p")
  (let* ((start (or start (region-beginning)))
         (end (or end (region-end)))
         (region (buffer-substring start end)))
    (goto-char start)
    (dotimes (i num)
      (insert region))))

(defun duplicate-current-line (arg)
  (interactive "p")
  (let (
        (c (current-column))
        )
    (progn
      (kill-ring-save
       (line-beginning-position)
       (line-end-position)
       )
      (end-of-line)
      (insert-char ?\n 1)
      (yank)
      (beginning-of-line)
      (forward-char c)
      )
    )
  )

(defun join-next-line ()
  (interactive)
  (join-line t)
)

(defun kill-or-join-next-line ()
  (interactive)
  (if (eolp)
      (join-next-line)
    (kill-line)
    )
  )

(provide 'defuns-editing)
