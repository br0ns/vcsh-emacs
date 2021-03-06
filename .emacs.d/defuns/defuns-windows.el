;;; Window related defuns
(defun defuns-windows-rotate-windows (count)
  "Rotate your windows"
  (interactive "p")
  (let* ((ws (window-list))
         (bs (mapcar 'window-buffer ws))
         (ss (mapcar 'window-start ws))
         (i 0)
         (n (count-windows))
         )
    (while (< i n)
      (let* ((w (elt ws i))
             (j (mod (+ i count) n))
             (b (elt bs j))
             (s (elt ss j))
             )
        (set-window-buffer w b)
        (set-window-start w s)
        )
      (setq i (+ i 1))
      )
    )
  )

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(provide 'defuns-windows)
