(progn
  (add-to-list 'load-path user-emacs-directory)
  (require 'settings)
  (add-to-list 'load-path config-defuns-dir)
  (add-to-list 'load-path config-config-dir)
  (package-initialize)
  (require 'config-package)
  (require 'defuns-packages)

  (install-packages)

  (byte-recompile-directory user-emacs-directory 0 t)
  )
