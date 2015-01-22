;; Functions for working with `package` and `config-packages`

(require 'cl)

(defun packages-installed-p ()
  "Check if all packages in `config-packages' are installed."
  (every #'package-installed-p config-packages))

(defun require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package config-packages)
    (add-to-list 'config-packages package)
    )
  (unless (package-installed-p package)
    (let ((replacement (cdr (assoc package config-replacements)))
          )
      (if replacement
          (if (string-match "https?://" replacement)
              (install-from-url replacement)
            (install-from-file replacement)
            )
        (progn
          (message "Installing package %s" package)
          (package-install package)
          )
        )
      )
    )
  )

(defun install-from-file (path)
  (message "Installing file '%s'" path)
  (package-install-file path)
  )

(defun install-from-url (url)
  (let ((path (make-temp-file "emacspkg" nil ".el"))
        )
    (message "Download '%s' to '%s'" url path)
    (with-current-buffer (url-retrieve-synchronously url)
      (write-region nil nil path nil)
      )
    (install-from-file path)
    (message "Deleting '%s'" path)
    (delete-file path)
    )
  )

(defun require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'require-package packages))

(defun install-packages ()
  "Install all packages listed in `config-packages'."
  (unless (packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Refreshing package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (require-packages config-packages)
    )
  )

(defun list-foreign-packages ()
  "Browse third-party packages not listed in `settings.el`.

Behaves similarly to `package-list-packages', but shows only the packages that
are installed and are not in `config-packages'.  Useful for
removing unwanted packages."
  (interactive)
  (package-show-package-list
   (set-difference package-activated-list config-packages)
   )
  )

(provide 'defuns-packages)
