;;;; br0ns' emacs configuration

;; Based on Emacs Prelude

;; Enable backtrace when errors occur
;; (setq debug-on-error t)

;; Add `<user-emacs-directory>/settings' to load path so `settings.el' can be
;; found
(defvar config-settings-dir
  (expand-file-name "settings" user-emacs-directory)
  "This directory holds the configuration of emacs and its packages"
  )
(add-to-list 'load-path config-settings-dir)

;; Always load newest byte code
(setq load-prefer-newer t)

;; General settings are stored in
;; `<user-emacs-directory>/settings/settings.el'
(require 'settings)

;; Add to load path
(add-to-list 'load-path config-config-dir)
(add-to-list 'load-path config-defuns-dir)

;; Reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; Warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Keep changes made through Emacs' customize interface in
;; `<user-emacs-directory>/settings/custom.el'
(setq custom-file (expand-file-name "custom.el" config-settings-dir))

;; Initialize package manager
(package-initialize)

;; Global key bindings are stored in
;; `<user-emacs-directory>/global-key-bindings.el'
(require 'global-key-bindings)

;; Import the configuration
(require 'config-ui)
(require 'config-editor)
(require 'config-package)

(require 'config-nav)
(require 'config-whole-line-or-region)
(require 'config-smartparens)
(require 'config-flyspell)
(require 'config-bm)
(require 'config-ido)
(require 'config-hippie)
(require 'config-smart-mode-line)
(require 'config-minimap)
(require 'config-adaptive-wrap)
(require 'config-god-mode)

(require 'config-multiple-cursors)
(require 'config-company)

;; Modes
(require 'config-prog-mode)
(require 'config-python-mode)
(require 'config-haskell-mode)

;; Defuns
(require 'defuns-packages)
(require 'defuns-windows)
(require 'defuns-movement)
(require 'defuns-editing)
(require 'defuns-files)
(require 'defuns-misc)
(require 'evil-numbers)
(put 'scroll-left 'disabled nil)
