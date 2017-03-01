;; Selected settings defined as variables here

;; Set path to package configurations
(defvar config-config-dir
  (expand-file-name "config" user-emacs-directory)
  "This directory holds the configuration of emacs and its packages"
  )

;; Set path to my own code
(defvar config-defuns-dir
  (expand-file-name "defuns" user-emacs-directory)
  "This directory holds homebrewn code snippets and packages"
  )

;; Keep all the files that Emacs creates in this dir
(defvar config-savefiles-dir
  (expand-file-name "savefiles" user-emacs-directory)
  "This directory holds backup/savefiles/history/..."
  )

(defvar config-settings-file
  (expand-file-name "settings.el" user-emacs-directory)
  "This file holds the most common settings"
  )

;; Set path to ELPA replacements
(defvar config-replacements-dir
  (expand-file-name "replacements" user-emacs-directory)
  "This directory holds replacements for ELPA packages"
)

(defvar config-auto-save t
  "Non-nil values enable auto-save"
  )

(defvar config-fill-column 80
  "The fill column; lines longer than this are bad"
  )

(defvar config-default-major-mode 'text-mode
  ""
  )

(defvar config-auto-complete-key "M-SPC"
  ""
  )

(defvar config-diminish-flycheck nil
  ""
  )

(defvar config-preferred-extensions
  '(".py"
    ".c"
    ".tex"
    ".org"
    ".txt"
    ".sml"
    ".hs"
    ".emacs")
  "Extensions to list first when finding files"
)

(defvar config-ignored-extensions
  '(".hi"
    ".so"
    ".o"
    ".aux"
    ".snm"
    ".nav"
    ".toc"
    ".vrb"
    ".pyc"
    ".pyo"
    ".elc")
  "Extensions to ignore when finding files"
  )

(defvar config-show-whitespace t
  "Non-nil enabled whitespace visualization"
  )

(defvar config-whitespace-color "#a40000"
  "Non-nil enabled whitespace visualization"
  )

(defvar config-clean-whitespace-on-save t
  "Non-nil enables cleaning of whitespace from files before save"
  )

(defvar config-tab-always-indent 'complete
  ""
  )

(defvar config-flyspell t
  "Non-nil enables Flyspell support"
  )

(defvar config-theme 'tangotango
  "The deftheme to use"
  )

(defvar config-current-line-color "#242b2c"
  "The background color of the current line"
  )

(defvar config-bookmark-color "#ce5c00"
  "The background color of bookmarked lines"
  )

(defvar config-border-color "gray30"
  ""
  )

(defvar config-tab-width 2
  "The default tab width"
  )

(defvar config-packages
  '(
    ace-jump-buffer
    ace-jump-mode
    adaptive-wrap
    anaconda-mode
    anzu
    bm
    browse-kill-ring
    company
    company-anaconda
    dash
    diminish
    discover-my-major
    expand-region
    flx-ido
    flycheck
    git-timemachine
    gitconfig-mode
    gitignore-mode
    highlight-chars
    ido-ubiquitous
    markdown-mode
    minimap
    move-text
    multifiles
    multiple-cursors
    projectile
    php-mode
    rust-mode
    smart-mode-line
    smart-mode-line-powerline-theme
    smartparens
    smartrep
    smex
    tangotango-theme
    undo-tree
    whole-line-or-region
    zoom-frm
    )
  "A list of packages that should be installed"
  )

;; List of ELPA replacements
(defvar config-replacements
  '((minimap       . "https://github.com/br0ns/minimap/raw/master/minimap.el")
    (adaptive-wrap . "https://github.com/br0ns/adaptive-wrap/raw/master/adaptive-wrap.el")
    )
  "A list of replacements for `package` packages.  Will be installed
by `require-package`."
  )

(defvar config-recentf-exclude
  nil
  "List of filenames to exclude when finding recently visited files."
)

(defvar config-indent-sensitive-modes
  '(conf-mode
    asm-mode
    coffee-mode
    haml-mode
    python-mode
    slim-mode
    yaml-mode)
  "Modes for which auto-indenting is suppressed."
  )

(defvar config-yank-indent-modes
  '(LaTeX-mode
    TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here."
  )

(defvar config-yank-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur."
  )

(defvar config-prog-mode-hook-extra-hooks
  '(python-mode-hook
    )
  "Unfortunately not all modes derive from `prog-mode'.  Add additional mode
hooks to this list."
  )

(provide 'settings)
