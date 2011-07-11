(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/vendor")

;; Keep customizations in a separate file
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file 'noerror)

(load "keybindings")

;; Common settings
(ansi-color-for-comint-mode-on)
(blink-cursor-mode -1)
(delete-selection-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)
(server-start)
(set-fringe-mode 1)
(scroll-bar-mode -1)
(setq auto-save-list-file-prefix nil
      bookmark-save-flag 1
      browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "open"
      custom-raised-buttons nil
      focus-follows-mouse t
      frame-title-format '((buffer-file-truename "%f" "%b"))
      echo-keystrokes 0.01
      gist-view-gist t
      ido-work-directory-list-ignore-regexps '("^/s/")
      inhibit-startup-screen t
      kill-read-only-ok t
      make-backup-files nil
      mode-line-inverse-video t
      mouse-autoselect-window t
      mouse-drag-copy-region nil
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control)))
      org-agenda-files '("~/wiki/wiki.org_archive")
      ring-bell-function 'ignore
      show-trailing-whitespace t
      speedbar-hide-button-brackets-flag t
      speedbar-indentation-width 2
      speedbar-show-unknown-files t
      speedbar-use-images nil
      starttls-use-gnutls t
      thing-at-point-file-name-chars "-~/[:alnum:]_.${}#%,"
      truncate-partial-width-windows nil
      vc-follow-symlinks t
      visible-bell nil)
(setq-default fill-column 79
              cursor-type 'bar
              ispell-program-name "aspell"
              mode-line-format '("%e" (buffer-file-truename "%f" "%b") "-%*--%l--" mode-line-modes "%-")
              indent-tabs-mode nil)
(show-paren-mode t)
(tool-bar-mode -1)

;; Some Acme-style chords
;; (add-to-list 'load-path "~/.emacs.d/vendor/acme-mouse/")
;; (require 'acme-mouse)

;; Colors and pretty things
(add-to-list 'load-path "~/.emacs.d/vendor/color-theme/")
(require 'color-theme)
(color-theme-initialize)
(load-file "~/.emacs.d/vendor/solarized/emacs-color-theme-solarized/color-theme-solarized.el")
(color-theme-emacs-nw)

;; dark theme
(color-theme-solarized-dark)
(add-to-list 'initial-frame-alist '(cursor-color . "#748284"))
(add-to-list 'default-frame-alist '(cursor-color . "#748284"))

;; light theme
;; (color-theme-solarized-light)
;; (add-to-list 'initial-frame-alist '(cursor-color . "#748284"))
;; (add-to-list 'default-frame-alist '(cursor-color . "#748284"))

;; auto-pair parentheses
(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)
(put 'autopair-backspace 'delete-selection 'supersede)
;; but not in interactive shells
(defun comint-hook ()
  (setq autopair-dont-activate t))
(add-hook 'comint-mode-hook 'comint-hook)
(add-hook 'term-mode-hook 'comint-hook)

;; Better terminal
(require 'multi-term)
(setq multi-term-program "/Users/alex/bin/bash_login")
(setq term-unbind-key-list '("ESC" "C-f" "C-o" "C-t" "C-b" "C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>"))
(add-to-list 'term-unbind-key-list "C-b")
(add-to-list 'term-unbind-key-list "C-t")
(add-to-list 'term-unbind-key-list "C-o")
(add-to-list 'term-unbind-key-list "C-f")
(add-to-list 'term-unbind-key-list "ESC")
(add-to-list 'term-bind-key-alist '("M-v" . term-paste))
(add-to-list 'term-bind-key-alist '("M-ESC" . term-send-raw-meta))
(add-to-list 'term-bind-key-alist '("C-c" . term-send-raw))
(setq
 ;; term-default-bg-color "#191717"
 ;; term-default-fg-color "#D2DEC4"
 term-default-bg-color "#ffffff"
 term-default-fg-color "#000000"
 multi-term-dedicated-select-after-open-p t
 ;; For some reason, Ubuntu has terminfo entries for Eterm* but not eterm*
 term-term-name "Eterm-color")

;; Git
(add-to-list 'load-path "~/.emacs.d/vendor/magit")
(autoload 'gist-region "gist" "Gist" t)
(autoload 'gist-list "gist" "Gist" t)
(autoload 'gist-region-private "gist" "Gist" t)
(autoload 'gist-region-or-buffer "gist" "Gist" t)
(autoload 'gist-region-or-buffer-private "gist" "Gist" t)
(autoload 'magit-status "magit" nil t)
(setq magit-log-cutoff-length 1000)

;; Auto-complete
(add-to-list 'load-path "~/.emacs.d/vendor/auto-complete")
(require 'auto-complete-config)
(ac-config-default)
(ac-set-trigger-key "TAB")
(setq ac-auto-start nil)
(defun ac-ropemacs-setup ()
  (ac-ropemacs-require)
  (setq ac-sources (append (list 'ac-source-ropemacs) ac-sources)))
(ac-ropemacs-initialize)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete-1.3/dict")
(add-to-list 'ac-modes 'yaml-mode)
(global-auto-complete-mode 1)

;; better buffer names
(require 'uniquify)
(setq  uniquify-buffer-name-style   'forward
       uniquify-strip-common-suffix nil
       uniquify-separator           "/"
       uniquify-after-kill-buffer-p t
       uniquify-ignore-buffers-re   "^\\*")

;; ido.el - better buffer and filename completion
(autoload 'ido-mode "ido")
(ido-mode t)
(setq ido-max-directory-size 200000)

;; Midnight mode
(midnight-delay-set 'midnight-delay "12:00am")
(add-to-list 'clean-buffer-list-kill-regexps
                 "\\*magit.*\\*")

;; yasnippet - will only be used with autocomplete
(add-to-list 'load-path "~/.emacs.d/vendor/yasnippet-0.6.1c")
(require 'yasnippet)
;; assign to unused key, since we won't be using it
(setq yas/trigger-key (kbd "C-c <kp-multiply>"))
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")
(setq yas/indent-line 'none)
;; keep the minor mode off. We'll use autocomplete
(yas/global-mode -1)

;; textmate features
(add-to-list 'load-path "~/.emacs.d/vendor/textmate.el/")
(require 'textmate)
(textmate-mode)

;; File type support

;; Python support
(setenv "DJANGO_SETTINGS_MODULE" "edev")
;; Ropemacs
(require 'pymacs)
(setq ropemacs-enable-autoimport t)
(pymacs-load "ropemacs" "rope-")
(ac-ropemacs-setup)
(rope-open-project "/Users/alex/v/ellington/rope")
;; https://github.com/fgallina/python.el
(add-to-list 'load-path "/Users/alex/.emacs.d/python.el/")
(require 'python)
(setq python-shell-exec-path (list "/Users/alex/v/ellington/bin/")
      python-shell-interpreter "ipython"
      python-shell-interpreter-args ""
      python-shell-process-environment (list
                                        (format "PATH=%s" (mapconcat
                                                           'identity
                                                           (reverse
                                                            (cons (getenv "PATH")
                                                                  '("/Users/alex/v/ellington/bin/")))
                                                           ":"))
                                        "VIRTUAL_ENV=/Users/alex/v/ellington/")
      python-shell-prompt-regexp "In \\[[0-9]+\\]: "
      python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
      python-shell-completion-setup-code ""
      python-shell-completion-string-code "';'.join(__IP.complete('''%s'''))\n")
     
;; Haskell
(autoload 'haskell-mode "~/.emacs.d/haskell-mode/haskell-site-file" "Haskell mode" t)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

;; org mode
(setq org-agenda-files (list "~/org")
      org-hide-leading-stars t
      org-log-done t
      org-agenda-skip-archived-trees nil
      org-highlight-sparse-tree-matches nil)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-mobile-directory "~/org/mobile")
(setq org-directory "~/org")
(setq org-mobile-inbox-for-pull "~/org/inbox.org")

;; yaml
(autoload 'yaml-mode "yaml-mode" "YAML Ain't Markup Language" t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(defun yaml-hook ()
  (define-key yaml-mode-map "\C-m" 'newline-and-indent))
(add-hook 'yaml-mode-hook 'yaml-hook)

;; ruby
(setq ruby-indent-level 4)

;; markdown
(autoload 'markdown-mode "markdown-mode" "markdown" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; TeX
(defun tex-hook ()
  (require 'flymake)
  (defun flymake-get-tex-args (file-name)
    (list "pdflatex"
          (list "-file-line-error" "-interaction=nonstopmode" file-name)))
  (flymake-mode)
  (flyspell-mode) 
)
(add-hook 'LaTeX-mode-hook 'tex-hook)

;; Lua support
(autoload 'lua-mode "lua-mode" "Edit Lua scripts" t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

;; Scala
(add-to-list 'load-path "/usr/local//Cellar/scala/2.8.1/libexec/misc/scala-tool-support/emacs")
(load "scala-mode-auto.el")
;; Ensime
(add-to-list 'load-path "~/.emacs.d/vendor/ensime/elisp/")
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
;; MINI HOWTO: 
;; Open .scala file. M-x ensime (once per project)

;; Platform-specific overrides
(if (eq system-type 'darwin)
  (load "osx.el")
  (load "linux.el"))
