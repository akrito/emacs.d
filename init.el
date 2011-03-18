;; TODO
;; make a wrapper for "correct-ipdb correct-django-admin.py"
;; Fix relative imports in python-shell
;; in multi-term, send ^C

;; Lisp setup
(require 'cl)
(add-to-list 'load-path "~/.emacs.d")

;; Keep customizations in a separate file
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file 'noerror)

(load "keybindings")

;; Manual customizations
(ansi-color-for-comint-mode-on)
(blink-cursor-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)
(global-hl-line-mode 1)
(server-start)
(set-fringe-mode 1)
(scroll-bar-mode -1)
(setq auto-save-list-file-prefix nil
      bookmark-save-flag 1
      browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser"
      custom-raised-buttons nil
      focus-follows-mouse t
      frame-title-format '((buffer-file-truename "%f" "%b"))
      echo-keystrokes 0.01
      gist-view-gist t
      ido-work-directory-list-ignore-regexps '("^/s/")
      inhibit-startup-screen t
      kill-read-only-ok t
      make-backup-files nil
      mode-line-inverse-video nil
      mouse-autoselect-window t
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
(add-to-list 'term-unbind-key-list "C-b")
(add-to-list 'term-unbind-key-list "C-t")
(add-to-list 'term-unbind-key-list "C-o")
(add-to-list 'term-unbind-key-list "C-f")
(add-to-list 'term-unbind-key-list "ESC")
(add-to-list 'term-bind-key-alist '("M-v" . term-paste))
(add-to-list 'term-bind-key-alist '("M-ESC" . term-send-raw-meta)) 
(setq
 ;; term-default-bg-color "#191717"
 ;; term-default-fg-color "#D2DEC4"
 term-default-bg-color "#ffffff"
 term-default-fg-color "#000000"
 multi-term-dedicated-select-after-open-p t
 ;; For some reason, Ubuntu has terminfo entries for Eterm* but not eterm*
 term-term-name "Eterm-color")

;; GUI stuff

;; On X11, change the pointer to an arrow, and remove the menu bar
(if (boundp 'x-pointer-arrow)
    (progn
      (setq-default x-pointer-shape x-pointer-arrow)
      ;; hack to force the pointer shape to change
      (set-mouse-color "black")
      (menu-bar-mode 0)))

;; Mac-specific, but shouldn't hurt Linux
(setq mac-command-modifier (quote meta))
(setq mac-option-modifier (quote alt))

;; Some Acme-style chords
(require 'acme-mouse)

;; Git
(autoload 'gist-region "gist" "Gist" t)
(autoload 'gist-list "gist" "Gist" t)
(autoload 'gist-region-private "gist" "Gist" t)
(autoload 'gist-region-or-buffer "gist" "Gist" t)
(autoload 'gist-region-or-buffer-private "gist" "Gist" t)
(autoload 'magit-status "magit" nil t)
(setq magit-log-cutoff-length 1000)

;; Colors and pretty things
(require 'color-theme)
(color-theme-initialize)
;; For a dark background
;; (set-face-background 'hl-line "#000000")
;; (load-file "~/.emacs.d/themes/zen-and-art.el")
;; (color-theme-zen-and-art)
;; For a light background
(set-face-background 'hl-line "#eeeeee")
(color-theme-emacs-nw)
;; (autoload 'global-pretty-mode "pretty-mode" "Pretty mode" t)

;; Auto-complete
(add-to-list 'load-path "~/.emacs.d/auto-complete-1.3")
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
;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

;; Midnight mode
(midnight-delay-set 'midnight-delay "12:00am")
(add-to-list 'clean-buffer-list-kill-regexps
                 "\\*magit.*\\*")

;; yasnippet - will only be used with autocomplete
(add-to-list 'load-path "~/.emacs.d/yasnippet-0.6.1c")
(require 'yasnippet)
;; assign to unused key, since we won't be using it
(setq yas/trigger-key (kbd "C-c <kp-multiply>"))
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")
(setq yas/indent-line 'none)
;; keep the minor mode off. We'll use autocomplete
(yas/global-mode -1)

;; File type support

;; Python support
(setenv "DJANGO_SETTINGS_MODULE" "edev")
(setq python-den-root-dir "~/.emacs.d/python-den")
(setq virtualenv-root-dir "~/v/") ;; remember the trailing slash
(add-to-list 'load-path python-den-root-dir)
(require 'python-den)
;; Use the default virtualenv
(workon-postactivate "~/v/ellington")
(add-hook 'python-mode-hook 'imenu-add-menubar-index)
;; (setq python-mode-hook nil)

;; Haskell
(autoload 'haskell-mode "~/.emacs.d/haskell-mode/haskell-site-file" "Haskell mode" t)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

;; org mode
(setq org-agenda-files (list "~/work.org" "~/work.org_archive")
      org-hide-leading-stars t
      org-log-done t
      org-agenda-skip-archived-trees nil
      org-highlight-sparse-tree-matches nil)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

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

;; Varnish conf support
(autoload 'vcl-mode "vcl-mode" "Edit Varnish VCL files" t)
(add-to-list 'auto-mode-alist '("\\.vcl$" . vcl-mode))

;; Lua support
(autoload 'lua-mode "lua-mode" "Edit Lua scripts" t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

;; ;; clojure-mode
;; (add-to-list 'load-path "~/opt/clojure-mode")
;; (require 'clojure-mode)

;; ;; slime
;; (eval-after-load "slime" 
;;   '(progn (slime-setup '(slime-repl))))

;; (add-to-list 'load-path "~/opt/slime")
;; (require 'slime)
;; (slime-setup)
