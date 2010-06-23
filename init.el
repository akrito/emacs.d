;; Lisp setup
(require 'cl)
(add-to-list 'load-path "~/.emacs.d")

;; Keep customizations in a separate file
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file 'noerror)

;; Manual customizations
(ansi-color-for-comint-mode-on)
(blink-cursor-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)
(global-hl-line-mode t)
(server-start)
(set-fringe-mode 0)
(set-scroll-bar-mode nil)
(setq auto-save-list-file-prefix nil
      browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser"
      custom-raised-buttons nil      
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
      show-trailing-whitespace t
      speedbar-hide-button-brackets-flag t
      speedbar-indentation-width 2
      speedbar-show-unknown-files t
      speedbar-use-images nil
      starttls-use-gnutls t
      thing-at-point-file-name-chars "-~/[:alnum:]_.${}#%,"
      truncate-partial-width-windows nil
      vc-follow-symlinks t
      visible-bell t)
(setq-default fill-column 79
              cursor-type 'bar
              mode-line-format '("%e" (buffer-file-truename "%f" "%b") "-%*--%l--" mode-line-modes "%-")
              indent-tabs-mode nil)
              ;; show-trailing-whitespace t)
(show-paren-mode t)
(tool-bar-mode -1)

;; auto-pair parentheses
(require 'autopair)
(autopair-global-mode 1)
(put 'autopair-insert-opening 'delete-selection t)
(put 'autopair-skip-close-maybe 'delete-selection t)
(put 'autopair-insert-or-skip-quote 'delete-selection t)
(put 'autopair-extra-insert-opening 'delete-selection t)
(put 'autopair-extra-skip-close-maybe 'delete-selection t)
(put 'autopair-backspace 'delete-selection 'supersede)
(put 'autopair-newline 'delete-selection t)

;; Mouse stuff
;; On X11, change the pointer to an arrow, and remove the menu bar
(if (boundp 'x-pointer-arrow)
    (progn
      (setq-default x-pointer-shape x-pointer-arrow)
      ;; hack to force the pointer shape to change
      (set-mouse-color "black")
      (menu-bar-mode 1)))

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

;; Colors
(require 'color-theme)
(color-theme-initialize)
;; For a dark background
(set-face-background 'hl-line "#000000")
(load-file "~/.emacs.d/themes/zen-and-art.el")
(color-theme-zen-and-art)
;; For a light background
;;(set-face-background 'hl-line "#eeeeee")

;; Completion
;; Auto-complete
(add-to-list 'load-path "~/.emacs.d/auto-complete-1.3")
(require 'auto-complete-config)
(ac-config-default)
(defun ac-ropemacs-setup ()
  (ac-ropemacs-require)
  (setq ac-sources (append (list 'ac-source-ropemacs) ac-sources)))
(ac-ropemacs-initialize)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete-1.3/dict")
(add-to-list 'ac-modes 'yaml-mode)
(global-auto-complete-mode 1)
;; Hippie
(global-set-key (kbd "C-SPC") (make-hippie-expand-function
                             '(try-complete-file-name-partially
                               try-complete-file-name
                               try-expand-dabbrev) t))

;; New ergonomic bindings
;; http://xahlee.org/emacs/ergonomic_emacs_keybinding.html
(load "ergonomic_keybinding_qwerty")
;; I don't like all of them
(global-set-key (kbd "C-!") 'shell-command)
(global-set-key (kbd "C-|") 'shell-command-on-region)
(global-set-key (kbd "C-n") 'make-frame-command)
(global-set-key (kbd "C-b") 'switch-to-buffer)
(global-set-key (kbd "M-b") 'bookmark-jump)
(global-set-key (kbd "M-h") 'backward-char)
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-k") 'previous-line)
(global-set-key (kbd "M-j") 'next-line)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-m") 'magit-status)
(global-set-key (kbd "M-r") 'rgrep)
(global-set-key (kbd "M-t") 'revert-buffer)
(global-set-key
     "\M-a"
     (lambda ()
       (interactive)
       (call-interactively
        (intern
         (ido-completing-read
          "M-a "
          (all-completions "" obarray 'commandp))))))
(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map "\M-a" (lambda ()
       (interactive)
       (call-interactively
        (intern
         (ido-completing-read
          "M-a "
          (all-completions "" obarray 'commandp))))))))

;; better buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style   'forward
      uniquify-strip-common-suffix nil
      uniquify-separator           "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re   "^\\*")

;; ido.el - better buffer and filename completion
(autoload 'ido-mode "ido")
(ido-mode t)
(setq ido-max-directory-size 200000)

;; one-to-one windows
(setq pop-up-frames t)
(setq special-display-buffer-names
      '(
        ("*Completions*" (same-frame t))
        ("*Ido Completions*" (same-frame t))
        ("*anything*" (same-frame t))
        ("*BBDB*" (same-frame t))
        ("*Deletions*" (same-frame t))
        ("*Marked Processes*" (same-frame t))
        ("*vc-diff*" (same-frame t))
        ("*VC-log*" (same-frame t))
        ("*magit-log-edit*" (same-frame t))
        ("*magit-diff*" (same-frame t))
        ("*magit-commit*" (same-frame t))))

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

;; Python support - run before autocomplete
(add-to-list 'load-path "~/.emacs.d/python-den")
(require 'python-den)
(setq virtualenv-root-dir "~/v/") ;; remember the trailing slash
(workon-postactivate "/home/alex/v/ellington")

;; Haskell
(autoload 'haskell-mode "~/.emacs.d/haskell-mode/haskell-site-file" "Haskell mode" t)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

;; org mode
(setq org-agenda-files (list "~/work.org" "~/work.org_archive"))
(setq org-hide-leading-stars t)
(setq org-log-done t)
(setq org-agenda-skip-archived-trees nil)
(setq org-highlight-sparse-tree-matches nil)
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
(defun flymake-get-tex-args (file-name)
  (list "pdflatex"
        (list "-file-line-error" "-interaction=nonstopmode" file-name)))
(add-hook 'LaTeX-mode-hook 'flymake-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; Currently disabled for debugging

; ;; iedit
; (autoload 'iedit-mode "iedit" nil t)
; (define-key global-map (kbd "C-;") 'iedit-mode)
;
; ;; chrome support
; ; (if (locate-library "edit-server")
; ;    (progn
; ;      (require 'edit-server)
; ;      (setq edit-server-new-frame nil)
; ;      (edit-server-start)))
;
; ;; Varnish conf support
; (autoload 'vcl-mode "vcl-mode" "Edit Varnish VCL files" t)
; (add-to-list 'auto-mode-alist '("\\.vcl$" . vcl-mode))
;
; ;; Lua support
; (autoload 'lua-mode "lua-mode" "Edit Lua scripts" t)
; (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
;
; ;; Apache conf support
; (autoload 'apache-mode "apache-mode" "Edit Apache confs" t)

; ;; restructured text
; (autoload 'rst-mode "rst" "restructured text" t)

; ;; add a column of numbers
; (autoload 'sum-column "sum-column" "Sums a column" t)

; ;; clojure
; ;;(add-to-list 'load-path "~/opt/clojure-mode")
; ;;(require 'clojure-mode)
; ;;(add-to-list 'load-path "~/src/swank-clojure")
; ;;(setq swank-clojure-jar-path "~/src/clojure/clojure.jar")
; ;;      swank-clojure-extra-classpaths (list
; ;;				      "~/src/swank-clojure/src/swank"
; ;;				      "~/src/clojure-contrib/clojure-contrib.jar"))
; ;;(require 'swank-clojure-autoload)
; ;;(eval-after-load "slime"
; ;;  '(progn (slime-setup '(slime-repl))))
; ;;(add-to-list 'load-path "~/opt/slime")
; ;;(require 'slime)
; ;;(slime-setup)
; ;;(clojure-slime-config)

