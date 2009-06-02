;; Lisp setup
(require 'cl)
(add-to-list 'load-path "~/.emacs.d")

;; Keep customizations in a separate file
(setq custom-file "emacs-custom.el")
(load custom-file 'noerror)

;; Manual customizations
(blink-cursor-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)
(menu-bar-mode nil)
(server-start)
(set-scroll-bar-mode nil)
(setq custom-raised-buttons nil)
(setq echo-keystrokes 0.01)
(setq inhibit-startup-screen t)
(setq fill-column 79)
(setq kill-read-only-ok t)
(setq make-backup-files nil)
(setq mode-line-inverse-video nil)
(setq mouse-autoselect-window t)
(setq mouse-wheel-progressive-speed nil)
(setq starttls-use-gnutls t)
(setq truncate-partial-width-windows nil)
(setq visible-bell t)
(setq vc-follow-symlinks t)
(setq-default cursor-type 'bar)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(show-paren-mode t)
(tool-bar-mode nil)

;; On X11, change the pointer to an arrow
(if (boundp 'x-pointer-arrow)
    (progn
      (setq-default x-pointer-shape x-pointer-arrow)
      ;; hack to force the pointer shape to change
      (set-mouse-color "black")))

;; smtp mail
;; http://obfuscatedcode.wordpress.com/2007/04/26/configuring-emacs-for-gmails-smtp/
(setq user-mail-address "alex.kritikos@gmail.com")
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      (expand-file-name "~/.authinfo")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)
(autoload 'compose-mail "smtpmail")
(load "abbrevs" 'noerror)
(add-hook 'mail-mode-hook 'mail-abbrevs-setup)

;; github gists
(require 'gist)
(setq gist-view-gist t)

;; Highlight the current line
(global-hl-line-mode t)
(set-face-background 'hl-line "#fff")

;; Rebind keys
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "C-b") 'switch-to-buffer)
(global-set-key (kbd "C-n") 'make-frame-command)
(global-set-key (kbd "C-w") 'kill-this-buffer)
(global-set-key (kbd "C-<return>") 'anything)
(global-set-key (kbd "C-f") 'isearch-forward)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)

;; Hippie expansion
(global-set-key (kbd "C-SPC") (make-hippie-expand-function
                             '(try-complete-file-name-partially
                               try-complete-file-name
                               try-expand-dabbrev) t))

;; better buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-strip-common-suffix nil)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; anything.el - Quicksilver for Emacs
(require 'anything)
(require 'anything-config)
(setq anything-sources
      (list anything-c-source-buffers
            anything-c-source-file-name-history
            anything-c-source-emacs-commands
            anything-c-source-locate))

;; ido.el - better buffer and filename completion
(autoload 'ido-mode "ido")
(ido-mode t)

;; Some Acme-style chords
(require 'acme-mouse)

;; File type support

;; Varnish conf support
(autoload 'vcl-mode "vcl-mode" "Edit Varnish VCL files" t)
(add-to-list 'auto-mode-alist '("\\.vcl$" . vcl-mode))

(autoload 'lua-mode "lua-mode" "Edit Lua scripts" t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

;; Apache conf support
(autoload 'apache-mode "apache-mode" "Edit Apache confs" t)

;; Haskell
(autoload 'haskell-mode "~/.emacs.d/haskell-mode/haskell-site-file" "Haskell mode" t)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

;; org mode
(setq org-agenda-files (list "~/wiki/Work.org" "~/wiki/Work.org_archive"))
(setq org-log-done t)
(setq org-agenda-skip-archived-trees nil)
(setq org-highlight-sparse-tree-matches nil)
(global-set-key "\C-ca" 'org-agenda)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
;; persist the "anything" keybinding in org-mode
(defun org-mode-setup ()
  (define-key org-mode-map (kbd "C-<return>") 'anything))
(add-hook 'org-mode-hook 'org-mode-setup)

;; restructured text
(autoload 'rst-mode "rst" "restructured text" t)

;; yaml
(autoload 'yaml-mode "yaml-mode" "YAML Ain't Markup Language" t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
