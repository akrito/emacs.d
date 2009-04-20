;; Lisp setup
(require 'cl)
(add-to-list 'load-path "~/.emacs.d")

;; Keep customizations in a separate file
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file 'noerror)

;; Manual customizations
(fset 'yes-or-no-p 'y-or-n-p)
(menu-bar-mode nil)
(server-start)
(set-scroll-bar-mode nil)
(setq blink-cursor-mode nil)
(setq custom-raised-buttons nil)
(setq echo-keystrokes 0.01)
(setq inhibit-startup-screen t)
(setq make-backup-files nil)
(setq mode-line-inverse-video nil)
(setq mouse-autoselect-window t)
(setq mouse-wheel-progressive-speed nil)
(setq truncate-partial-width-windows nil)
(setq visible-bell t)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(show-paren-mode t)
(tool-bar-mode nil)
(transient-mark-mode t)

;; Rebind keys
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "C-b") 'switch-to-buffer)
(global-set-key (kbd "C-n") 'make-frame-command)
(global-set-key (kbd "C-w") 'kill-this-buffer)
(global-set-key (kbd "C-<return>") 'anything)

;; Hippie or dabbrev expansion
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

;; ido.el
(autoload 'ido-mode "ido")
(ido-mode t)

;; File type support

;; Apache conf support
(autoload 'apache-mode "apache-mode" "Edit Apache confs" t)

;; Haskell
(load "~/.emacs.d/haskell-mode/haskell-site-file")
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

