(require 'cl)

;; Keep customizations in a separate file
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file 'noerror)

;; Manual customizations
(server-start)
(add-to-list 'load-path "~/.emacs.d")
(setq blink-cursor-mode nil)
(setq truncate-partial-width-windows nil)
(setq-default indent-tabs-mode nil)
(setq mouse-autoselect-window t)
(tool-bar-mode nil)
(set-scroll-bar-mode nil)
(setq show-paren-mode t)
(menu-bar-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)

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
;;(global-set-key (kbd "C-SPC") 'dabbrev-expand)


;; Look like Acme (useful with Wmii and plumbing)
;;(setq frame-title-format
;;      '(buffer-file-truename "%f" "%b"))

;; line numbers
;;(set-variable 'linum-format "%3d " nil)
;;(set-variable 'column-number-mode nil nil)
;;(set-variable 'line-number-mode nil nil)

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

;; color themes
;;(require 'color-theme)
;;(color-theme-initialize)
;;(color-theme-charcoal-black)

;; vcs stuff
;;(add-to-list 'vc-handled-backends 'SVN)
;;(add-to-list 'vc-handled-backends 'HG)
;;(add-to-list 'vc-handled-backends 'GIT)
;;(set-variable 'vc-follow-symlinks t)

;; File type support

;; Apache conf support
(autoload 'apache-mode "apache-mode" "Edit Apache confs" t)

;; Ruby support
;;(setq ri-ruby-script "/home/alex/.emacs.d/ri-emacs.rb")
;;(autoload 'ri "/home/alex/.emacs.d/ri-ruby.el" nil t)
;;(autoload 'ruby-mode "ruby-mode" "Edit Ruby files" t)
;;(autoload 'rcodetools "rcodetools" nil t)
;;(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
;;(setq ruby-indent-level 4)
;;(setq ruby-deep-indent-paren nil)

;; Haskell
(load "~/.emacs.d/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

;; org mode
(setq org-agenda-files (list "~/wiki/Work.org" "~/wiki/Work.org_archive"))
(setq org-log-done t)
(setq org-agenda-skip-archived-trees nil)
(global-set-key "\C-ca" 'org-agenda)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
;; persist the "anything" keybinding in org-mode
(defun org-mode-setup ()
  (define-key org-mode-map (kbd "C-<return>") 'anything))
(add-hook 'org-mode-hook 'org-mode-setup)

;; sup
;;(add-to-list 'auto-mode-alist '("/tmp/sup.*" . post-mode))

;; restructured text
(autoload 'rst-mode "rst" "restructured text" t)

;; yaml
(autoload 'yaml-mode "yaml-mode" "YAML Ain't Markup Language" t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

