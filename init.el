;; Lisp setup
(require 'cl)
(add-to-list 'load-path "~/.emacs.d")
;; (add-to-list 'load-path "~/.emacs.d/company")

;; Keep customizations in a separate file
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file 'noerror)

;; Manual customizations
(blink-cursor-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)
(menu-bar-mode nil)
(server-start)
(set-scroll-bar-mode nil)
(setq custom-raised-buttons nil
      echo-keystrokes 0.01
      gist-view-gist t
      inhibit-startup-screen t
      kill-read-only-ok t
      make-backup-files nil
      mode-line-inverse-video nil
      mouse-autoselect-window t
      mouse-wheel-progressive-speed nil
      starttls-use-gnutls t
      truncate-partial-width-windows nil
      visible-bell t
      vc-follow-symlinks t
      thing-at-point-file-name-chars "-~/[:alnum:]_.${}#%,")
(setq-default fill-column 79
              cursor-type 'bar
              indent-tabs-mode nil
              show-trailing-whitespace t)
(show-paren-mode t)
(tool-bar-mode nil)

;; sudo editing of local files
;; http://nflath.com/2009/08/tramp/
;; (defun sudo-edit-current-file ()
;;   (interactive)
;;   (find-alternate-file (concat "/sudo:root@localhost:" (buffer-file-name (current-buffer)))))

;; On X11, change the pointer to an arrow
(if (boundp 'x-pointer-arrow)
    (progn
      (setq-default x-pointer-shape x-pointer-arrow)
      ;; hack to force the pointer shape to change
      (set-mouse-color "black")))

;; wanderlust
;; TODO remove conflicting C-O binding
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl "mymail" "Wanderlust config" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
(autoload 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

;; github gists
(autoload 'gist-region "gist" "Gist" t)
(autoload 'gist-region-private "gist" "Gist" t)
(autoload 'gist-region-or-buffer "gist" "Gist" t)
(autoload 'gist-region-or-buffer-private "gist" "Gist" t)

;; Highlight the current line
(global-hl-line-mode t)
(set-face-background 'hl-line "#333333")

;; Colors
(require 'color-theme)
(color-theme-initialize)
(color-theme-arjen)

;; Rebind keys
(global-set-key (kbd "C-!") 'shell-command)
(global-set-key (kbd "C-|") 'shell-command-on-region)
(global-set-key (kbd "C-c C-r") 'sudo-edit-current-file)
;; Hippie expansion
(global-set-key (kbd "C-SPC") (make-hippie-expand-function
                             '(try-complete-file-name-partially
                               try-complete-file-name
                               try-expand-dabbrev) t))
;; Company expansion
;; (autoload 'company-mode "company" nil t)
;; (autoload 'global-company-mode "company" nil t)
;; (global-company-mode)

;; (eval-after-load "company"
;;   '(progn
;;      (setq company-backends '(company-dabbrev-code))))

;; New ergonomic bindings
;;://xahlee.org/emacs/ergonomic_emacs_keybinding.html
(load "ergonomic_keybinding_qwerty")
;; I don't like all of them
(global-set-key (kbd "C-n") 'make-frame-command)
(global-set-key (kbd "C-b") 'switch-to-buffer)
(global-set-key (kbd "M-h") 'backward-char)
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-k") 'previous-line)
(global-set-key (kbd "M-j") 'next-line)
(global-set-key
     "\M-a"
     (lambda ()
       (interactive)
       (call-interactively
        (intern
         (ido-completing-read
          "M-a "
          (all-completions "" obarray 'commandp))))))

;; add a column of numbers
(autoload 'sum-column "sum-column" "Sums a column" t)

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
(setq ido-enable-tramp-completion nil)

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
        ("*VC-log*" (same-frame t))))

;; Some Acme-style chords
(require 'acme-mouse)

;; File type support

;; Varnish conf support
(autoload 'vcl-mode "vcl-mode" "Edit Varnish VCL files" t)
(add-to-list 'auto-mode-alist '("\\.vcl$" . vcl-mode))

;; Lua support
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
(setq org-agenda-files (list "~/work.org" "~/work.org_archive"))
(setq org-hide-leading-stars t)
(setq org-log-done t)
(setq org-agenda-skip-archived-trees nil)
(setq org-highlight-sparse-tree-matches nil)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; restructured text
(autoload 'rst-mode "rst" "restructured text" t)

;; yaml
(autoload 'yaml-mode "yaml-mode" "YAML Ain't Markup Language" t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; python
;; (load "flymake" t)
;; (defun flymake-pyflakes-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                      'flymake-create-temp-inplace))
;;          (local-file (file-relative-name
;;                       temp-file
;;                       (file-name-directory buffer-file-name))))
;;     (list "pyflakes" (list local-file))))
;; (add-to-list 'flymake-allowed-file-name-masks
;;              '("\\.py\\'" flymake-pyflakes-init))
;; (add-hook 'find-file-hook 'flymake-find-file-hook)
;; (add-hook 'python-mode-hook
;;           '(lambda () (eldoc-mode 1)) t)
;;(require 'pysmell)
 ;;(require 'pymacs)
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
 ;;(pymacs-load "ropemacs" "rope-")

;; ruby
(setq ruby-indent-level 4)

;; markdown
(autoload 'markdown-mode "markdown-mode" "markdown" t)

;; javascript
;; TODO remove the mouse bindings that conflict with acme-mouse.el
