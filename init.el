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

; ;; On X11, change the pointer to an arrow
; (if (boundp 'x-pointer-arrow)
;     (progn
;       (setq-default x-pointer-shape x-pointer-arrow)
;       ;; hack to force the pointer shape to change
;       (set-mouse-color "black")))

;; github gists
(autoload 'gist-region "gist" "Gist" t)
(autoload 'gist-list "gist" "Gist" t)
(autoload 'gist-region-private "gist" "Gist" t)
(autoload 'gist-region-or-buffer "gist" "Gist" t)
(autoload 'gist-region-or-buffer-private "gist" "Gist" t)

;; magit
(autoload 'magit-status "magit" nil t)

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
; ;; Highlight the current line
; (global-hl-line-mode t)
; ;; For a dark background
; ;;(set-face-background 'hl-line "#333333")
; ;; For a light background
; (set-face-background 'hl-line "#eeeeee")
; 
; ;; Colors
(require 'color-theme)
(color-theme-initialize)
(load-file "~/.emacs.d/themes/zen-and-art.el")
(color-theme-zen-and-art)
(color-theme-gtk-ide)
(color-theme-dark-laptop)
;;(color-theme-aalto-light)

;; Rebind keys
(global-set-key (kbd "C-!") 'shell-command)
(global-set-key (kbd "C-|") 'shell-command-on-region)
;; Hippie expansion
(global-set-key (kbd "C-SPC") (make-hippie-expand-function
                             '(try-complete-file-name-partially
                               try-complete-file-name
                               try-expand-dabbrev) t))

;; New ergonomic bindings
;; http://xahlee.org/emacs/ergonomic_emacs_keybinding.html
(load "ergonomic_keybinding_qwerty")
;; I don't like all of them
(global-set-key (kbd "C-n") 'make-frame-command)
(global-set-key (kbd "C-b") 'switch-to-buffer)
(global-set-key (kbd "M-h") 'backward-char)
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-k") 'previous-line)
(global-set-key (kbd "M-j") 'next-line)
(global-set-key (kbd "M-g") 'goto-line)
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

; ;; add a column of numbers
; (autoload 'sum-column "sum-column" "Sums a column" t)

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
;(setq ido-enable-tramp-completion nil)

; ;; one-to-one windows
; (setq pop-up-frames t)
; (setq special-display-buffer-names
;       '(
;         ("*Completions*" (same-frame t))
;         ("*Ido Completions*" (same-frame t))
;         ("*anything*" (same-frame t))
;         ("*BBDB*" (same-frame t))
;         ("*Deletions*" (same-frame t))
;         ("*Marked Processes*" (same-frame t))
;         ("*vc-diff*" (same-frame t))
;         ("*VC-log*" (same-frame t))
;         ("*magit-log-edit*" (same-frame t))
;         ("*magit-diff*" (same-frame t))
;         ("*magit-commit*" (same-frame t))))
 
 ;; Some Acme-style chords
 (require 'acme-mouse)
 
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
; 
; ;; Midnight mode
; (midnight-delay-set 'midnight-delay "12:00am")
; (add-to-list 'clean-buffer-list-kill-regexps
;                  "\\*magit.*\\*")
; 
; ;; File type support
; 
; ;; Better Python support
; ;;(require 'pymacs)
; ;;(pymacs-load "ropemacs" "rope-")
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

;; Haskell
(autoload 'haskell-mode "~/.emacs.d/haskell-mode/haskell-site-file" "Haskell mode" t)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

; ;; org mode
; (setq org-agenda-files (list "~/work.org" "~/work.org_archive"))
; (setq org-hide-leading-stars t)
; (setq org-log-done t)
; (setq org-agenda-skip-archived-trees nil)
; (setq org-highlight-sparse-tree-matches nil)
; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
; 
; ;; restructured text
; (autoload 'rst-mode "rst" "restructured text" t)

;; yaml
(autoload 'yaml-mode "yaml-mode" "YAML Ain't Markup Language" t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; ruby
(setq ruby-indent-level 4)

; ;; markdown
; (autoload 'markdown-mode "markdown-mode" "markdown" t)
