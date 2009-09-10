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

;; sudo editing of local files
;; http://nflath.com/2009/08/tramp/
(defun sudo-edit-current-file ()
  (interactive)
  (find-alternate-file (concat "/sudo:root@localhost:" (buffer-file-name (current-buffer)))))
(global-set-key (kbd "C-c C-r") 'sudo-edit-current-file)

;; On X11, change the pointer to an arrow
(if (boundp 'x-pointer-arrow)
    (progn
      (setq-default x-pointer-shape x-pointer-arrow)
      ;; hack to force the pointer shape to change
      (set-mouse-color "black")))

;; smtp mail
;; http://obfuscatedcode.wordpress.com/2007/04/26/configuring-emacs-for-gmails-smtp/
(setq user-mail-address "alex.kritikos@gmail.com")
(setq user-full-name "Alex Kritikos")
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

;; wanderlust
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;; IMAP
;;(setq elmo-imap4-default-server "imap.gmail.com")
;;(setq elmo-imap4-default-user "alex.kritikos@gmail.com")
;;(setq elmo-imap4-default-authenticate-type 'clear)
;;(setq elmo-imap4-default-port '993)
;;(setq elmo-imap4-default-stream-type 'ssl)
;;(setq elmo-imap4-use-modified-utf7 t)

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

;; bbdb
(setq bbdb-file "~/.emacs.d/bbdb")           ;; keep ~/ clean; set before loading
(require 'bbdb)
(bbdb-initialize)
(setq
    bbdb-offer-save 1                        ;; 1 means save-without-asking
    bbdb-use-pop-up nil
    bbdb-electric-p t                        ;; be disposable with SPC
    bbdb-popup-target-lines  1               ;; very small
    bbdb-dwim-net-address-allow-redundancy t ;; always use full name
    bbdb-quiet-about-name-mismatches 2       ;; show name-mismatches 2 secs
    bbdb-always-add-address t                ;; add new addresses to existing
                                             ;; contacts automatically
    bbdb-canonicalize-redundant-nets-p t     ;; x@foo.bar.cx => x@bar.cx
    bbdb-completion-type nil                 ;; complete on anything
    bbdb-complete-name-allow-cycling t       ;; cycle through matches
                                             ;; this only works partially
    bbbd-message-caching-enabled t           ;; be fast
    bbdb-use-alternate-names t               ;; use AKA
    bbdb-elided-display t                    ;; single-line addresses
    bbdb-completion-display-record nil       ;; don't open a window when completing

    ;; auto-create addresses from mail
    bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook
    bbdb-ignore-some-messages-alist ;; don't ask about fake addresses
    ;; NOTE: there can be only one entry per header (such as To, From)
    ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html
    '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter")))

;; github gists
(require 'gist)
(setq gist-view-gist t)

;; Highlight the current line
(global-hl-line-mode t)
(set-face-background 'hl-line "#eeeeee")


;; Rebind keys
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "C-b") 'switch-to-buffer)
(global-set-key (kbd "C-n") 'make-frame-command)
;; I'd like to use this, but it sometimes gets dead errors
;; (global-set-key (kbd "C-w") 'kill-this-buffer)
(global-set-key (kbd "C-w") 'ido-kill-buffer)
(global-set-key (kbd "C-<return>") 'anything)
(global-set-key (kbd "C-f") 'isearch-forward)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-!") 'shell-command)
(global-set-key (kbd "C-|") 'shell-command-on-region)

;; Hippie expansion
(global-set-key (kbd "C-SPC") (make-hippie-expand-function
                             '(try-complete-file-name-partially
                               try-complete-file-name
                               try-expand-dabbrev) t))

;; add a column of numbers
(defun sum-column()
  "Sums a column of numbers starting at point"
  (interactive)
  (save-excursion
    (if (and (not (= (current-column) 0))
	     (re-search-backward "[ \t]" 0 t ))
	(forward-char))
    (let ((retn 0)
	  (old-column (current-column))
	  (old-next-line-add-newlines))
      (setq next-line-add-newlines nil)
      (while (not
	      (looking-at "^[ \t]*$"))
	(move-to-column old-column t)
	(if (and (looking-at "-?[0123456789]+")
		 (eq (current-column) old-column))
		(setq retn (+ retn (string-to-number (current-word)))))
	(next-line)
	(beginning-of-line))
      (next-line)
      (next-line)
      (move-end-of-line 0)
      (insert (make-string (- old-column (current-column)) 32))
      (insert (number-to-string retn))
      (setq next-line-add-newlines old-next-line-add-newlines)
      retn)))

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

;; one-to-one windows
(setq pop-up-frames t)
(setq special-display-buffer-names
      '(
        ("*Completions*" (same-frame t))
        ("*Ido Completions*" (same-frame t))
        ("*anything*" (same-frame t))))

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
(setq org-hide-leading-stars t)
(setq org-log-done t)
(setq org-agenda-skip-archived-trees nil)
(setq org-highlight-sparse-tree-matches nil)
(global-set-key "\C-ca" 'org-agenda)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
;; persist the "anything" keybinding in org-mode
(defun org-mode-setup ()
  (define-key org-mode-map (kbd "C-<return>") 'anything)
  (define-key org-mode-map (kbd "C-M-<return>") 'org-meta-return))
(add-hook 'org-mode-hook 'org-mode-setup)

;; restructured text
(autoload 'rst-mode "rst" "restructured text" t)

;; yaml
(autoload 'yaml-mode "yaml-mode" "YAML Ain't Markup Language" t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; python + pyflakes
(load "flymake" t)
(defun flymake-pyflakes-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "pyflakes" (list local-file))))
(add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-pyflakes-init))
(add-hook 'find-file-hook 'flymake-find-file-hook)
(add-hook 'python-mode-hook
          '(lambda () (eldoc-mode 1)) t)

;; ruby
(setq ruby-indent-level 4)

;; markdown
(autoload 'markdown-mode "markdown-mode" "markdown" t)

;; javascript
;; TODO remove the mouse bindings that conflict with acme-mouse.el
