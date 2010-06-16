;; Lisp setup
(require 'cl)
(add-to-list 'load-path "~/.emacs.d")

;; Keep customizations in a separate file
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file 'noerror)

;; Manual customizations
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)
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
(tool-bar-mode -1)
(blink-cursor-mode -1)

;; On X11, change the pointer to an arrow, and remove the menu bar
(if (boundp 'x-pointer-arrow)
    (progn
      (setq-default x-pointer-shape x-pointer-arrow)
      ;; hack to force the pointer shape to change
      (set-mouse-color "black")
      (menu-bar-mode 1)))

(setq auto-save-list-file-prefix nil)

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
(global-hl-line-mode t)
(setq show-trailing-whitespace t)
;; For a dark background
(set-face-background 'hl-line "#000000")
;; For a light background
;;(set-face-background 'hl-line "#eeeeee")

;; Colors
(require 'color-theme)
(color-theme-initialize)
(load-file "~/.emacs.d/themes/zen-and-art.el")
(color-theme-zen-and-art)
;; (color-theme-late-night)
;; (color-theme-charcoal-black)
;; (color-theme-gtk-ide)
;; (color-theme-dark-laptop)
;; (color-theme-aalto-light)

;; Rebind keys
(global-set-key (kbd "C-!") 'shell-command)
(global-set-key (kbd "C-|") 'shell-command-on-region)
;; Completion
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
(setq ido-max-directory-size 200000)
;(setq ido-enable-tramp-completion nil)

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

;; Midnight mode
(midnight-delay-set 'midnight-delay "12:00am")
(add-to-list 'clean-buffer-list-kill-regexps
                 "\\*magit.*\\*")

;; yasnippet
(add-to-list 'load-path "~/.emacs.d/yasnippet-0.6.1c")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")

;; File type support

;; Better Python support
;; This is on by default in Emacs 23.2, I hear
(ansi-color-for-comint-mode-on) 
;; __IPYTHON__ will return the interpreter in both ipython and ipdb. __IP only
;; works for ipython
(setq ipython-completion-command-string "print(';'.join(__IPYTHON__.Completer.all_completions('%s')))\n")
(defun ipython-complete ()
    "Try to complete the python symbol before point. Only knows about the stuff
in the current *Python* session."
    (interactive)
    (let* ((ugly-return nil)
           (sep ";")
           (python-process (or (get-buffer-process (current-buffer))
                                        ;XXX hack for .py buffers
                               (get-process py-which-bufname)))
           ;; XXX currently we go backwards to find the beginning of an
           ;; expression part; a more powerful approach in the future might be
           ;; to let ipython have the complete line, so that context can be used
           ;; to do things like filename completion etc.
           (beg (save-excursion (skip-chars-backward "a-z0-9A-Z_./" (point-at-bol))
                                (point)))
           (end (point))
           (pattern (buffer-substring-no-properties beg end))
           (completions nil)
           (completion-table nil)
           completion
         (comint-preoutput-filter-functions
          (append comint-preoutput-filter-functions
                  '(ansi-color-filter-apply
                    (lambda (string)
                      (setq ugly-return (concat ugly-return string))
                      "")))))
      (process-send-string python-process
                            (format ipython-completion-command-string pattern))
      (accept-process-output python-process)
      (setq completions
            (split-string (substring ugly-return 0 (position ?\n ugly-return)) sep))
      (setq completion-table (loop for str in completions
                                   collect (list str nil)))
      (setq completion (try-completion pattern completion-table))
      (cond ((eq completion t))
            ((null completion)
             (message "Can't find completion for \"%s\"" pattern)
             (ding))
            ((not (string= pattern completion))
             (delete-region beg end)
             (insert completion))
            (t
             (setq completion
                   (ido-completing-read pattern 
                                        (all-completions pattern completion-table)))
             (delete-region beg end)
             (insert completion)))))

(defun better-pdb-breakpoints ()
  (gud-def gud-break  "break %d/%f:%l"  "\C-b" "Set breakpoint at current line.")
  (gud-def gud-remove "clear %d/%f:%l"  "\C-d" "Remove breakpoint at current line")
  )
(add-hook 'pdb-mode-hook 'better-pdb-breakpoints)
;; (defun ipython-shell-hook ()
;;       ;; the following is to synchronize dir-changes
;;       (make-local-variable 'shell-dirstack)
;;       (setq shell-dirstack nil)
;;       (make-local-variable 'shell-last-dir)
;;       (setq shell-last-dir nil)
;;       (make-local-variable 'shell-dirtrackp)
;;       (setq shell-dirtrackp t)
;;       (add-hook 'comint-input-filter-functions 'shell-directory-tracker nil t)

;;       (ansi-color-for-comint-mode-on)
;;       (define-key python-shell-map [tab] 'ipython-complete)
;;       ;; Add this so that tab-completion works both in X11 frames and inside
;;       ;; terminals (such as when emacs is called with -nw).
;;       (define-key python-shell-map "\t" 'ipython-complete)

;; (add-hook 'python-shell-hook 'ipython-shell-hook)
(require 'virtualenv)
(set-variable 'virtualenv-root-dir "/home/alex/v/")
(defun workon-postactivate (virtualenv)
  (require 'virtualenv)
  (virtualenv-activate-environment virtualenv))

(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

(add-to-list 'load-path "~/.emacs.d/auto-complete-1.3")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete-1.3/dict")
(ac-config-default)
(global-auto-complete-mode 1)

(setq python-check-command "pyflakes")

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

;; org mode
(setq org-agenda-files (list "~/work.org" "~/work.org_archive"))
(setq org-hide-leading-stars t)
(setq org-log-done t)
(setq org-agenda-skip-archived-trees nil)
(setq org-highlight-sparse-tree-matches nil)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

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

;; markdown
(autoload 'markdown-mode "markdown-mode" "markdown" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; TeX
(defun flymake-get-tex-args (file-name)
(list "pdflatex"
(list "-file-line-error" "-interaction=nonstopmode" file-name)))
 
(add-hook 'LaTeX-mode-hook 'flymake-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
