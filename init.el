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
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete-1.3/dict")
(ac-config-default)
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

;; yasnippet
(add-to-list 'load-path "~/.emacs.d/yasnippet-0.6.1c")
(require 'yasnippet)
(setq yas/trigger-key (kbd "C-c <kp-multiply>"))
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")
(setq yas/indent-line 'none)

;; File type support

;; Python
;; Force python.el, not python-mode.el
(autoload 'python-mode "python" "python" t)
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

;; Tab-completion in IPython shells
(add-hook 'comint-mode-hook 
          '(lambda () 
             (local-set-key (kbd "<tab>") 'ipython-complete)))

;; Pdb
(defun better-pdb ()
  (gud-def gud-break  "break %d/%f:%l"  "\C-b" "Set breakpoint at current line.")
  (gud-def gud-remove "clear %d/%f:%l"  "\C-d" "Remove breakpoint at current line")
  (compilation-shell-minor-mode t)
  (local-set-key (kbd "<tab>") 'ipython-complete)
  (setq overlay-arrow-string "")
  )
(add-hook 'pdb-mode-hook 'better-pdb)
;; Highlight the current line when debugging
(defvar gud-overlay
  (let* ((ov (make-overlay (point-min) (point-min))))
    (overlay-put ov 'face 'secondary-selection)
    ov)
  "Overlay variable for GUD highlighting.")
(defadvice gud-display-line (after my-gud-highlight act)
  "Highlight current line."
  (let* ((ov gud-overlay)
         (bf (gud-find-file true-file)))
    (save-excursion
      (set-buffer bf)
      (move-overlay ov (line-beginning-position) (line-beginning-position 2)
                    (current-buffer)))))
(defun gud-kill-buffer ()
  (if (eq major-mode 'gud-mode)
      (delete-overlay gud-overlay)))
(add-hook 'kill-buffer-hook 'gud-kill-buffer)

;; Virtualenv
(autoload 'virtualenv-activate-environment "virtualenv" "virtualenv" t)
(setq virtualenv-root-dir "/home/alex/v/")
(defun workon-postactivate (virtualenv)
  (virtualenv-activate-environment virtualenv)
  (if (functionp 'rope-open-project) (rope-open-project (concat virtualenv "/rope"))))

;; Ropemacs
(autoload 'pymacs-load "pymacs")
(setq ropemacs-enable-autoimport t)
(defun python-hook ()
  (pymacs-load "ropemacs" "rope-")
  (ac-ropemacs-setup))
;  (local-set-key "\M-/" 'rope-code-assist))
(add-hook 'python-mode-hook 'python-hook)

;; Pyflakes
(setq python-check-command "pyflakes")

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

