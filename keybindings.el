;; Unset annoying bindings
(global-unset-key (kbd "C-p")) ; Print

;; Bindings
(global-set-key (kbd "C-|") 'shell-command-on-region)
(global-set-key (kbd "M-'") 'comment-dwim)
(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "C-b") 'switch-to-buffer)
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "C-d") 'delete-char)
(global-set-key (kbd "M-f") 'isearch-forward)
(global-set-key (kbd "M-F") 'rgrep)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-m") 'magit-status)
(global-set-key (kbd "M-p") 'ido-find-file-in-tag-files)
(global-set-key (kbd "M-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "M-r") 'ido-goto-symbol)
(global-set-key (kbd "C-s") 'other-frame)
(global-set-key (kbd "M-t") 'revert-buffer)
(global-set-key (kbd "C-t") 'multi-term-dedicated-toggle)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "M-x") 'kill-region)
(global-set-key (kbd "M-z") 'undo)

;; Overrides for keybindings major modes like to define
(defvar ak-keys-minor-mode-map (make-keymap) "ak-keys-minor-mode keymap.")
(define-key ak-keys-minor-mode-map (kbd "M-a") 'ido-execute-extended-command)
(define-key ak-keys-minor-mode-map (kbd "M-o") 'find-file)
(define-key ak-keys-minor-mode-map (kbd "M-s") 'save-buffer)
(define-key ak-keys-minor-mode-map (kbd "M-w") 'kill-this-buffer)
(define-minor-mode ak-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " ak" 'ak-keys-minor-mode-map)
(ak-keys-minor-mode 1)
(defun ak-minibuffer-setup-hook ()
  (ak-keys-minor-mode 0))
(add-hook 'minibuffer-setup-hook 'ak-minibuffer-setup-hook)

;; Options
(normal-erase-is-backspace-mode 1)

;; Support functions
(add-hook 'isearch-mode-hook 'isearch-hook)
(defun isearch-hook ()
  (define-key isearch-mode-map (kbd "M-;") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "M-f") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "M-:") 'isearch-repeat-backward)
  
  (define-key isearch-mode-map (kbd "M-p") 'recenter) ; was isearch-ring-retreat
  (define-key isearch-mode-map (kbd "M-n") 'nil) ; was isearch-ring-advance
  (define-key isearch-mode-map (kbd "<f11>") 'isearch-ring-retreat)
  (define-key isearch-mode-map (kbd "<f12>") 'isearch-ring-advance)
  
  (define-key isearch-mode-map (kbd "M-y") 'nil) ; was isearch-yank-kill
  
  (define-key isearch-mode-map (kbd "M-c") 'kill-ring-save) ; was isearch-toggle-case-fold
  (define-key isearch-mode-map (kbd "M-r") 'isearch-toggle-regexp)
  (define-key isearch-mode-map (kbd "M-e") 'backward-kill-word) ; was isearch-edit-string
  )

(defun ido-execute-extended-command ()
  (interactive)
  (call-interactively
   (intern
    (ido-completing-read
     "M-a "
     (all-completions "" obarray 'commandp)))))

(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

(require 'find-file-in-project)
(defun ido-find-file-in-tag-files ()
      (interactive)
      (save-excursion
        (let ((enable-recursive-minibuffers t))
          (visit-tags-table-buffer))
        (find-file
         (expand-file-name
          (ido-completing-read
           "Project file: " (tags-table-files) nil t)))))
