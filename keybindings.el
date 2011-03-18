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
;; (global-set-key (kbd "C-a") 'beginning-of-line)
;; (global-set-key (kbd "C-n") 'make-frame-command)
(global-set-key (kbd "C-b") 'switch-to-buffer)
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "C-d") 'delete-char)
;; (global-set-key (kbd "C-e") 'end-of-line)
;; (global-set-key (kbd "C-k") 'kill-line)
;; (global-set-key (kbd "M-b") 'bookmark-jump)
(global-set-key (kbd "M-f") 'isearch-forward)
(global-set-key (kbd "M-F") 'rgrep)
;; (global-set-key (kbd "M-h") 'backward-char)
;; (global-set-key (kbd "M-j") 'next-line)
;; (global-set-key (kbd "M-k") 'previous-line)
;; (global-set-key (kbd "M-l") 'forward-char)
;; (global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-m") 'magit-status)
(global-set-key (kbd "M-p") 'ido-find-file-in-tag-files)
(global-set-key (kbd "M-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "M-r") 'ido-goto-symbol) ; or any key you see fit
(global-set-key (kbd "C-s") 'other-frame)
(global-set-key (kbd "M-t") 'revert-buffer)
(global-set-key (kbd "C-t") 'multi-term-dedicated-toggle)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "M-w") 'close-current-buffer)
(global-set-key (kbd "M-x") 'kill-region)
(global-set-key (kbd "M-z") 'undo)

;; Overrides for keybindings major modes like to define
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")
(define-key my-keys-minor-mode-map (kbd "M-a") 'ido-execute-extended-command)
(define-key my-keys-minor-mode-map (kbd "M-o") 'find-file)
(define-key my-keys-minor-mode-map (kbd "M-s") 'save-buffer)
(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)
(my-keys-minor-mode 1)

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

(defun close-current-buffer ()
"Close the current buffer.

Similar to (kill-buffer (current-buffer)) with the following addition:

• prompt user to save if the buffer has been modified even if the buffer is not associated with a file.
• make sure the buffer shown after closing is a user buffer.
• if the buffer is a file, add the path to the list recently-closed-buffers.

A emacs buffer is one who's name starts with *.
Else it is a user buffer."
 (interactive)
 (let (emacsBuff-p isEmacsBufferAfter)
   (if (string-match "^*" (buffer-name))
       (setq emacsBuff-p t)
     (setq emacsBuff-p nil))

   ;; offer to save buffers that are non-empty and modified, even for non-file visiting buffer. (because kill-buffer does not offer to save buffers that are not associated with files)
   (when (and (buffer-modified-p)
              (not emacsBuff-p)
              (not (string-equal major-mode "dired-mode"))
              (if (equal (buffer-file-name) nil) 
                  (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
                t
                )
              )
     (if (y-or-n-p
            (concat "Buffer " (buffer-name) " modified; Do you want to save?"))
       (save-buffer)
       (set-buffer-modified-p nil)))

   ;; save to a list of closed buffer
   (when (not (equal buffer-file-name nil))
     (setq recently-closed-buffers
           (cons (cons (buffer-name) (buffer-file-name)) recently-closed-buffers))
     (when (> (length recently-closed-buffers) recently-closed-buffers-max)
           (setq recently-closed-buffers (butlast recently-closed-buffers 1))
           )
     )

   ;; close
   (kill-buffer (current-buffer))

   ;; if emacs buffer, switch to a user buffer
   (if (string-match "^*" (buffer-name))
       (setq isEmacsBufferAfter t)
     (setq isEmacsBufferAfter nil))
   (when isEmacsBufferAfter
     (previous-user-buffer)
     )
   )
 )
