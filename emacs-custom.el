(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "chromium-browser")
 '(fringe-mode 0 nil (fringe))
 '(ido-work-directory-list-ignore-regexps (quote ("^/s/")))
 '(mode-line-format (quote ("%e" (buffer-file-truename "%f" "%b") "-%*--%l--" mode-line-modes "%-")))
 '(mouse-wheel-scroll-amount (quote (2 ((shift) . 1) ((control)))))
 '(org-agenda-files (quote ("~/wiki/wiki.org_archive")) t)
 '(speedbar-hide-button-brackets-flag t)
 '(speedbar-indentation-width 2)
 '(speedbar-show-unknown-files t)
 '(speedbar-use-images nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(speedbar-directory-face ((((class color) (background light)) (:foreground "blue4" :family "DejaVu Sans"))))
 '(speedbar-file-face ((((class color) (background light)) (:foreground "cyan4" :family "DejaVu Sans"))))
 '(speedbar-selected-face ((((class color) (background light)) (:foreground "red" :underline t :family "DejaVu Sans"))))
 '(trailing-whitespace ((((class color) (background light)) (:strike-through "lightgray")))))
