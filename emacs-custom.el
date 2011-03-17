(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(mode-line-format (quote ("%e" (buffer-file-truename "%f" "%b") "-%*--%l-%c-" mode-line-modes "%-")))
 '(mode-line-inverse-video t)
 '(org-agenda-files (quote ("~/work.org_archive")) t)
 '(speedbar-vc-do-check nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#191717" :foreground "#D2DEC4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "apple" :family "Menlo"))))
 '(mode-line ((t (:background "white" :foreground "#3F3B3B" :box nil))))
 '(mode-line-inactive ((default (:inherit mode-line)) (((class color) (min-colors 88) (background dark)) (:foreground "grey50" :weight light)))))
