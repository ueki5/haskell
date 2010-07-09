(setq haskell-program-name "/usr/bin/ghci -i~/dev/haskell/RWH")
(load "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(setq inferior-haskell-hook
      (function
       (lambda ()
	 (set-buffer-file-coding-system 'utf-8)
	 (set-buffer-process-coding-system 'utf-8 'utf-8))))
(add-to-list 'default-frame-alist
              '(font . "-Adobe-Courier-Bold-R-Normal--24-230-75-75-M-80-ISO8859-1"))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(color-theme-legal-frame-parameters "\\(color\\|mode\\|font\\|height\\|width\\)$")
 '(color-theme-selection "Deep Blue" nil (color-theme))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(transient-mark-mode t))
