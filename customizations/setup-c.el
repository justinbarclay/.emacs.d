(use-package rainbow-delimiters
  :init (add-hook 'c-mode-hook 'rainbow-delimiters-mode))

(use-package ycmd
  :ensure t
  :init (add-hook 'c-mode-hook 'ycmd-mode))
(use-package smartparens
  :ensure t
  :init (add-hook 'c-mode-hook 'smartparens-mode))                 
