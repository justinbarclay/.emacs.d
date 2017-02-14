(use-package rainbow-delimiters
  :init (add-hook 'c-mode-hook 'rainbow-delimiters-mode))

(use-package ycmd-mode
  :init (add-hook 'c-mode-hook 'ycmd-mode))
(use-package smart-parens
  :init (add-hook 'c-mode-hook 'smartparens-mode))                 
