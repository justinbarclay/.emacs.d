(use-package smartparens
  :ensure t
  :init (add-hook 'c-mode-hook 'smartparens-mode))
(use-package c-eldoc
  :ensure t)

(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'semantic-mode)
(add-hook 'c-mode-hook 'ycmd-mode)
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
