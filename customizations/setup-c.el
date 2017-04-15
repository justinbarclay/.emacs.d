(use-package smartparens
  :ensure t
  :init (add-hook 'c-mode-hook 'smartparens-mode))

(add-hook 'c-mode-hook flycheck-mode)
(add-hook 'c-mode-hook semantic-mode)
