(use-package company
  :ensure t
  :commands (global-company-mode)
  :config
  (progn
    (eval-after-load 'company
      '(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))
    
    (eval-after-load 'company
      '(push 'company-robe company-backends))
    (setq company-idle-delay 0.5)
    (setq company-frontends
          '(company-pseudo-tooltip-unless-just-one-frontend
            company-preview-frontend
            company-echo-metadata-frontend))
    
    (setq company-auto-complete t)
    (setq company-tooltip-align-annotations t)
    (global-set-key (kbd "<C-tab>") 'company-capf)))

(add-hook 'after-init-hook 'global-company-mode)
