(use-package company
  :ensure t
  :config
  (progn
    (eval-after-load 'company
      '(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))
    
    (eval-after-load 'company
      '(push 'company-robe company-backends))
    (setq company-idle-delay 0.3)
    (add-hook 'after-init-hook 'global-company-mode)  
    (setq company-frontends
          '(company-pseudo-tooltip-unless-just-one-frontend
            company-preview-frontend
            company-echo-metadata-frontend))
    
    (setq company-auto-complete t)
    (global-set-key (kbd "<C-tab>") 'company-capf)))



