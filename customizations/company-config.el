(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(setq company-idle-delay 0.1)

(eval-after-load 'company
  '(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))

(setq company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
        company-preview-frontend
        company-echo-metadata-frontend))

(setq company-auto-complete t)
;; (require 'company-ycmd)

;; (company-ycmd-setup)
;; (company-ycmd-enable-comprehensive-automatic-completion)

;; (require 'ycmd)
;; (add-hook 'after-init-hook #'global-ycmd-mode)
