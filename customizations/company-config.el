(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; (require 'company-ycmd)
;; (company-ycmd-setup)
;; (company-ycmd-enable-comprehensive-automatic-completion)

;; (require 'ycmd)
;; (add-hook 'after-init-hook #'global-ycmd-mode)
