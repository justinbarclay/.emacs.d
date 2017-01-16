(use-package ycmd
  :ensure t
  :config
  (list (set-variable 'ycmd-server-command '("/Users/Justin/.pyenv/shims/python" "/Users/Justin/ycmd/ycmd"))
   (set-variable 'ycmd-extra-conf-whitelist '("~/*"))
   (setq ycmd-force-semantic-completion t)))

;; In some cases you may see that company and flycheck interfere with one another.
;; You can end up with strange completion artifacts in your buffers.
;; This mostly seems to happen when you run emacs in "terminal mode", i.e.
;; with emacs -nw.
;; (setq flycheck-indication-mode nil)

(use-package company-ycmd
  :ensure t
  :config
  (company-ycmd-setup))

(use-package flycheck-ycmd
  :ensure t
  :init (flycheck-ycmd-setup)
  (progn ((add-hook 'ycmd-file-parse-result-hook 'flycheck-ycmd--cache-parse-results)
          (add-to-list 'flycheck-checkers 'ycmd))))



;; Make sure the flycheck cache sees the parse results


;; Add the ycmd checker to the list of available checkers
