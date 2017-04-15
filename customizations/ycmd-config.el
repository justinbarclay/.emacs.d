(use-package ycmd
  :ensure t
  :init (add-hook 'c-mode-hook 'ycmd-mode)
  :config
  (list
   (set-variable 'ycmd-server-command '("/Users/Justin/.pyenv/shims/python" "/Users/Justin/ycmd/ycmd"))
   (set-variable 'ycmd-extra-conf-whitelist '("~/*"))
   ((set-variable 'ycmd-global-config "~/.ycm_extra_conf.py"))
   (require 'ycmd-eldoc)
   (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup)
   (setq ycmd-force-semantic-completion t)
   (add-hook 'ycmd-file-parse-result-hook 'flycheck-ycmd--cache-parse-results)))

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
  :init
  (flycheck-ycmd-setup)
  (add-to-list 'flycheck-checkers 'ycmd))




;; Make sure the flycheck cache sees the parse results

;; Add the ycmd checker to the list of available checkers
