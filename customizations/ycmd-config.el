(require 'ycmd)
(add-hook 'after-init-hook #'global-ycmd-mode)

(set-variable 'ycmd-server-command '("python" "/Users/Justin/ycmd/ycmd"))

(require 'flycheck-ycmd)
(flycheck-ycmd-setup)

;; In some cases you may see that company and flycheck interfere with one another. 
;; You can end up with strange completion artifacts in your buffers. 
;; This mostly seems to happen when you run emacs in "terminal mode", i.e. 
;; with emacs -nw.
(setq flycheck-indication-mode nil)

(require 'company-ycmd)
(company-ycmd-setup)
