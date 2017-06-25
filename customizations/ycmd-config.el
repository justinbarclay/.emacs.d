(use-package flycheck-ycmd
  :ensure t
  :commands (global-flycheck-mode)
  :init
  (flycheck-ycmd-setup)
  (add-to-list 'flycheck-checkers 'ycmd))

(use-package ycmd
  :ensure t
  :commands (ycmd-mode)
  :config
   (set-variable 'ycmd-server-command '("python" "/Users/Justin/ycmd/ycmd"))
   (set-variable 'ycmd-extra-conf-whitelist '("~/*"))
   (set-variable 'ycmd-global-config "~/.ycm_extra_conf.py")
;;   (set-variable 'ycmd-rust-src-path (getenv "RUST_SRC_PATH"))
   (require 'ycmd-eldoc)
   (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup)
   (setq ycmd-force-semantic-completion t)
   (add-hook 'ycmd-file-parse-result-hook 'flycheck-ycmd--cache-parse-results))

;; In some cases you may see that company and flycheck interfere with one another.
;; You can end up with strange completion artifacts in your buffers.
;; This mostly seems to happen when you run emacs in "terminal mode", i.e.
;; with emacs -nw.
;; (setq flycheck-indication-mode nil)

(use-package company-ycmd
  :ensure t
  :commands (ycmd-mode)
  :config
  (company-ycmd-setup))

;; Make sure the flycheck cache sees the parse results

;; Add the ycmd checker to the list of available checkers
