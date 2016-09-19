;;(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-20160827.649")
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

(require 'auto-complete-config)
(ac-config-default)

(require 'auto-complete-clang)

(setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers ac-source-clang))

;; defun xwl-c-mode-common-hook ()
;;   (setq ac-sources (cons 'ac-source-clang ac-sources)))

(add-hook 'c-mode-common-hook 'xwl-c-mode-common-hook)
(global-auto-complete-mode t)
