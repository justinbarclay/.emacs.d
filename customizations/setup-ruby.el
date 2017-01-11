(require 'smartparens-config)
(require 'rbenv)

(add-hook 'ruby-mode-hook (lambda ()
                            (robe-mode)
                            (global-rbenv-mode)
                            (yard-mode)
                            (inf-ruby-minor-mode)
                            (smartparens-mode)
                            (robe-start)
                            (flycheck-mode)
                            (indent-guide-mode 1)
                            ))

(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
;; The builtin ruby-mode in Emacs is quite simple, it just provides some simple 
;; functions for font-locking, indentation and code navigation.
;; Setup ruby-mode to auto load under any of these circumstances
(defun ome-ruby-mode-setup ()
  ;; Ah, this huge auto-mode-alist list comes from emacs prelude
  (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Capfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.thor\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Thorfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Vagrantfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . ruby-mode))
  )




;;(defun ome-inf-ruby-setup ()
;;  (require 'inf-ruby)
;;  (define-key inf-ruby-minor-mode-map (kbd "C-c C-z") 'run-ruby)
;;  (when (executable-find "pry")
;;    (add-to-list 'inf-ruby-implementations '("pry" . "pry"))
;;    (setq inf-ruby-default-implementation "pry")))

(ome-ruby-mode-setup)
;;(ome-inf-ruby-setup)

;; (defun ome-robe-mode-setup ()
;;   (add-hook 'robe-mode-hook 'ac-robe-setup)
;;   (add-to-list 'ac-modes 'inf-ruby-mode)
;;   (add-hook 'inf-ruby-mode-hook 'ac-robe-setup))
;; (setq rbenv-installation-dir "/usr/local/rbenv")
;; (rbenv-use-corresponding)

(setq ruby-indent-level 4)

