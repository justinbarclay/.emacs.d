;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.


;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package dired+
  :ensure t
  :config
  (setq dired-dwim-target t)
  (setq dired-recursive-copies `always))

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs

(use-package recentf
  :ensure t
  :config
  (setq recentf-save-file (concat user-emacs-directory ".recentf"))
  (recentf-mode 1)
  (setq recentf-max-menu-items 40))

;; As Stolen from http://cestlaz.github.io/posts/using-emacs-6-swiper/ (January 10, 2017)
;; it looks like counsel is a requirement for swiper
(use-package counsel
  :ensure t
  )
(use-package counsel-projectile
  :ensure t)

(use-package flx
  :ensure t)

(use-package swiper
  :ensure t
  :demand t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-initial-inputs-alist nil)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-load-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    ;;    (global-set-key (kbd "C-c j") 'counsel-git-grep) ;; leave git to magit
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))
    

;; As Stolen from http://cestlaz.github.io/posts/using-emacs-7-avy/ (January 10, 2017)
(use-package avy
  :ensure t
  :bind ("C-c s" . avy-goto-char))

;; projectile everywhere!
(use-package projectile
  :ensure t
  :init (progn
          (projectile-mode)
          (require 'counsel-projectile)
          (setq projectile-completion-system 'ivy)))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq treemacs-follow-after-init          t
          treemacs-width                      35
          treemacs-indentation                2
          treemacs-git-integration            t
          treemacs-collapse-dirs              3
          treemacs-silent-refresh             nil
          treemacs-change-root-without-asking nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-show-hidden-files          t
          treemacs-never-persist              nil
          treemacs-is-never-other-window      nil
          treemacs-goto-tag-strategy          'refetch-index)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t))
  :bind
  (:map global-map
        ([f8]        . treemacs-toggle)
        ("<C-M-tab>" . treemacs-toggle)
        ("M-0"       . treemacs-select-window)
        ("C-c 1"     . treemacs-delete-other-windows)))
  
(use-package treemacs-projectile
  :defer t
  :ensure t
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header))
