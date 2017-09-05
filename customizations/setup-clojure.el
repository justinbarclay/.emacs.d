;;;;
;; Cider
;;;;
(use-package cider
  :ensure t
  :config
  ;; REPL related stuff

  ;; REPL history file
  (setq cider-repl-history-file "~/.emacs.d/cider-history")

  ;; nice pretty printing
  (setq cider-repl-use-pretty-printing t)

  ;; nicer font lock in REPL
  (setq cider-repl-use-clojure-font-lock t)

  ;; result prefix for the REPL
  (setq cider-repl-result-prefix ";; => ")

  ;; never ending REPL history
  (setq cider-repl-wrap-history t)

  ;; looong history
  (setq cider-repl-history-size 3000)

  ;; eldoc for clojure
  (add-hook 'cider-mode-hook #'eldoc-mode)

  ;; error buffer not popping up
  (setq cider-show-error-buffer nil)

  ;; go right to the REPL buffer when it's finished connecting
  (setq cider-repl-pop-to-buffer-on-connect nil)
  ;; company mode for completion
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)

  (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
  ;; key bindings
  ;; these help me out with the way I usually develop web apps
  (defun cider-start-http-server ()
    (interactive)
    (cider-load-current-buffer)
    (let ((ns (cider-current-ns)))
      (cider-repl-set-ns ns)
      (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
      (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))
  (defun cider-refresh ()
    (interactive)
    (cider-interactive-eval (format "(user/reset)")))
  (defun cider-user-ns ()
    (interactive)
    (cider-repl-set-ns "user"))
  (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
  (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
  (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
  (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns))

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)

;;;;
;; Clojure
;;;;


(use-package clojure-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
  (setq inferior-lisp-program "lein repl")
  ;; Enable parinfer for Clojure
  (add-hook 'clojure-mode-hook 'parinfer-mode-enable)
  (add-hook 'clojure-mode-hook 'cider-mode)
  (add-hook 'clojure-mode-hook 'eldoc-mode)
  (add-hook 'clojure-mode-hook 'subword-mode)
  ;;(add-hook 'clojure-mode-hook (lambda () (cider-jack-in)))
  (add-hook 'clojurescript-mode-hook (lambda () (cider-jack-in-clojurescript)))
  (font-lock-add-keywords
   nil
   '(("(\\(facts?\\)"
      (1 font-lock-keyword-face))
     ("(\\(background?\\)"
      (1 font-lock-keyword-face))))
  (electric-pair-mode)
  (define-clojure-indent (facts 1)))
  
;; A little more syntax highlighting
(use-package clojure-mode-extra-font-locking
  :ensure t)

;; ;; go right to the REPL buffer when it's finished connecting
;; (setq cider-repl-pop-to-buffer-on-connect nil)

;; ;; When there's a cider error, show its buffer and switch to it
;; (setq cider-show-error-buffer t)
;; (setq cider-auto-select-error-buffer t)

;; ;; Where to store the cider history.
;; (setq cider-repl-history-file "~/.emacs.d/cider-history")

;; ;; Wrap when navigating history.
;; (setq cider-repl-wrap-history t)

;; ;; enable paredit in your REPL
;; (add-hook 'cider-repl-mode-hook 'parinfer-mode)

;; Use clojure mode for other extensions
;; (add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))
