;;;;
;; Packages
;;;;
(require 'package)
(setq package-archives '(("tromey" . "http://tromey.com/elpa/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
;;(require 'package)
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ido-ubiquitous

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; project navigation
    projectile

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; edit html tags like sexps
    tagedit

    ;; multiple cursos
    multiple-cursors

    ;; auto-complete for easy auto-completeing for languages
    ;; auto-complete
    company
    ;; company-irony
    company-quickhelp
    ycmd
    company-ycmd
    
    ;; add syntax checking
    flycheck
    ;; flycheck-ycmd

    ;; visibile max line length
    fill-column-indicator

    ;; git integration
    magit
    
    ;; C++ Auto Complete Support
    ;; auto-complete-clang
    ;; Powerline and themes
    powerline
    airline-themes
    ;; Eval and replace

    ;; Start up profiler
    esup
    ))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;;
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")


;;;;
;; Customization
;;;;

;;Turn off tool-bar
(tool-bar-mode -1)

;; Set C tabs length
(setq-default c-basic-offset 4)
;; Starting conditions for Emacs
;; Setting Emacs to take focus over terminal
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(x-focus-frame nil)
;;
;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; Setup for auto complete
;;  (load "AC-Config.el")
(load "company-config.el")
;; For editing lisps
(load "elisp-editing.el")
;; Auto-complete backend
(load "ycmd-config.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")
(load "setup-python.el")

;; Enable FCI as a global minor mode
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)
(global-flycheck-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(coffee-tab-width 2)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (tron-theme tagedit solarized-theme smex rainbow-delimiters projectile paredit multiple-cursors monokai-theme monochrome-theme material-theme majapahit-theme magit look-dired jedi ido-ubiquitous flycheck-ycmd fill-column-indicator exec-path-from-shell esup cparen company-ycmd company-quickhelp company-irony clojure-mode-extra-font-locking cider auto-package-update auto-complete-clang auto-complete-chunk auto-complete-c-headers ample-theme airline-themes achievements ac-python ac-c-headers)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
