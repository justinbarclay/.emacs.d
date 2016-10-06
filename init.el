

;;;;
;; PackageS
;;;;
(require 'package)
;; Define package repositories
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives
;;             '("tromey" . "http://tromey.com/elpa/") t)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)

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
(x-focus-frame nil)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
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
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector ["white" "#303030" "#b3b3b3" "#606060"])
 '(blink-cursor-mode nil)
 '(coffee-tab-width 2)
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (material)))
 '(custom-safe-themes
   (quote
    ("946e871c780b159c4bb9f580537e5d2f7dba1411143194447604ecbaf01bd90c" "962dacd99e5a99801ca7257f25be7be0cebc333ad07be97efd6ff59755e6148f" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "9e54a6ac0051987b4296e9276eecc5dfb67fdcd620191ee553f40a9b6d943e78" "3a53f98f4354d66ffaec1edce1bc54a3c622c8a73a583e90fde456b311b889f2" "341a1f149c8ab55893e5a065e96235e43ee9f82423f4c018bf31a430e1dc1b0f" "5b29f90eb304b440c908de31caf7d730db451b5909e8a84a2e7cd8d60f6d5c1f" "d4e9f95acd51433b776f1127143bbc7d0f1d41112d547e5b7a9a506be369dc39" "cab317d0125d7aab145bc7ee03a1e16804d5abdfa2aa8738198ac30dc5f7b569" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664" default)))
 '(fci-rule-color "#3E3D31")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3E3D31" . 0)
     ("#67930F" . 20)
     ("#349B8D" . 30)
     ("#21889B" . 50)
     ("#968B26" . 60)
     ("#A45E0A" . 70)
     ("#A41F99" . 85)
     ("#3E3D31" . 100))))
 '(hl-sexp-background-color "#1c1f26")
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (tron-theme company-anaconda ycmd auto-complete-clang tagedit smex rainbow-delimiters projectile paredit multiple-cursors monokai-theme monochrome-theme material-theme majapahit-theme magit look-dired jedi ido-ubiquitous flycheck fill-column-indicator exec-path-from-shell cparen company clojure-mode-extra-font-locking cider auto-package-update auto-complete-chunk auto-complete-c-headers achievements ac-python ac-c-headers)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3E3D31" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(xterm-color-names
   ["#f7f3e5" "#b34d6c" "#777e62" "#ae6a56" "#4d6e66" "#685672" "#6b6254" "#4d5452"])
 '(xterm-color-names-bright
   ["#e0ddce" "#db4764" "#d7875f" "#7ca194" "#807b8b" "#806c5b" "#4d5452"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
