;; These customizations change the way emacs looks and disable/enable
;; some user interface elements. Some useful customizations are
;; commented out, and begin with the line "CUSTOMIZE". These are more
;; a matter of preference and may require some fiddling to match your
;; preferences

;; Turn off the menu bar at the top of each frame because it's distracting
;;(menu-bar-mode 1)

;; Show line numbers
(set-default 'display-line-numbers 'visual)
(set-default 'display-line-numbers-current-absolute 1)

(setq display-line-numbers (quote visual))
;;(setq display-line-numbers-current-absolute nil)
;; Colour all brackets based on depth
;; Currently allows two different bracket colours based level

(use-package rainbow-delimiters
  :ensure t
  :init
  (list (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
        (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
        (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
        (add-hook 'parinfer-mode-hook 'rainbow-delimiters-mode)
        (add-hook 'java-mode 'rainbow-delimiters-mode)))

;; You can uncomment this to remove the graphical toolbar at the top. After
;; awhile, you won't need the toolbar.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
;; Color Themes
;; Read http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
;; for a great explanation of emacs color themes.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html
;; for a more technical explanation.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

(use-package dracula-theme
  :config
  (load-theme 'dracula t))
 
;; increase font size for better readability
(set-face-attribute 'default nil
                    :family "Inconsolata for Powerline" :height 180 :weight 'normal)

;; Uncomment the lines below by removing semicolons and play with the
;; values in order to set the width (in characters wide) and height
;; (in lines high) Emacs will have whenever you start it
;; (setq initial-frame-alist '((top . 0) (left . 0) (width . 177) (height . 53)))

;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
 select-enable-clipboard t

 ;; I'm actually not sure what this does but it's recommended?
 select-enable-primary t

 ;; Save clipboard strings into kill ring before replacing them.
 ;; When one selects something in another program to paste it into Emacs,
 ;; but kills something in Emacs before actually pasting it,
 ;; this selection is gone unless this variable is non-nil
 save-interprogram-paste-before-kill t

 ;; Shows all options when running apropos. For more info,
 ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
 apropos-do-all t

 ;; Mouse yank commands yank at point instead of at click.
 mouse-yank-at-point t)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; no bell
(setq ring-bell-function 'ignore)

;; Style fill column indicator
;; (setq fci-rule-width 2)
;; (setq fci-rule-color "green")
;; (setq fci-rule-column 80)

;;; Commentary
;; Powerline and customizations
(require 'powerline)
(powerline-center-theme)
(setq powerline-default-separator 'wave)
(use-package all-the-icons		
  :ensure t)
             
(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config))
  ;; (spaceline-emacs-theme))

(use-package spaceline-all-the-icons
  :ensure t
  :after spaceline
  :config (spaceline-all-the-icons-theme)
  (custom-set-faces '(spaceline-highlight-face ((t (:background "#cb619e"
                                                    :foreground "#f8f8f2"
                                                    :inherit 'mode-line))))
                  '(powerline-active2 ((t (:background "#44475a"
                                            :foregound "#50fa7b"
                                            :inherit 'mode-line))))
                  '(mode-line ((t (:background "#282a36"
                                            :foregound "#50fa7b"
                                            :inherit 'mode-line))))
                  '(powerline-active1 ((t (:background "#6272a4"
                                            :foregound "#50fa7b"
                                            :inherit 'mode-line)))))
  (setq spaceline-all-the-icons-separator-type 'wave))
  
  

(provide 'ui)
;;; ui.el ends here

;;(:inherit 'mode-line :foreground "thistle4" :background "DarkOrchid4")

;; (custom-set-faces '(spaceline-highlight-face ((t (:background "#cb619e"
;;                                                     :foreground "#f8f8f2"
;;                                                     :inherit 'mode-line))))
;;                   '(powerline-active2 ((t (:background "#44475a"
;;                                             :foregound "#50fa7b"
;;                                             :inherit 'mode-line))))
;;                   '(mode-line ((t (:background "#282a36"
;;                                             :foregound "#50fa7b"
;;                                             :inherit 'mode-line))))
;;                   '(powerline-active1 ((t (:background "#6272a4"
;;                                             :foregound "#50fa7b"
;;                                             :inherit 'mode-line)))))
