(require 'cl)
;; These customizations change the way emacs looks and disable/enable
;; some user interface elements. Some useful customizations are
;; commented out, and begin with the line "CUSTOMIZE". These are more
;; a matter of preference and may require some fiddling to match your
;; preferences

;; Turn off the menu bar at the top of each frame because it's distracting
;;(menu-bar-mode 1)


;; (line-number-mode)
(global-display-line-numbers-mode)

;; Show line numbers
(set-default 'display-line-numbers-type 'visual)

(setq display-line-numbers-current-absolute t)

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
  :ensure t
  :demand t
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

(defun font-name-replace-size (font-name new-size)
  (let ((parts (split-string font-name "-")))
    (setcar (nthcdr 7 parts) (format "%d" new-size))
    (mapconcat 'identity parts "-")))

(defun increment-default-font-height (delta)
  "Adjust the default font height by DELTA on every frame.
The pixel size of the frame is kept (approximately) the same.
DELTA should be a multiple of 10, in the units used by the
:height face attribute."
  (let* ((new-height (+ (face-attribute 'default :height) delta))
         (new-point-height (/ new-height 10)))
    (dolist (f (frame-list))
      (with-selected-frame f
        ;; Latest 'set-frame-font supports a "frames" arg, but
        ;; we cater to Emacs 23 by looping instead.
        (set-frame-font (font-name-replace-size (face-font 'default)
                                                new-point-height)
                        t)))
    (set-face-attribute 'default nil :height new-height)
    (message "default font size is now %d" new-point-height)))

(defun increase-default-font-height ()
  (interactive)
  (increment-default-font-height 10))

(defun decrease-default-font-height ()
  (interactive)
  (increment-default-font-height -10))

(global-set-key (kbd "C-M-=") 'increase-default-font-height)
(global-set-key (kbd "C-M--") 'decrease-default-font-height)

;; Style fill column indicator
;; (setq fci-rule-width 2)
;; (setq fci-rule-color "green")
;; (setq fci-rule-column 80)

;;; Commentary
;; Powerline and customizations
(use-package powerline
  :ensure t
  :config
  (powerline-center-theme))
(setq powerline-default-separator 'wave)
(use-package all-the-icons		
  :ensure t)

(use-package spaceline
  :ensure t
  :demand t
  :config
  (require 'spaceline-config))
  ;; (spaceline-emacs-theme))

(use-package spaceline-all-the-icons
  :ensure t 
  :after spaceline
  :defer t
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
;; ;;; ui.el ends here
