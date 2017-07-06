;; Org config used from https://github.com/zamansky/dotemacs/commit/0d1f8ad89ab3e69cb9320811c5ec63409880eadd

(use-package org
  :init
  (setq truncate-lines t))
(use-package org-bullets
  :ensure t
  :init
  (progn
    (global-company-mode '(not org-mode))
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))) 
    (custom-set-variables
     '(org-default-notes-file (concat org-directory "/notes.org"))
     '(org-directory "~/Dropbox/orgfiles")
     '(org-export-html-postamble nil)
     '(org-hide-leading-stars t)
     '(org-startup-folded (quote overview))
     '(org-startup-indented t))
    (global-set-key (kbd "C-c a") 'org-agenda)
    (global-set-key (kbd "C-c c") 'org-capture)
    :config
    (setq org-startup-truncated nil)
    (setq org-capture-templates
          '(("a" "Appointment" entry (file+headline  "~/Dropbox/orgfiles/gcal.org" "Appointments")
             "* TODO %?\n:PROPERTIES:\n\n:END:\nDEADLINE: %^T \n %i\n")
            ("l" "Link" entry (file+headline "~/Dropbox/orgfiles/links.org" "Links")
             "* %? %^L %^g \n%T" :prepend )))
    (setq org-agenda-files (list "~/Dropbox/orgfiles/gcal.org")) 
    
    ))

(use-package org-eww)




(require 'ox-odt)

(use-package org-present
  :ensure t)


(defadvice org-capture-finalize 
    (after delete-capture-frame activate)  
  "Advise capture-finalize to close the frame."  
  (if (equal "capture" (frame-parameter nil 'name))  
      (delete-frame)))

(defadvice org-capture-destroy 
    (after delete-capture-frame activate)  
  "Advise capture-destroy to close the frame."  
  (if (equal "capture" (frame-parameter nil 'name))  
      (delete-frame)))

(use-package noflet
  :ensure t )
(defun make-capture-frame ()
  "Create a new frame and run 'org-capture'."
  (interactive)
  (make-frame '((name . "capture")))
  (select-frame-by-name "capture")
  (delete-other-windows)
  (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
	  (org-capture)))


(use-package org-gcal
  :ensure t
  :config
  (setq org-gcal-client-id "681014132158-pht7365idrlknvon718pb7cu6oerjcrc.apps.googleusercontent.com"
        org-gcal-client-secret ""
        org-gcal-file-alist '(("justincbarclay@gmail.com" .  "~/Dropbox/orgfiles/gcal.org"))))

(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync)))
(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))

(defun mycalendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    ;; (cfw:org-create-source "Green")  ; orgmode source
    (cfw:ical-create-source "gcal" "https://calendar.google.com/calendar/ical/justincbarclay%40gmail.com/private-5dd5c123e5f8f2bf66c9a03347dcb2bb/basic.ics" "IndianRed")))) ; google calendar ICS

(use-package calfw
  :ensure t
  :config
  (require 'calfw) 
  (require 'calfw-org)
  (setq cfw:org-overwrite-default-keybinding t)
  (require 'calfw-ical))

(use-package calfw-gcal
  :ensure t
  :config
  (require 'calfw-gcal))
