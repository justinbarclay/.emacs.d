;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell

(use-package diminish
  :demand t
  :config (progn
            ;;            (diminish 'auto-revert-mode)
            ;;            (diminish 'outline-minor-mode)
            ;;            (diminish 'amd-mode)
            (diminish 'js2-refactor-mode)
            (diminish 'tern-mode)))
;;            (diminish 'eslintd-fix-mode)))
;;            (diminish 'widgetjs-mode)))


(when (memq system-type '(windows-nt))
  (exec-path-from-shell-initialize)
  (setq explicit-shell-file-name "c:/windows/system32/bash.exe")
  (setq shell-file-name "bash")
  (setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
  (setenv "SHELL" shell-file-name)
  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
)


(defun xah-run-current-file ()
  "Execute the current file.
For example, if the current buffer is the file x.py, then it'll call 「python x.py」 in a shell.
The file can be Emacs Lisp, PHP, Perl, Python, Ruby, JavaScript, Bash, Ocaml, Visual Basic, TeX, Java, Clojure.
File suffix is used to determine what program to run.

If the file is modified or not saved, save it automatically before run.

URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
version 2016-01-28"
  (interactive)
  (let (
         (-suffix-map
          ;; (‹extension› . ‹shell program name›)
          `(
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "python")
            ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
            ("rb" . "ruby")
            ("go" . "go run")
            ("js" . "node") ; node.js
            ("sh" . "bash")
            ("clj" . "java -cp /home/xah/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
            ("rkt" . "racket")
            ("ml" . "ocaml")
            ("vbs" . "cscript")
            ("tex" . "pdflatex")
            ("latex" . "pdflatex")
            ("java" . "javac")
            ;; ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
            ))

         -fname
         -fSuffix
         -prog-name
         -cmd-str)

    (when (null (buffer-file-name)) (save-buffer))
    (when (buffer-modified-p) (save-buffer))

    (setq -fname (buffer-file-name))
    (setq -fSuffix (file-name-extension -fname))
    (setq -prog-name (cdr (assoc -fSuffix -suffix-map)))
    (setq -cmd-str (concat -prog-name " \""   -fname "\""))

    (cond
     ((string-equal -fSuffix "el") (load -fname))
     ((string-equal -fSuffix "java")
      (progn
        (shell-command -cmd-str "*xah-run-current-file output*" )
        (shell-command
         (format "java %s" (file-name-sans-extension (file-name-nondirectory -fname))))))
     (t (if -prog-name
            (progn
              (message "Running…")
              (shell-command -cmd-str "*xah-run-current-file output*" ))
          (message "No recognized program file suffix for this file."))))))

(global-set-key (kbd "s-r") 'xah-run-current-file)

(use-package eshell
  :config (progn
            (eval-after-load 'esh-opt
              '(progn
                 (require 'em-prompt)
                 (require 'em-term)
                 (require 'em-cmpl)
                 (setenv "PAGER" "cat")
                 (add-hook 'eshell-mode-hook
                           #'company-mode)

                 (add-to-list 'eshell-visual-commands "ssh")
                 (add-to-list 'eshell-visual-commands "htop")
                 (add-to-list 'eshell-visual-commands "top")
                 (add-to-list 'eshell-visual-commands "tail")
                 (add-to-list 'eshell-visual-commands "vim")
                 (add-to-list 'eshell-visual-commands "bower")
                 (add-to-list 'eshell-visual-commands "npm")

                 (add-to-list 'eshell-command-completions-alist
                              '("gunzip" "gz\\'"))
                 (add-to-list 'eshell-command-completions-alist
                              '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))))))

;;(use-package eldoc
;;  :config (global-eldoc-mode))
