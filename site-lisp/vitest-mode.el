;;; vitest-mode.el --- Minor mode for running Vitest/Jest tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  User

;; Author: User <user@example.com>
;; Keywords: tools, processes, javascript
;; Version: 0.1.0

;;; Commentary:

;; A minor mode to run Vitest or Jest tests and display status in the gutter.

;;; Code:

(require 'cl-lib) ;; For cl-defstruct, cl-find
(require 'json)
(require 'project)
(require 'ansi-color)

(defgroup vitest-mode nil
  "Minor mode for running Vitest/Jest tests."
  :group 'tools)

(defcustom vitest-mode-command-vitest "npx vitest"
  "Command to run Vitest."
  :type 'string
  :group 'vitest-mode)

(defcustom vitest-mode-command-jest "npx jest"
  "Command to run Jest."
  :type 'string
  :group 'vitest-mode)

(defvar vitest-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c v f") #'vitest-mode-run-file)
    (define-key map (kbd "C-c v t") #'vitest-mode-run-test-at-point)
    (define-key map (kbd "C-c v n") #'vitest-mode-next-test)
    (define-key map (kbd "C-c v p") #'vitest-mode-previous-test)
    (define-key map (kbd "C-c v N") #'vitest-mode-next-failure)
    (define-key map (kbd "C-c v P") #'vitest-mode-previous-failure)
    map)
  "Keymap for `vitest-mode`.")

;;; Face definitions for the gutter icons

(defface vitest-mode-running-face
  '((t :foreground "yellow"))
  "Face for the running state indicator.")

(defface vitest-mode-pass-face
  '((t :foreground "green"))
  "Face for the passed state indicator.")

(defface vitest-mode-fail-face
  '((t :foreground "red"))
  "Face for the failed state indicator.")

(defface vitest-mode-skip-face
  '((t :foreground "gray"))
  "Face for the skipped state indicator.")

(define-fringe-bitmap 'vitest-mode-circle
  (vector #b00111100
          #b01111110
          #b11111111
          #b11111111
          #b11111111
          #b11111111
          #b01111110
          #b00111100))

;;; Variables

(defvar-local vitest-mode-all-tests nil
  "List of all `vitest-mode-test-result` structs in the current buffer.")

(defvar-local vitest-mode-failures nil
  "List of `vitest-mode-test-result` structs for failing tests.")

;; Corrected regex for general test definition matching (including describe)
(defconst vitest-mode-test-regexp "\(?:describe\|it\|test\)\s *\(\s *['\"`]\([^'\"`]+\)['\"`]\)"
  "Regex to match test definitions (including describe blocks).")

(cl-defstruct vitest-mode-test-result
  name
  line
  start-pos  ;; Match beginning of test name in buffer
  end-pos    ;; Match end of test name in buffer
  status     ;; \"passed\", \"failed\", \"skipped\", etc.
  message    ;; Failure message, if any
  fringe-overlay ;; Emacs overlay for fringe indicator
  text-overlay)  ;; Emacs overlay for text highlight/help-echo

;;; Core Logic

(defun vitest-mode--project-root ()
  "Get the root of the current project."
  (if-let* ((project (project-current)))
      (project-root project)
    default-directory))

(defun vitest-mode--detect-runner ()
  "Detect whether to use vitest or jest based on package.json or lock files."
  (let ((root (vitest-mode--project-root)))
    (cond
     ((file-exists-p (expand-file-name "vite.config.ts" root)) 'vitest)
     ((file-exists-p (expand-file-name "vite.config.js" root)) 'vitest)
     ((file-exists-p (expand-file-name "vitest.config.ts" root)) 'vitest)
     ((file-exists-p (expand-file-name "jest.config.js" root)) 'jest)
     (t 'vitest))))

(defun vitest-mode--clear-overlays ()
  "Remove all vitest-mode overlays and clear test results."
  (dolist (test-obj vitest-mode-all-tests)
    (when (vitest-mode-test-result-fringe-overlay test-obj)
      (delete-overlay (vitest-mode-test-result-fringe-overlay test-obj)))
    (when (vitest-mode-test-result-text-overlay test-obj)
      (delete-overlay (vitest-mode-test-result-text-overlay test-obj))))
  (setq vitest-mode-all-tests nil)
  (setq vitest-mode-failures nil))

(defun vitest-mode--update-test-result-overlays (test-obj face message)
  "Update or create overlays for TEST-OBJ with FACE and MESSAGE."
  ;; Clear existing overlays for this test-obj
  (when (vitest-mode-test-result-fringe-overlay test-obj)
    (delete-overlay (vitest-mode-test-result-fringe-overlay test-obj))
    (setf (vitest-mode-test-result-fringe-overlay test-obj) nil))
  (when (vitest-mode-test-result-text-overlay test-obj)
    (delete-overlay (vitest-mode-test-result-text-overlay test-obj))
    (setf (vitest-mode-test-result-text-overlay test-obj) nil))

  (save-excursion
    (goto-char (point-min))
    (forward-line (1- (vitest-mode-test-result-line test-obj)))
    ;; Fringe overlay
    (let ((ov (make-overlay (line-beginning-position) (line-beginning-position))))
      (overlay-put ov 'before-string
                   (propertize "!" 'display
                               (list 'left-fringe 'vitest-mode-circle face)))
      (setf (vitest-mode-test-result-fringe-overlay test-obj) ov)))

  ;; Text overlay for failed tests (help-echo and underline)
  (when (eq face 'vitest-mode-fail-face)
    (let* ((start-pos (vitest-mode-test-result-start-pos test-obj))
           (end-pos (vitest-mode-test-result-end-pos test-obj))
           (ov (make-overlay start-pos end-pos)))
      (overlay-put ov 'help-echo message)
      (overlay-put ov 'face '(:underline (:style wave :color "red")))
      (setf (vitest-mode-test-result-text-overlay test-obj) ov))))


(defun vitest-mode--find-test-definitions ()
  "Scan buffer for test definitions.
Returns a list of `vitest-mode-test-result` structs (without status/overlays)."
  (let ((tests nil))
    (save-excursion
      (goto-char (point-min))
      ;; Heuristic regex for `it('name', ...`, `test("name", ...`
      (while (re-search-forward "\\(?:describe\|it\|test\)\\s *\\(\s *['\"`]\([^'\"`]+\)
['\"`]\)" nil t)
        (let ((name (match-string 1))
              (start (match-beginning 1))
              (end (match-end 1))
              (line (line-number-at-pos (match-beginning 0))))
          (push (make-vitest-mode-test-result
                 :name name
                 :line line
                 :start-pos start
                 :end-pos end)
                tests))))
    (nreverse tests))) ;; Return in order of appearance

(defun vitest-mode--mark-running ()
  "Initialize `vitest-mode-all-tests` and mark them as running."
  (vitest-mode--clear-overlays) ; Clears previous overlays/data
  (setq vitest-mode-all-tests (vitest-mode--find-test-definitions)) ; Populates vitest-mode-all-tests
  (dolist (test-obj vitest-mode-all-tests)
    (vitest-mode--update-test-result-overlays test-obj 'vitest-mode-running-face nil)))

(defun vitest-mode--process-sentinel (process event)
  "Sentinel for the test process."
  (when (memq (process-status process) '(exit signal))
    (let ((buffer (process-buffer process))
          (source-buffer (process-get process 'source-buffer)))
      (when (and (buffer-live-p buffer)
                 (buffer-live-p source-buffer))
        (with-current-buffer source-buffer
          (let ((output (buffer-string)))
            (vitest-mode--handle-results output))) ;; Pass buffer explicitly
        (kill-buffer buffer)))))

(defun vitest-mode--handle-results (output)
  "Parse OUTPUT and update overlays in current buffer (source-buffer)."
  (let ((json-start (string-match "{" output)))
    (when json-start
      (vitest-mode--clear-overlays) ;; Clear previous overlays
      (setq vitest-mode-all-tests (vitest-mode--find-test-definitions)) ;; Re-scan test definitions
      (let* ((json-str (substring output json-start))
             (json-object (condition-case nil
                              (json-read-from-string json-str)
                            (error nil))))
        (when json-object
          (vitest-mode--apply-results json vitest-mode-all-tests))))))

(defun vitest-mode--apply-results (json test-objects)
  "Map JSON results to TEST-OBJECTS and update their status and overlays."
  (setq vitest-mode-failures nil) ;; Clear failures list before repopulating
  (let ((results (append (alist-get 'testResults json) nil)))
    (dolist (file-result results)
      (let ((assertions (append (alist-get 'assertionResults file-result) nil)))
        (dolist (assertion assertions)
          (let* ((title (alist-get 'title assertion))
                 (status (alist-get 'status assertion))
                 (failure-messages (append (alist-get 'failureMessages assertion) nil))
                 (test-obj (cl-find title test-objects :key #'vitest-mode-test-result-name :test #'string=)))
            (when test-obj
              (setf (vitest-mode-test-result-status test-obj) status)
              (let* ((msg (if failure-messages (mapconcat #'identity failure-messages "\n") nil))
                     (face (cond
                            ((string= status "passed") 'vitest-mode-pass-face)
                            ((string= status "failed") 'vitest-mode-fail-face)
                            ((member status '("pending" "skipped" "todo" "disabled")) 'vitest-mode-skip-face)
                            (t 'vitest-mode-running-face))))
                (setf (vitest-mode-test-result-message test-obj) msg)
                (vitest-mode--update-test-result-overlays test-obj face msg)
                (when (string= status "failed")
                  (push test-obj vitest-mode-failures)))))))))
  ;; Sort failures by line number
  (setq vitest-mode-failures (sort vitest-mode-failures (lambda (a b) (< (vitest-mode-test-result-line a) (vitest-mode-test-result-line b))))))

(defun vitest-mode-next-test ()
  "Move to the next test definition."
  (interactive)
  (re-search-forward vitest-mode-test-regexp nil t))

(defun vitest-mode-previous-test ()
  "Move to the previous test definition."
  (interactive)
  (re-search-backward vitest-mode-test-regexp nil t))

(defun vitest-mode-next-failure ()
  "Move to the next failing test."
  (interactive)
  (if (null vitest-mode-failures)
      (message "No failing tests.")
    (let ((current-line (line-number-at-pos))
          (target-test nil))
      (dolist (test-obj vitest-mode-failures)
        (when (and (not target-test) (> (vitest-mode-test-result-line test-obj) current-line))
          (setq target-test test-obj)))
      ;; Wrap around if no next failure
      (unless target-test
        (setq target-test (car vitest-mode-failures))
        (message "Wrapped to first failure."))
      (goto-char (point-min))
      (forward-line (1- (vitest-mode-test-result-line target-test)))
      (back-to-indentation))))

(defun vitest-mode-previous-failure ()
  "Move to the previous failing test."
  (interactive)
  (if (null vitest-mode-failures)
      (message "No failing tests.")
    (let ((current-line (line-number-at-pos))
          (target-test nil))
      (dolist (test-obj (reverse vitest-mode-failures))
        (when (and (not target-test) (< (vitest-mode-test-result-line test-obj) current-line))
          (setq target-test test-obj)))
      ;; Wrap around if no previous failure
      (unless target-test
        (setq target-test (car (last vitest-mode-failures)))
        (message "Wrapped to last failure."))
      (goto-char (point-min))
      (forward-line (1- (vitest-mode-test-result-line target-test)))
      (back-to-indentation))))

(defun vitest-mode-run-file ()
  "Run tests for the current file."
  (interactive)
  (let* ((file (buffer-file-name))
         (runner (vitest-mode--detect-runner))
         (cmd (if (eq runner 'vitest)
                  (format "%s run --reporter=json %s" vitest-mode-command-vitest file)
                (format "%s --json %s" vitest-mode-command-jest file)))
         (output-buffer (generate-new-buffer "*vitest-mode-output*"))
         (default-directory (vitest-mode--project-root)))
    (let ((proc (make-process
                 :name "vitest-mode-runner"
                 :buffer output-buffer
                 :command (split-string cmd)
                 :sentinel #'vitest-mode--process-sentinel
                 :noquery t)))
      (process-put proc 'source-buffer (current-buffer))
      (vitest-mode--mark-running))))

(defun vitest-mode-run-test-at-point ()
  "Run the test at point using filename:line syntax."
  (interactive)
  (let* ((file (buffer-file-name))
         (line (line-number-at-pos))
         (runner (vitest-mode--detect-runner))
         (output-buffer (generate-new-buffer "*vitest-mode-output*"))
         ;; Construct command parts
         (base-cmd (split-string (if (eq runner 'vitest)
                                     vitest-mode-command-vitest
                                   vitest-mode-command-jest)))
         (args (if (eq runner 'vitest)
                   (list "run" "--reporter=json" (format "%s:%d" file line))
                 (list "--json" (format "%s:%d" file line))))
         (full-cmd (append base-cmd args))
         (default-directory (vitest-mode--project-root)))

    (let ((proc (make-process
                 :name "vitest-mode-runner"
                 :buffer output-buffer
                 :command full-cmd
                 :sentinel #'vitest-mode--process-sentinel
                 :noquery t)))
      (process-put proc 'source-buffer (current-buffer))
      (vitest-mode--mark-running))))

;;;###autoload
(define-minor-mode vitest-mode
  "Minor mode to run Vitest/Jest tests from Emacs."
  :lighter " Vitest"
  :keymap vitest-mode-map
  :group 'vitest-mode)

(provide 'vitest-mode)
;;; vitest-mode.el ends here
