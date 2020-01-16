;;; bloop.el --- Run Bloop from Emacs   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (c) 2018 Paweł Bartkiewicz

;; Author: Paweł Bartkiewicz <tuuresairon+emacs.bloop@gmail.com>
;; URL: https://github.com/tues/emacs-bloop
;; Keywords: scala bloop compilation tools convenience
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; TODO

;;; Code:

(require 'json)
(require 'subr-x)
(require 'comint)

;;; Customization
(defgroup bloop nil
  "Run Bloop from Emacs."
  :group 'tools
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/tues/emacs-bloop"))

(defcustom bloop-program-name "bloop"
  "Program invoked by the `bloop-exec' command."
  :type 'string
  :group 'bloop)

(defcustom bloop-reporter "scalac"
  "Either bloop or scalac. The main difference is that bloop shows errors in reverse order. Emacs generally assumes the first error in the output is the most relavent so the scalac reporter will most likely be preferred. This is used for test and compile."
  :type 'string
  :group 'bloop)

(defcustom bloop-console-prompt "scala> "
  "Prompt for Bloop console."
  :type 'string
  :group 'bloop)

(defcustom bloop-console-prompt2 "     | "
  "Prompt for Bloop console, which shows up when the line is continuing."
  :type 'string
  :group 'bloop)

(defconst bloop-ansi-escape-re
  (rx (or ?\233 (and ?\e ?\[))
      (zero-or-more (char (?0 . ?\?)))
      (zero-or-more (char ?\s ?- ?\/))
      (char (?@ . ?~))))

(defun bloop-prompt-regexp ()
  (concat "^\\(" bloop-console-prompt "\\|@ \\)[ ]*"))

(defun bloop-buffer-name (_root command)
  (concat "*bloop-" command "*"))

(defun bloop-directory (root)
  (file-name-as-directory (concat root ".bloop")))

(defun bloop-find-root (file)
  (file-name-as-directory (or (locate-dominating-file file ".bloop")
                              (error (concat "Can't find `.bloop' directory. "
                                             "Have you generated bloop config for this project? "
                                             "https://scalacenter.github.io/bloop/docs/installation/")))))

(defun bloop-project-files (bloop-dir)
  (directory-files bloop-dir t "\\.json$"))

(defun bloop-read-project-file (project-file)
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (json-read-file project-file))
         (project (gethash "project" json)))
    (when project
      (let ((name (gethash "name" project))
            (dirs (gethash "sources" project)))
        (cons name (mapcar 'file-name-as-directory dirs))))))

(defun bloop-longest-string (func strings)
  (let ((sorted (sort strings (lambda (x y) (> (length (funcall func x))
                                               (length (funcall func y)))))))
    (car sorted)))

(defun bloop-project-match-file (project file)
  (let* ((name (car project))
         (sources (cdr project))
         (filtered (seq-filter (lambda (path) (string-prefix-p path file)) sources)))
    (cons name (bloop-longest-string 'identity filtered))))

(defun bloop-find-project (root file)
  (let* ((project-files (bloop-project-files (bloop-directory root)))
         (projects (mapcar 'bloop-read-project-file project-files))
         (sources (mapcar (lambda (project) (bloop-project-match-file project file)) projects))
         (filtered (seq-filter (lambda (x) (cdr x)) sources))
         (project (bloop-longest-string 'cdr filtered)))
    project))

(defun bloop-current-project (root)
  (bloop-find-project root (buffer-file-name)))

(defun bloop-exec (comint root command &rest args)
  (unless command (error "Missing argument `command'."))

  (let* ((buffer-name (bloop-buffer-name root command))
         (raw-command (cons bloop-program-name (cons command args)))
         (full-command (string-join (mapcar 'shell-quote-argument raw-command) " "))
         (default-directory default-directory)
         (inhibit-read-only 1))

    (when (not (executable-find bloop-program-name))
      (error (concat "`%s' not found. Is bloop installed and on PATH? "
                     "See `bloop-program-name' variable.")
             bloop-program-name))

    (if comint
        (with-current-buffer (get-buffer-create buffer-name)
          (setq-local comint-output-filter-functions
                      (remove 'ansi-color-process-output
                              comint-output-filter-functions))
          (add-to-list 'comint-output-filter-functions
                       'bloop-prompt-filter)
          (add-to-list 'comint-output-filter-functions
                       'bloop-unrecognized-csi-filter)
          (add-to-list 'comint-output-filter-functions
                       'ansi-color-process-output)
          (pop-to-buffer-same-window (current-buffer))
          ;; (read-only-mode)
          (buffer-disable-undo)
          (if (comint-check-proc (current-buffer))
              (error "A bloop command is still running!")
            ;; TODO: Maybe save buffers?
            (cd root)
            (erase-buffer)
            (insert (concat root "$ " full-command))
            (newline 2)
            (comint-mode)
            (setq comint-prompt-regexp (bloop-prompt-regexp))
            (setq-local comint-use-prompt-regexp t)
            (setq-local comint-prompt-read-only t)
            ;; (compilation-shell-minor-mode)
            (comint-exec (current-buffer) buffer-name bloop-program-name nil (cons command args))
            (current-buffer)))
      (let ((compilation-buffer-name-function (lambda (_mode) buffer-name)))
        (cd root)
        (compile full-command)))))


;;;###autoload
(defun bloop-compile ()
  (interactive)
  (let* ((root (bloop-find-root (buffer-file-name)))
         (project (bloop-current-project root))
         (project-name (car project)))
    (bloop-exec nil root "compile" "--reporter" bloop-reporter project-name)))

;;;###autoload
(defun bloop-test ()
  (interactive)
  (let* ((root (bloop-find-root (buffer-file-name)))
         (project (bloop-current-project root))
         (project-name (car project)))
    (bloop-exec nil root "test" "--reporter" bloop-reporter project-name)))

;;;###autoload
(defun bloop-test-only ()
  (interactive)
  (let* ((root (bloop-find-root (buffer-file-name)))
         (project (bloop-current-project root))
         (project-name (car project))
         (target-test (concat "*" (replace-regexp-in-string ".scala" "" (car (last (split-string (buffer-file-name) "/")))))))
    (bloop-exec nil root "test" "--reporter" bloop-reporter "--only" target-test project-name)))

;;;###autoload
(defun bloop-clean ()
  (interactive)
  (let* ((root (bloop-find-root (buffer-file-name)))
         (project (bloop-current-project root))
         (project-name (car project)))
    (bloop-exec nil root "clean" project-name)))

(defun bloop-unrecognized-csi-filter (ignored)
  (let ((start-marker (if (and (markerp comint-last-output-start)
                               (eq (marker-buffer comint-last-output-start)
                                   (current-buffer))
                               (marker-position comint-last-output-start))
                          comint-last-output-start
                        (point-min-marker)))
        (end-marker (process-mark (get-buffer-process (current-buffer)))))
    (save-excursion
      (goto-char start-marker)
      (while (re-search-forward bloop-ansi-escape-re end-marker t)
        (replace-match "")))))

(defun bloop-prompt-filter (str)
  (when (> (length str) 0)
    (let ((start-marker (if (and (markerp comint-last-output-start)
                                 (eq (marker-buffer comint-last-output-start)
                                     (current-buffer))
                                 (marker-position comint-last-output-start))
                            comint-last-output-start
                          (point-min-marker)))
          (end-marker (process-mark (get-buffer-process (current-buffer))))
          (inhibit-read-only t))
      ;; Replace prompt
      (save-excursion
        (goto-char start-marker)
        (while (re-search-forward (bloop-prompt-regexp) end-marker t)
          (replace-match bloop-console-prompt t t)))
      ;; Replace indent
      (save-excursion
        (goto-char start-marker)
        (while (re-search-forward "^\\(  \\)[ ]*[^ ]" end-marker t)
          (replace-match bloop-console-prompt2 t t nil 1)))
      (save-excursion
        (goto-char start-marker)
        (when (re-search-forward "^   $" end-marker t)
          (replace-match bloop-console-prompt2 t t)))
      ;; Delete echoed content
      (save-excursion
        (goto-char end-marker)
        (beginning-of-line)
        (let* ((bound (min (point) start-marker))
               (end1 (point))
               (beg1 (re-search-backward (bloop-prompt-regexp) bound t))
               (end2 (point))
               (beg2 (re-search-backward (bloop-prompt-regexp) nil t)))
          (when (and beg1 beg2
                     (string=
                      (string-trim (buffer-substring-no-properties beg1 end1))
                      (string-trim (buffer-substring-no-properties beg2 end2))))
            (delete-region beg2 end2)))))))

;;;###autoload
(defun bloop-console ()
  (interactive)
  (let* ((root (bloop-find-root (buffer-file-name)))
         (project (bloop-current-project root))
         (project-name (car project))
         (default-directory root))
    (bloop-exec t root "console" project-name)))

;;;###autoload
(defun bloop-show-current-project ()
  (interactive)
  (let* ((root (bloop-find-root (buffer-file-name))))
    (message (format "%S %S" root (bloop-current-project root)))))

;;;###autoload
(define-minor-mode bloop-mode
  "Minor mode to run Bloop from Emacs"
  :lighter ""
  :keymap (make-sparse-keymap)
  :group 'bloop
  nil)

(define-key bloop-mode-map (kbd "C-c b c") 'bloop-compile)
(define-key bloop-mode-map (kbd "C-c b q") 'bloop-show-current-project)

(provide 'bloop)
;;; bloop.el ends here
