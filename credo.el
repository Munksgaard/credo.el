;;; credo.el --- Credo runner -*- lexical-binding: t -*-

;; Copyright (C) 2025 Philip Munksgaard.

;; Author: Philip Munksgaard <philip@munksgaard.me>
;; URL: https://github.com/Munksgaard/credo.el
;; Version: 0.1.0
;; Keywords: processes credo elixir
;; Package-Requires: ((s "1.11.0") (emacs "26.1") (transient "0.3.6") (project "0.9.8"))

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides commands to run Credo checks.

;;;; Credits

;; This package would not have been possible without the exunit.el package[1],
;; which this package borrows heavily from.
;;
;;  [1] https://github.com/ananthakumaran/exunit.el/

;;; Code:

(require 's)
(require 'ansi-color)
(require 'compile)
(require 'transient)
(require 'project)

;;; Private

(transient-define-infix credo-transient:--checks ()
  :description "Only include checks that match the given comma-seperated patterns"
  :class 'transient-option
  :shortarg "-c"
  :argument "--checks=")

(transient-define-infix credo-transient:--checks-with-tag ()
  :description "Only include checks that match the given comma-separated tags"
  :class 'transient-option
  :multi-value 'repeat
  :shortarg "-t"
  :argument "--checks-with-tag=")

(transient-define-infix credo-transient:--checks-without-tag ()
  :description "Ignore checks that match the given comma-separated tags"
  :class 'transient-option
  :multi-value 'repeat
  :shortarg "-T"
  :argument "--checks-without-tag=")

(transient-define-infix credo-transient:--config-file ()
  :description "Use the given config file as Credo's config"
  :class 'transient-option
  :shortarg "-f"
  :argument "--config-file")

(transient-define-infix credo-transient:--config-name ()
  :description "Use the given config instead of \"default\""
  :class 'transient-option
  :shortarg "-n"
  :argument "--config-name")

(transient-define-infix credo-transient:--enable-disabled-checks ()
  :description "Re-enable disabled checks that match the given comma-seperated patterns"
  :class 'transient-option
  :shortarg "-d"
  :argument "--enable-disabled-checks=")

(transient-define-infix credo-transient:--files-included ()
  :description "Only include these comma-separated files (accepts globs)"
  :class 'transient-option
  :multi-value 'repeat
  :shortarg "-i"
  :argument "--files-included=")

(transient-define-infix credo-transient:--files-excluded ()
  :description "Exclude these comma-separated files (accepts globs)"
  :class 'transient-option
  :multi-value 'repeat
  :shortarg "-i"
  :argument "--files-excluded=")

(transient-define-infix credo-transient:--ignore-checks ()
  :description "Ignore checks that match the given comma-seperated patterns"
  :class 'transient-option
  :shortarg "-i"
  :argument "--ignore-checks=")

(transient-define-infix credo-transient:--min-priority ()
  :description "Minimum priority to show issues (higher,high,normal,low,ignore or number)"
  :class 'transient-option
  :shortarg "-m"
  :argument "--min-priority="
  :choices )

(transient-define-prefix credo-transient ()
  "Credo"
  ["Arguments"
   ("-a" "all" "--all")
   ("-A" "all priorities" "--all-priorities")
   (credo-transient:--checks :level 5)
   (credo-transient:--checks-with-tag :level 5)
   (credo-transient:--checks-without-tag :level 5)
   (credo-transient:--config-file :level 5)
   (credo-transient:--config-name :level 5 )
   (credo-transient:--enable-disabled-checks :level 5)
   (credo-transient:--files-included :level 5)
   (credo-transient:--files-excluded :level 5)
   (credo-transient:--ignore-checks :level 5)
   (credo-transient:--min-priority :level 5)]
  ["Actions"
   ("s" "suggest" credo-suggest)
   ("l" "list" credo-list :level 5)
   ("r" "rerun" credo-rerun)])

(defcustom credo-suggest-command '("mix" "credo" "suggest")
  "A command used to run credo.  Represented as list or function."
  :type '(choice (repeat string) function)
  :group 'credo
  :risky t)

(defcustom credo-list-command '("mix" "credo" "list")
  "A command used to run credo.  Represented as list or function."
  :type '(choice (repeat string) function)
  :group 'credo
  :risky t)

(defcustom credo-environment '()
  "List of environment variables used when running credo.
Each element should be a string of the form ENVVARNAME=VALUE."
  :type '(repeat (string :tag "ENVVARNAME=VALUE"))
  :group 'credo)

(defcustom credo-key-command-prefix  (kbd "C-c C-,")
  "The prefix for all credo related key commands."
  :type 'string
  :group 'credo)

(defvar credo-last-directory nil
  "Directory the last credo command ran in.")

(defvar credo-last-arguments nil
  "Arguments passed to `credo-do-compile' at the last invocation.")

(defvar-local credo-project-root nil)

(defun credo-project-root ()
  "Return the current project root.

This value is cached in a buffer local to avoid filesytem access
on every call."
  (or
   credo-project-root
   (let ((root (locate-dominating-file default-directory "mix.exs")))
     (unless root
       (error "Couldn't locate project root folder.  Make sure the current file is inside a project"))
     (setq credo-project-root (expand-file-name root)))))

(defun credo-colorize-compilation-buffer ()
  "Colorize the credo compilation buffer."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))

(defvar credo-compilation-error-regexp-alist-alist
  '((elixir-error " +\\(\\(?:([0-9A-Za-z_-]*) \\)?[0-9A-Za-z@_./:-]+\\.\\(?:ex\\|exs\\|erl\\)\\):\\([0-9]+\\):?" 1 2 nil 2 1)))

(defvar credo-compilation-error-regexp-alist
  (mapcar 'car credo-compilation-error-regexp-alist-alist))

(define-compilation-mode credo-compilation-mode "Credo Compilation"
  "Compilation mode for Credo output."
  (setq compilation-buffer-name-function
        (lambda (compilation-mode)
          (if (project-current)
              (concat "*" (downcase compilation-mode) "-" (project-name (project-current)) "*")
            (compilation--default-buffer-name compilation-mode))))
  (add-hook 'compilation-filter-hook 'credo-colorize-compilation-buffer nil t))

(defun credo-do-compile (args)
  "Run compile and save the ARGS for future invocation."
  (setq credo-last-directory default-directory
        credo-last-arguments args)

  (compile args 'credo-compilation-mode))

(defun credo-build-command (command-list-or-func args)
  "Combines credo-suggest-command with arguments.

To get a string representation of a command to pass to a compilation phase."
  (let ((command (if (functionp command-list-or-func)
                     (funcall command-list-or-func args)
                   (append command-list-or-func args))))
    (s-join " " command)))

(defun credo-compile (command &optional directory)
  "Run credo with the given ARGS."
  (let* ((default-directory (or directory (credo-project-root)))
         (compilation-environment credo-environment)
         (args (if-let (infixes (transient-args 'credo-transient))
                   infixes
                 '())))
    (credo-do-compile (credo-build-command command args))))

;;; Public

(define-minor-mode credo-mode
  "Minor mode for Credo runner."
  :lighter " Credo" :keymap `((,credo-key-command-prefix . credo-transient)))

;;;###autoload
(defun credo-rerun ()
  "Re-run the last credo invocation."
  (interactive)
  (if (not credo-last-directory)
      (error "No previous check")
    (let ((default-directory credo-last-directory))
      (credo-do-compile credo-last-arguments))))

;;;###autoload
(defun credo-suggest ()
  "Run credo suggest in the current project."
  (interactive)
  (credo-compile credo-suggest-command))

;;;###autoload
(defun credo-list ()
  "Run credo list in the current project."
  (interactive)
  (credo-compile credo-list-command))

(provide 'credo)

;;; credo.el ends here
