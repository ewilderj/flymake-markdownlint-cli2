;;; flymake-mdl.el --- An mdl Flymake backend  -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Micah Elliott

;; ORIGINAL
;; Author: Micah Elliott <mde@micahelliott.com>
;; URL: https://github.com/micahelliott/flymake-mdl

;; This fork
;; Author: Edd Wilder-James
;; URL: https://github.com/ewilderj/flymake-mdl
;; Package-Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage:
;;   (require 'flymake-mdl)
;;   (add-hook 'sql-mode-hook 'flymake-mdl-setup)
;;
;; Derived largely from ruby example:
;; https://www.gnu.org/software/emacs/manual/html_node/flymake/An-annotated-example-backend.html

;;; Code:

(require 'cl-lib)

(defvar-local mdl--flymake-proc nil)

(message "loading flymake-mdl package")

(defgroup flymake-mdl nil
  "MDL markdown backend for Flymake."
  :prefix "flymake-mdl-"
  :group 'tools)

(defcustom flymake-mdl-program
  "markdownlint-cli2"
  "Name of to the `mdlint' executable."
  ;; Alternatives are: hugslint (for hugsql preprocessing), or a script of your own.
  :type 'string)

(defcustom flymake-mdl-config
  nil
  "Path to linter config file. If nil, will search the default directory and its parents for a file named '.markdownlint-cli2.mjs'"
  :type 'string)


;; recursively look for the file ".markdownlint-cli2.mjs" in the
;; current directory and then its parents
(defun find-mdl-config (dir)
  (let ((config-file (expand-file-name ".markdownlint-cli2.mjs" dir)))
    (if (file-exists-p config-file)
        config-file
      (let ((parent-dir (file-name-directory (directory-file-name dir))))
        (if (equal dir parent-dir)
            nil
          (find-mdl-config parent-dir)))))
  )

(defun flymake-mdl (report-fn &rest _args)
  ;; (message "running eee flymake-mdl")
  ;; Not having an interpreter is a serious problem which should cause
  ;; the backend to disable itself, so an error is signaled.
  (unless (executable-find flymake-mdl-program)
    (error "Could not find '%s' executable" flymake-mdl-program))

  (setq mdl--config-file
        (or flymake-mdl-config
            (find-mdl-config default-directory))
        )
  (message "config file: %s" mdl--config-file)

  ;; create arguments for linter. if there is a config file, use it
  ;; by adding "--config" and the path to the config file, otherwise
  ;; just use "-" to read from stdin
  (setq mdl--args
        (if mdl--config-file
            (list "--config" mdl--config-file "-")
          (list "-")))

  (setq mdl--dir
        (if mdl--config-file
            (file-name-directory mdl--config-file)
          default-directory))

  ;; (unless (executable-find "ruby") (error "Cannot find a suitable ruby 2"))
  ;; If a live process launched in an earlier check was found, that
  ;; process is killed.  When that process's sentinel eventually runs,
  ;; it will notice its obsoletion, since it have since reset
  ;; `flymake-mdl-proc' to a different value
  (when (process-live-p mdl--flymake-proc) (kill-process mdl--flymake-proc))
  ;; Save the current buffer, the narrowing restriction, remove any narrowing restriction.

  (let ((source (current-buffer)) (default-directory mdl--dir))
    (save-restriction
      (widen)
      ;; Reset the `mdl--flymake-proc' process to a new process calling the ruby tool.

      (setq
       mdl--flymake-proc
       (make-process
        :name "flymake-mdl" :noquery t :connection-type 'pipe
        :buffer (generate-new-buffer " *flymake-mdl*") ; Make output go to a temporary buffer.
        ;; :command '("ruby" "-w" "-c")
        ;; :command '("hugslint")
        ;; append computed mdl--args to the command
        :command (append (list flymake-mdl-program) mdl--args)
        :sentinel
        (lambda (proc _event)
          ;; Check that the process has indeed exited, as it might be simply suspended.
          (when (memq (process-status proc) '(exit signal))
            (unwind-protect
                ;; Only proceed if `proc' is the same as `mdl--flymake-proc', which indicates that `proc' is not an obsolete process.
                (if (with-current-buffer source (eq proc mdl--flymake-proc))
                    (with-current-buffer (process-buffer proc)
                      ;; echo buffer working directory
                      ;; (message (expand-file-name default-directory))
                      ;; (message (buffer-string))
                      (goto-char (point-min))
                      ;; Parse the output buffer for diagnostic's messages and locations, collect them in a list of objects, and call `report-fn'.
                      (cl-loop
                       ;; (stdin):9: MD012 Multiple consecutive blank lines
                       ;; README.md:12: MD032 Lists should be surrounded by blank lines
                       ;; do (message "searching")
                       while (search-forward-regexp
                              ;; "^.*:\\([0-9]+\\) \\(MD[0-9]+\\) \\(.*\\)$"
                              "^\\(stdin\\):\\([0-9]+\\):?[0-9]* \\([A-Z]+[0-9]+/.*\\)$"
                              nil t)
                       ;; "^\\(?:.*.rb\\|-\\):\\([0-9]+\\): \\(.*\\)$"
                       ;; do (message "found match %s" (match-string 0))
                       for msg = (match-string 3)
                       for (beg . end) = (flymake-diag-region source (string-to-number (match-string 2)))
                       ;; for type = (if (string-match "^warning" msg) :warning :error)
                       for type = :warning
                       when (and beg end)
                       collect (flymake-make-diagnostic source beg end type msg)
                       into diags
                       finally (funcall report-fn diags)))
                  (flymake-log :warning "Canceling obsolete check %s" proc))
              ;; Cleanup the temporary buffer used to hold the check's output.
              (kill-buffer (process-buffer proc)))))))
      ;; Send the buffer contents to the process's stdnin, followed by an EOF.
      (process-send-region mdl--flymake-proc (point-min) (point-max))
      (process-send-eof mdl--flymake-proc))))

(defun mdl-setup-flymake-backend ()
  "Add mdl to diagnostics."
  (add-hook 'flymake-diagnostic-functions 'flymake-mdl nil t))

;; (add-hook 'sql-mode-hook 'mdl-setup-flymake-backend)

;;;###autoload
(defun flymake-mdl-setup ()
  "Enable mdl markdown flymake backend."
  (add-hook 'flymake-diagnostic-functions #'flymake-mdl nil t))

(provide 'flymake-mdl)
;;; flymake-mdl.el ends here
