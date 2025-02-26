;;; flymake-mdl.el --- An mdl Flymake backend  -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Micah Elliott
;; Copyright (c) 2025 Edd Wilder-James

;; Author: Edd Wilder-James
;; URL: https://github.com/ewilderj/flymake-mdl
;; Package-Version: 0

;; Original author
;; Author: Micah Elliott <mde@micahelliott.com>
;; URL: https://github.com/micahelliott/flymake-mdl

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
;;   (require 'flymake-markdownlint-cli2)
;;   (add-hook 'markdown-mode-hook 'flymake-markdownlint-cli2-setup)
;;
;; Derived largely from ruby example:
;; https://www.gnu.org/software/emacs/manual/html_node/flymake/An-annotated-example-backend.html

;;; Code:

(require 'cl-lib)

(defvar-local mdl--flymake-proc nil)


(message "loading flymake-markdownlint-cli2 package")

(defgroup flymake-markdownlint-cli2 nil
  "markdownlint-cli2 backend for Flymake."
  :prefix "flymake-markdownlint-cli2-"
  :group 'tools)

(defcustom flymake-markdownlint-cli2-program
  "markdownlint-cli2"
  "Name of the `markdownlint-cli2' executable."
  :type 'string)

(defcustom flymake-markdownlint-cli2-config-filename
  ".markdownlint-cli2.mjs"
  "File name of the linter config file"
  :type 'string)

(defcustom flymake-markdownlint-cli2-config
  nil
  "Full path of linter config file. If nil, searches the default directory and its parents for a file named `flymake-markdownlint-cli2-config-filename`"
  :type 'string)

(defun find-mdl-config (dir)
  (let ((config-file (expand-file-name flymake-markdownlint-cli2-config-filename dir)))
    (if (file-exists-p config-file)
        config-file
      (let ((parent-dir (file-name-directory (directory-file-name dir))))
        (if (equal dir parent-dir)
            nil
          (find-mdl-config parent-dir)))))
  )

(defun flymake-markdownlint-cli2 (report-fn &rest _args)
  ;; Not having the linter is a serious problem which should cause
  ;; the backend to disable itself, so an error is signaled.
  (unless (executable-find flymake-markdownlint-cli2-program)
    (error "Could not find '%s' executable" flymake-markdownlint-cli2-program))

  ;; If a live process launched in an earlier check was found, that
  ;; process is killed.  When that process's sentinel eventually runs,
  ;; it will notice its obsoletion, since it have since reset
  ;; `flymake-mdl-proc' to a different value
  (when (process-live-p mdl--flymake-proc) (kill-process mdl--flymake-proc))
  ;; Save the current buffer, the narrowing restriction, remove any narrowing restriction.

  ;; (let ((mdl--config-file
  ;;        (or flymake-mdl-config
  ;;            (find-mdl-config default-directory)))
  ;;       (mdl--args
  ;;        (if mdl--config-file
  ;;            (list "--config" mdl--config-file "-")
  ;;          (list "-")))
  ;;       (mdl--dir
  ;;        (if mdl--config-file
  ;;            (file-name-directory mdl--config-file)
  ;;          default-directory)))
  ;;   )

  (let ((source (current-buffer))
        (mdl--config-file
         (or flymake-markdownlint-cli2-config
             (find-mdl-config default-directory)))
        (mdl--args
         (if mdl--config-file
             (list "--config" mdl--config-file "-")
           (list "-")))
        (default-directory (if mdl--config-file
                               (file-name-directory mdl--config-file)
                             default-directory)))

    (save-restriction
      (widen)
      ;; Reset the `mdl--flymake-proc' process to a new process calling the linter

      (setq
       mdl--flymake-proc
       (make-process
        :name "flymake-markdownlint-cli2" :noquery t :connection-type 'pipe
        :buffer (generate-new-buffer " *flymake-markdownlint-cli2*") ; Make output go to a temporary buffer.
        :command (append (list flymake-markdownlint-cli2-program) mdl--args)
        :sentinel
        (lambda (proc _event)
          ;; Check that the process has indeed exited, as it might be simply suspended.
          (when (memq (process-status proc) '(exit signal))
            (unwind-protect
                ;; Only proceed if `proc' is the same as `markdownlint-cli2--flymake-proc',
                ;; which indicates that `proc' is not an obsolete process.
                (if (with-current-buffer source (eq proc mdl--flymake-proc))
                    (with-current-buffer (process-buffer proc)
                      ;; echo buffer working directory
                      ;; (message (expand-file-name default-directory))
                      ;; (message (buffer-string))
                      (goto-char (point-min))
                      ;; Parse the output buffer for diagnostic's messages and
                      ;; locations, collect them in a list of objects, and call `report-fn'.
                      (cl-loop
                       while (search-forward-regexp
                              "^\\(stdin\\):\\([0-9]+\\):?[0-9]* \\([A-Z]+[0-9]+/.*\\)$"
                              nil t)

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

      ;; Send the buffer contents to the process's stdin, followed by an EOF.
      (process-send-region mdl--flymake-proc (point-min) (point-max))
      (process-send-eof mdl--flymake-proc))))

;;;###autoload
(defun flymake-markdownlint-cli2-setup ()
  "Enable markdownlint-cli2 markdown flymake backend."
  (add-hook 'flymake-diagnostic-functions #'flymake-markdownlint-cli2 nil t))

(provide 'flymake-markdownlint-cli2)
;;; flymake-markdownlint-cli2.el ends here
