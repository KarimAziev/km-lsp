;;; km-lsp.el --- Miscellaneous LSP customizations -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/km-lsp
;; Version: 0.1.0
;; Keywords: convenience
;; Package-Requires: ((emacs "29.1") (transient "0.7.7") (lsp-mode "9.0.0"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Miscellaneous LSP customizations for Emacs.

;;; Code:



(require 'transient)
(require 'lsp-mode)

(defvar-local km-lsp-custom-typescript-sdk-path nil)


(declare-function project-root "project")
(declare-function project-files "project")
(declare-function project-files "project")

(defun km-lsp-project-root (pr)
  "Return the root directory of the project PR."
  (if (fboundp 'project-root)
      (project-root pr)
    (with-no-warnings
      (car (project-roots pr)))))

(defun km-lsp-get-project-files ()
  "Return the list of files in the current project."
  (let ((pr
         (ignore-errors
           (project-current nil))))
    (when pr
      (let ((default-directory (km-lsp-project-root pr)))
        (project-files pr)))))

(defun km-lsp-custom-typescript-sdk ()
  "Return the TypeScript SDK path from the project.

Usage example:

\\=(with-eval-after-load \\='lsp-volar
    (when (fboundp \\='lsp-register-custom-settings)
      (lsp-register-custom-settings
       \\='((\" typescript.tsdk\" km-lsp-custom-typescript-sdk t)))))"
  (or
   km-lsp-custom-typescript-sdk-path
   (setq km-lsp-custom-typescript-sdk-path
         (or
          (ignore-errors
            (when-let* ((file (expand-file-name
                              "node_modules/typescript/lib"
                              (km-lsp-project-root (project-current nil)))))
              (and (file-exists-p file)
                   file)))
          (when-let* ((file (locate-dominating-file default-directory
                                                   "node_modules/typescript/lib")))
            (expand-file-name file "node_modules/typescript/lib"))
          (let ((files
                 (km-lsp-get-project-files))
                (typescript-lib))
            (while (and (not typescript-lib) files)
              (let* ((file (car files))
                     (ts-lib (and (string=
                                   "package.json"
                                   (file-name-nondirectory
                                    file))
                                  (expand-file-name
                                   "node_modules/typescript/lib"
                                   (file-name-parent-directory
                                    file)))))
                (when (and ts-lib
                           (file-exists-p ts-lib))
                  (setq typescript-lib ts-lib)))
              (setq files (cdr files)))
            typescript-lib)
          (when (fboundp
                 'lsp-volar-get-typescript-tsdk-path)
            (lsp-volar-get-typescript-tsdk-path))))))

;; lsp-booster

(defun km-lsp-booster--advice-json-parse (old-fn &rest args)
  "Parse JSON or execute bytecode if the following character is '#'.

Argument OLD-FN is the original JSON parsing function to be advised.

Remaining arguments ARGS are additional arguments passed to OLD-FN."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))

(defvar lsp-use-plists)

(defun km-lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Advise command CMD to use \"emacs-lsp-booster\" if conditions are met.

Argument OLD-FN is the original function to be advised.

Argument CMD is the command to be executed.

Optional argument TEST? is a boolean flag for testing purposes."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?) ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection)) ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

;;;###autoload
(defun km-lsp-booster-enable ()
  "Enable LSP booster if conditions are met and add necessary advices."
  (when (and (equal (getenv "LSP_USE_PLISTS") "true")
             (executable-find "emacs-lsp-booster")
             lsp-use-plists)
    (advice-add (if (progn
                      (require 'json nil t)
                      (fboundp 'json-parse-buffer))
                    'json-parse-buffer
                  'json-read)
                :around
                #'km-lsp-booster--advice-json-parse)
    (advice-add 'lsp-resolve-final-command :around
                #'km-lsp-booster--advice-final-command)))


(defun km-lsp-reload-workspaces ()
  "Restart all active LSP workspaces by shutting them down."
  (interactive)
  (let ((workspaces (lsp-workspaces)))
    (dolist (workspace workspaces)
      (let ((lsp--cur-workspace workspace))
        (lsp--warn "Restarting %s" (lsp--workspace-print lsp--cur-workspace))
        (lsp--shutdown-workspace t)))))



(defun km-lsp--make-toggle-description (label value &optional align)
  "Create a toggle description string with optional alignment.

Argument LABEL is a string used as the label for the toggle.

Argument VALUE is a boolean indicating the toggle state.

Optional argument ALIGN is an integer specifying the alignment position."
  (concat label
          (unless (string-suffix-p " " label)
            " ")
          (propertize " " 'display
                      (list 'space :align-to (or align 40)))
          (if value
              (concat "["
                      (propertize
                       "+"
                       'face
                       'success)
                      "]")
            "[ ]")))


;;;###autoload (autoload 'km-lsp-menu "km-lsp" nil t)
(transient-define-prefix km-lsp-menu ()
  "Command dispatcher for LSP related commands."
  ["LSP"
   ["Refactor"
    ("r" "Rename symbol" lsp-rename :inapt-if-not (lambda ()
                                                    (lsp-feature?
                                                     "textDocument/rename")))
    ("= =" "Format buffer" lsp-format-buffer)
    ("= r" "Format region" lsp-format-region)
    ("T f" "Toggle on type formatting" lsp-toggle-on-type-formatting
     :transient t)]
   ["Actions"
    ("a" "Code actions" lsp-execute-code-action)
    ("i" "Imports fix" lsp-organize-imports)
    ("S" "Toggle signature auto activate" lsp-toggle-signature-auto-activate
     :transient t)]]
  [["Navigation"
    ("f d" "Go to definition" lsp-find-definition :inapt-if-not
     (lambda ()
       (lsp-feature? "textDocument/definition")))
    ("f ." "Find references" lsp-find-references :inapt-if-not
     (lambda ()
       (lsp-feature?
        "textDocument/references")))
    ("f i" "Find implementations" lsp-find-implementation :inapt-if-not
     (lambda ()
       (lsp-feature?
        "textDocument/implementation")))
    ("f c" "Find declarations" lsp-find-declaration :inapt-if-not
     (lambda ()
       (lsp-feature?
        "textDocument/declaration")))
    ("f t" "Go to type declaration" lsp-find-type-definition  :inapt-if-not
     (lambda ()
       (lsp-feature? "textDocument/typeDefinition")))
    ("h r" "Highlight references" lsp-document-highlight)
    ("h s" "Highlight symbol" lsp-toggle-symbol-highlight :transient t)]]
  [["Session"
    ("v" "View logs" lsp-workspace-show-log)
    ("s" "Describe session" lsp-describe-session)
    ("R" "Restart server" lsp-workspace-restart)
    ("K" "Shutdown server" lsp-workspace-shutdown)]
   ["Workspace Folders"
    ("w b" "Remove blocklist" lsp-workspace-blocklist-remove)
    ("w a" "Add folders" lsp-workspace-folders-add)
    ("w r" "Remove folders" lsp-workspace-folders-remove)
    ("w o" "Add folders" lsp-workspace-folders-open)
    ("w D" "Disconnect" lsp-disconnect)]]
  ["Toggle features"
   ("t l"  lsp-lens-mode
    :transient t
    :description  (lambda ()
                    (km-lsp--make-toggle-description "Lenses "
                                                     (bound-and-true-p
                                                      lsp-lens-mode))))
   ("t h" lsp-headerline-breadcrumb-mode
    :transient t
    :description
    (lambda ()
      (km-lsp--make-toggle-description "Headerline breadcrumb"
                                       (bound-and-true-p
                                        lsp-headerline-breadcrumb-mode))))
   ("t c" lsp-modeline-code-actions-mode
    :transient t
    :description
    (lambda ()
      (km-lsp--make-toggle-description "Modeline code actions"
                                       (bound-and-true-p
                                        lsp-modeline-code-actions-mode))))
   ("t d" lsp-modeline-diagnostics-mode
    :transient t
    :description
    (lambda ()
      (km-lsp--make-toggle-description
       "Modeline diagnostics"
       (bound-and-true-p
        lsp-modeline-diagnostics-mode))))
   ("t t" lsp-toggle-trace-io
    :description  (lambda ()
                    (km-lsp--make-toggle-description
                     "Toggle logging "
                     (bound-and-true-p
                      lsp-log-io)))
    :transient t)])


(provide 'km-lsp)
;;; km-lsp.el ends here