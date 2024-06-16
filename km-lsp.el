;;; km-lsp.el --- Miscellaneous LSP customizations -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/km-lsp
;; Version: 0.1.0
;; Keywords: convenience
;; Package-Requires: ((emacs "29.1"))
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
            (when-let ((file (expand-file-name
                              "node_modules/typescript/lib"
                              (km-lsp-project-root (project-current nil)))))
              (and (file-exists-p file)
                   file)))
          (when-let ((file (locate-dominating-file default-directory
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

(provide 'km-lsp)
;;; km-lsp.el ends here