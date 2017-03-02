;;; generate-common-header-mode-line.el --- Generate .el from -source.el. -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Constantin Kulikov
;;
;; Author: Constantin Kulikov (Bad_ptr) <zxnotdead@gmail.com>
;; Version: 0.1
;; Package-Requires: ()
;; Date: 2017/01/29 09:05:26
;; License: GPL either version 3 or any later version
;; Keywords: code generation
;; X-URL: http://github.com/Bad-ptr/common-header-mode-line.el

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Generate .el from -source.el

;;; Code:

(require 'common-code-substitute)


(defvar common-header-mode-line-pkg-filename
  "common-header-mode-line-pkg.el")

(defvar common-header-mode-line-pkg-form
  (with-current-buffer
      (find-file-noselect
       common-header-mode-line-pkg-filename)
    (car (read-from-string (buffer-string)))))

(defvar common-header-mode-line-version
  (cl-caddr common-header-mode-line-pkg-form))

(defvaralias 'version 'common-header-mode-line-version)

(defvar date-time (format-time-string "%d/%m/%Y %H:%M"))

(defvar common-header-mode-line-input-filename
  "common-header-mode-line-source.el")
(defvar common-header-mode-line-output-filename
  (concat
   (substring
    common-header-mode-line-input-filename
    0 (cl-search "-source.el" common-header-mode-line-input-filename))
   ".el"))

(defvar common-header-mode-line-generated-code nil)
(setq common-header-mode-line-generated-code
      (common-code-substitute-1
       `((items . ("header" "mode")))
       (with-current-buffer
           (find-file-noselect
            common-header-mode-line-input-filename)
         (car (read-from-string (buffer-string))))))


(defun common-code-pp (forms)
  (mapconcat
   #'(lambda (form)
       (cl-typecase form
         (symbol (symbol-name form))
         (string form)
         (cons
          (cl-case (car form)
            (:text (common-code-pp (cdr form)))
            (:autoload (concat ";;;###autoload\n"
                               (common-code-pp (cdr form))))
            (progn (common-code-pp (cdr form)))
            (t (pp-to-string form))))
         (t (pp-to-string form))))
   forms "\n"))


(with-current-buffer
    (find-file-noselect
     common-header-mode-line-output-filename)
  (erase-buffer)
  (emacs-lisp-mode)
  (insert
   (common-code-pp
    (cdr common-header-mode-line-generated-code)))
  (indent-region (point-min) (point-max))
  (write-file
   (file-name-nondirectory
    common-header-mode-line-output-filename)))



(provide 'generate-common-header-mode-line)

;;; generate-common-header-mode-line.el ends here
