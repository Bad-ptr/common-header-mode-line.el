;;; per-window-header-mode-line-source.el --- source of the per-window-header-mode-line.el.

;; Copyright (C) 2017 Constantin Kulikov
;;
;; Author: Constantin Kulikov (Bad_ptr) <zxnotdead@gmail.com>
;; Date: 2017/01/29 09:05:26
;; License: GPL either version 3 or any later version
;; URL: http://github.com/Bad-ptr/common-header-mode-line.el

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

;; Edit this file if you want to modify per-window-header-mode-line.el

(
 (:text
  ";;; per-window-$@-line.el --- per-window $@-line.

;; Copyright (C) 2017 Constantin Kulikov
;;
;; Author: Constantin Kulikov (Bad_ptr) <zxnotdead@gmail.com>
;; Date: ${date-time}$
;; License: GPL either version 3 or any later version
;; URL: http://github.com/Bad-ptr/common-$@-line.el

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

;; This file is autogenerated. Do not edit manually.
;; This file is the part of the common-$@-line package

;; Draws per-window $@-line.

;; To use it, first install common-$@-line package, then:

;; (require 'per-window-$@-line)
;; (per-window-$0-line-mode)
;; (per-window-$1-line-mode)

;; M-x customize-group RET per-window-$@-line RET
;; M-x customize-group RET per-window-$0-line RET
;; M-x customize-group RET per-window-$1-line RET

;;; Code:
")

 (require 'face-remap)

 (declare-function common-$@-line-add-delayed-update-function "ext:common-$@-line" (fun))
 (declare-function common-$@-line-rem-delayed-update-function "ext:common-$@-line" (fun))


 (defvar per-window-$*-line-mode nil)
 (defvar per-window-$@-line-mode nil)

 ;; (unless (boundp 'common-$@-line-mode)
 ;;   (require 'common-$@-line))


 (defgroup per-window-$@-line nil
   "Customize per-window-$@-line."
   :prefix "per-window-$@-line-"
   :group 'common-$@-line)

 (defcustom per-window-$@-line-ignore-buffer-functions (list #'minibufferp)
   "Ignore buffer(argument) if one of these functions return non nil."
   :group 'per-window-$@-line
   :type '(repeat function))


 (defgroup per-window-$*-line nil
   "Customize per-window-$*-lie."
   :prefix "per-window-$*-line-"
   :group 'per-window-$@-line)

 (defcustom per-window-$*-line-format-function
   #'per-window-$*-line--format-function
   "Function, takes window, returns $*-line-format for window."
   :group 'per-window-$*-line
   :type 'function
   :set (lambda (sym val)
          (custom-set-default sym val)
          (when per-window-$*-line-mode
            (per-window-$@-line-update-all-windows t))))

 (defvar-local per-window-$*-line--saved-emacs-format nil
   "Default format.")

 (defvar-local per-window-$*-line--face-remap-cookies nil
   "Per-buffer face-remap cookie.")

 (defface per-window-$*-line-active-face
   '((default :inherit $*-line :height 5))
   "Face to use for per-window $*-line when window is active."
   :group 'per-window-$*-line)

 (defface per-window-$*-line-inactive-face
   '((default :inherit mode-line-inactive :height 3))
   "Face to use for per-window $*-line when window is inactive."
   :group 'per-window-$*-line)

 (defun per-window-$*-line--format-function (win)
   "Default format function."
   ($eval
    (if (eq 'mode '$*)
        " "
      nil)))

 (defun per-window-$*-line--format (win)
   (funcall per-window-$*-line-format-function win))

 (defun per-window-$*-line--update-window (win &optional buf)
   (when per-window-$*-line-mode
     (unless (buffer-live-p buf)
       (setq buf (window-buffer win)))
     (with-current-buffer buf
       (unless per-window-$*-line--saved-emacs-format
         (setq-local per-window-$*-line--saved-emacs-format
                     (or (buffer-local-value '$*-line-format buf)
                         :nil)))
       ($eval
        (if (eq 'mode '$*)
            '(progn
               (unless per-window-$*-line--face-remap-cookies
                 (push (face-remap-add-relative
                        '$*-line-inactive 'per-window-$*-line-inactive-face)
                       per-window-$*-line--face-remap-cookies)
                 (push (face-remap-add-relative
                        '$*-line 'per-window-$*-line-active-face)
                       per-window-$*-line--face-remap-cookies)))
          '(progn
             (when per-window-$*-line--face-remap-cookies
               (face-remap-remove-relative
                per-window-$*-line--face-remap-cookies)
               (setq-local per-window-$*-line--face-remap-cookies nil))
             (setq-local per-window-$*-line--face-remap-cookies
                         (face-remap-add-relative '$*-line
                                                  (if (eq win (selected-window))
                                                      'per-window-$*-line-active-face
                                                    'per-window-$*-line-inactive-face))))))
       (setq-local $*-line-format
                   (per-window-$*-line--format win)))))

 (defun per-window-$@-line--update-window (win)
   (let ((buf (window-buffer win)))
     (unless (run-hook-with-args-until-success
              'per-window-$@-line-ignore-buffer-functions
              buf)
       ($subforms
        (per-window-$*-line--update-window win buf)))))

 (defun per-window-$@-line-update-all-windows (&optional all-frames)
   (interactive "P")
   (setq all-frames (not (null all-frames)))
   (save-excursion
     (let*
         ((start-win
           (if (minibuffer-window-active-p (selected-window))
               (minibuffer-selected-window)
             (selected-window)))
          (cwin (next-window start-win 0 all-frames)))
       (while (not (eq cwin start-win))
         (per-window-$@-line--update-window cwin)
         (setq cwin (next-window cwin 0 all-frames)))
       (per-window-$@-line--update-window start-win))))

 (defun per-window-$@-line-generic-hook-function (&rest args)
   (when (or per-window-$0-line-mode per-window-$1-line-mode)
     (per-window-$@-line-update-all-windows))
   t)

 (defun per-window-$*-line--activate ()
   (common-$@-line-add-delayed-update-function
    #'per-window-$@-line-generic-hook-function)
   (per-window-$@-line-update-all-windows t))

 (defun per-window-$*-line--deactivate ()
   (unless (or per-window-$0-line-mode per-window-$1-line-mode)
     (common-$@-line-rem-delayed-update-function
      #'per-window-$@-line-generic-hook-function))
   (dolist (buf (buffer-list))
     (with-current-buffer buf
       (when per-window-$*-line--face-remap-cookies
         ($eval
          (if (eq 'mode '$*)
              '(while per-window-$*-line--face-remap-cookies
                 (face-remap-remove-relative
                  (pop per-window-$*-line--face-remap-cookies)))
            '(progn
               (face-remap-remove-relative per-window-$*-line--face-remap-cookies)
               (setq-local per-window-$*-line--face-remap-cookies nil)))))
       (when per-window-$*-line--saved-emacs-format
         (if (eq :nil per-window-$*-line--saved-emacs-format)
             (setq-local $*-line-format nil)
           (setq-local $*-line-format per-window-$*-line--saved-emacs-format))
         (setq-local per-window-$*-line--saved-emacs-format nil))))
   ($subforms
    (progn
      ($eval
       (when (eq 'mode '$*)
         '(face-spec-recalc '$*-line-inactive nil)))
      (face-spec-recalc '$*-line nil))))


 (:autoload
  (define-minor-mode per-window-$*-line-mode
    "Toggle the `per-window-$*-line-mode'. If active
it manages the `$*-line' appearence in visible windows
by changing the buffer-local variable `$*-line-format'
of visible buffers."
    :require 'per-window-$@-line
    :group   'per-window-$*-line
    :init-value nil
    :global     t
    (if per-window-$*-line-mode
        (per-window-$*-line--activate)
      (per-window-$*-line--deactivate))))

 (:autoload
  (define-minor-mode per-window-$@-line-mode
    "`per-window-$0-line-mode' + `per-window-$1-line-mode'."
    :require 'per-window-$@-line
    :group   'per-window-$@-line
    :init-value nil
    :global     t
    (if per-window-$@-line-mode
        ($subforms
         (per-window-$*-line-mode 1))
      ($subforms
       (per-window-$*-line-mode -1)))))


 (provide 'per-window-$@-line)

 (:text
  "
;;; per-window-$@-line.el ends here")
 )
