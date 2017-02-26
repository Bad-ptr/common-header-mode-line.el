;;; common-code-substitute.el --- Perform substitution on code atoms. -*- lexical-binding: t -*-

;; Copyright (C) 2017 Constantin Kulikov
;;
;; Author: Constantin Kulikov (Bad_ptr) <zxnotdead@gmail.com>
;; Version: 0.2
;; Package-Requires: ()
;; Date: 2017/01/27 15:26:17
;; License: GPL either version 3 or any later version
;; Keywords: mode-line, header-line, convenience, frames, windows, ui
;; X-URL: https://github.com/Bad-ptr/common-header-mode-line.el

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

;; Perform substitution on code atoms.


;;; Code:


(unless (fboundp 'alist-get)
  (error "`alist-get' is not defined. Update your emacs."))

(require 'cl-lib)


(defun common-code-splice-list (input &optional splice-sym)
  (unless splice-sym (setq splice-sym '$splice))
  (let (ret)
    (mapc #'(lambda (it)
              (if (and it (listp it) (listp (cdr it))
                       (eq splice-sym (car it)))
                  (mapc #'(lambda (it)
                            (push it ret))
                        (cdr it))
                (push it ret)))
          input)
    (nreverse ret)))

(defun common-code-in-out-stack-map (in-out-stack fun)
  (let ((ret in-out-stack))
    (while (setq in-out-stack
                 (funcall fun in-out-stack))
      (setq ret in-out-stack))
    ret))

(cl-macrolet
    ((push-stack (place &rest values)
                 `(push ,(cons 'list values) ,place))
     (pick-stack (place)
                 `(car ,place))
     (pop-stack  (place)
                 `(pop ,place)))

  (defun common-code-in-out-stack--map (tree fun)
    (common-code-in-out-stack-map
     (cons (list (list tree 'ret 0)) nil)
     #'(lambda (in-out-stack)
         (cl-destructuring-bind (in-stack . out-stack)
             in-out-stack
           (when in-stack
             (funcall fun (pop-stack in-stack)
                      in-stack out-stack))))))

  (defun common-code-in-out-stack-tree--map
      (tree fun &optional per-item-fun splice-sym)
    (common-code-in-out-stack--map
     tree
     #'(lambda (in-cur in-stack out-stack)
         (cl-destructuring-bind
             (in-item in-part in-level &rest _rest)
             in-cur

           (when per-item-fun
             (setq in-item (funcall per-item-fun in-item))
             (setcar in-cur in-item))

           (while (and out-stack
                       (< in-level (cadar out-stack)))
             (pop-stack out-stack))

           (cl-typecase in-item
             (cons
              (setq in-item
                    (common-code-splice-list in-item splice-sym))
              (cl-destructuring-bind
                  (out-item out-level &rest _rest)
                  (or (pick-stack out-stack)
                      (list nil 0))
                (let ((newcons (cons nil nil)))
                  (cl-case in-part
                    (car
                     (setcar out-item newcons))
                    (cdr
                     (setcdr out-item newcons))
                    (t))
                  (push-stack out-stack newcons (1+ out-level))))
              (push-stack in-stack (cdr in-item) 'cdr (1+ in-level))
              (push-stack in-stack (car in-item) 'car (1+ in-level)))
             (t
              (let ((ret (funcall fun in-item)))
                (if out-stack
                    (cl-destructuring-bind
                        (out-item out-level &rest _rest)
                        (pop-stack out-stack)
                      (cl-case in-part
                        (car
                         (setcar out-item ret)
                         (push-stack out-stack out-item out-level))
                        (cdr
                         (setcdr out-item ret)
                         (unless out-stack
                           (push-stack out-stack out-item out-level)))
                        (t
                         (setq out-item ret)
                         (push-stack out-stack out-item out-level))))
                  (push-stack out-stack ret 0)))))
           (cons in-stack out-stack)))))

  (defun common-code-tree-map (tree fun &optional per-item-fun splice-sym)
    (let
        ((ret
          (common-code-in-out-stack-tree--map
           tree fun per-item-fun splice-sym)))
      (while (cdr ret)
        (pop-stack ret))
      (caar ret))))


(defun common-code-substitute-str (state str)
  (let ((substitutors (alist-get 'str-substitutors state
                                 common-code-default-str-substitutors)))
    (cl-reduce
     #'(lambda (str funcons)
         (funcall (cdr funcons) state str))
     substitutors :initial-value str)))

(defun common-code-substitute-sym (state sym)
  (let ((substf (alist-get
                 sym
                 (alist-get 'sym-substitutors state
                            common-code-default-sym-substitutors))))
    (if substf
        (funcall substf state sym)
      (let ((new-sym-name
             (common-code-substitute-str
              state (symbol-name sym))))
        (intern new-sym-name)))))

(defun common-code-substitute-atom (state atom)
  (cl-typecase atom
    (symbol
     (funcall (alist-get 'subst-sym state
                         #'common-code-substitute-sym)
              state atom))
    (string
     (funcall (alist-get 'subst-str state
                         #'common-code-substitute-str)
              state atom))
    (t atom)))

(defun common-code-substitute-cons (state cons)
  (let ((ret cons))
    (cond
     ((let* ((kar (car ret))
             (kar-handler
              (alist-get
               kar (alist-get 'cons-substitutors
                              state
                              common-code-default-cons-substitutors))))
        (when kar-handler
          (setq ret
                (funcall kar-handler state ret))
          t))
      t)
     ((let ((sconsf (alist-get 'subst-consf state
                               #'common-code-substitute-cons))
            (satomf (alist-get 'subst-atomf state
                               #'common-code-substitute-atom))
            (kar (car ret))
            (kdr (cdr ret)))
        (setq kdr (funcall (if (consp kdr) sconsf satomf)
                           state kdr))
        (setq kar (funcall (if (consp kar) sconsf satomf)
                           state kar))
        (setq ret (cons kar kdr)))
      t)
     (t t))
    ret))

(defun common-code-substitute-tree (state tree)
  (let ((sconsf (alist-get 'subst-consf state
                           #'common-code-substitute-cons))
        (satomf (alist-get 'subst-atomf state
                           #'common-code-substitute-atom)))
    (cl-typecase tree
      (cons (funcall sconsf state tree))
      (t (funcall satomf state tree)))))

(defun common-code-substitute-form (state form)
  (let (ret)
    (let ((i-tems (alist-get 'items state))
          (running t)
          temp)
      (setf (alist-get 'n state 0) 0)
      (setf (alist-get 'form-loop state) nil)
      (while (and i-tems running)
        (setq temp
              (funcall
               (alist-get 'subst-treef state
                          #'common-code-substitute-tree)
               state form))
        (when temp
          (push temp ret))
        (if (alist-get 'form-loop state)
            (progn
              (cl-incf (alist-get 'n state 0))
              (setq i-tems (cdr i-tems)))
          (setq running nil))))
    (if (cdr ret)
        (cons (alist-get 'block-type state 'progn)
              ret)
      (car ret))))

(defun common-code-substitute-forms (state forms)
  (cons
   (alist-get 'block-type state 'progn)
   (mapcar (apply-partially
            (alist-get 'subst-formf state
                       #'common-code-substitute-form)
            state)
           forms)))

(defun common-code-substitute-1
    (state forms)
  "Substitute symbols and strings in `BODY'.
The `STATE' is an alist."
  (list 'quote
        (funcall
         (alist-get 'subst-formsf state
                    #'common-code-substitute-forms)
         state forms)))

(defun common-code-substitute
    (state &rest body)
  "Substitute symbols and strings in `BODY'.
The `STATE' is an alist."
  (common-code-substitute-1 state body))




(defmacro common-code-def-alist-key-fun (prefix keys suffix)
  (let ((prefix-sym (cl-gensym "prefix-"))
        (suffix-sym (cl-gensym "suffix-")))
    `(let ((,prefix-sym ,prefix)
           (,suffix-sym ,suffix))
       (mapcar
        #'(lambda (key)
            (cons key
                  (intern
                   (concat
                    ,prefix-sym
                    (symbol-name key)
                    ,suffix-sym))))
        ,keys))))


(defun common-code-${}$-substitute-str (state str)
  (while (string-match "^.*\\(${.*}\\$\\).*$" str)
    (let ((code-str (match-string 1 str))
          cread (start 0) code-to-eval fun)
      (setq code-str
            (string-remove-prefix
             "${" (string-remove-suffix "}$" code-str)))
      (while (cl-destructuring-bind (code . end)
                 (condition-case nil
                     (read-from-string code-str start)
                   (error (list nil)))
               (when code
                 (setq start end)
                 (push code code-to-eval)
                 t)))
      (setq code-to-eval (nreverse code-to-eval)
            fun (eval `(lambda (state code) ,@code-to-eval)))
      (setf (alist-get 'type state) '${}$)
      (setq str (concat
                 (substring str 0 (match-beginning 1))
                 (format "%s" (funcall fun state code-to-eval))
                 (substring str (match-end 1))))))
  str)

(defun common-code-$@-substitute-str (state str)
  (while (string-match "^.*\\($@\\).*$" str)
    (setf (alist-get 'type state) '$@)
    (setq str (concat
               (substring str 0 (match-beginning 1))
               (mapconcat #'identity (alist-get 'items state)
                          (alist-get 'items-sep state "-"))
               (substring str (match-end 1)))))
  str)

(defun common-code-$n-substitute-str (state str)
  (while (string-match "^.*\\($[0-9]+\\).*$" str)
    (let (($n (nth (string-to-number
                    (substring (match-string 1 str) 1))
                   (alist-get 'items state))))
      (setf (alist-get 'type state) '$n)
      (setq str (concat
                 (substring str 0 (match-beginning 1))
                 $n
                 (substring str (match-end 1))))))
  str)

(defun common-code-$*-substitute-str (state str)
  (while (string-match "^.*\\($\\\*\\).*$" str)
    (setf (alist-get 'type state) '$*)
    (setf (alist-get 'form-loop state) t)
    (setq str (concat
               (substring str 0 (match-beginning 1))
               (nth (alist-get 'n state 0)
                    (alist-get 'items state))
               (substring str (match-end 1)))))
  str)


(defvar common-code-default-str-substitutors
  (common-code-def-alist-key-fun
   "common-code-"
   '(${}$ $@ $n $*)
   "-substitute-str"))


(defun common-code-$@--substitute-sym (state sym)
  (mapcar #'identity (alist-get 'items state)))

(defun common-code-$@_-substitute-sym (state sym)
  (mapcar #'intern (alist-get 'items state)))

(defvar common-code-default-sym-substitutors
  (common-code-def-alist-key-fun
   "common-code-"
   '($@- $@_)
   "-substitute-sym"))


(defun common-code-$eval-no-subst-substitute-cons (state cons)
  (let ((fu (cdr cons)))
    (setq fu (eval `(lambda (state cons) ,@fu)))
    (funcall fu state cons)))

(defun common-code-$eval-substitute-cons (state cons)
  (let ((kdr (cdr cons)))
    (setq kdr (common-code-substitute-tree state kdr))
    (common-code-$eval-no-subst-substitute-cons state (cons (car cons) kdr))))

(defun common-code-$with-state-substitute-cons (state cons)
  (setq cons (cdr cons))
  (let ((old-state state))
    (common-code-substitute-tree (car cons) (cdr cons))))

(defun common-code-$with-derived-state-substitute-cons (state cons)
  (setq cons (cdr cons))
  (let ((new-state (copy-alist state))
        (mod (car cons)))
    (dolist (key (mapcar #'car mod))
      (setq new-state (delq (assq key new-state) new-state))
      (push (assq key mod) new-state))
    (common-code-$with-state-substitute-cons
     state
     (cons '$with-state (cons new-state (cdr cons))))))

(defun common-code-$_subforms-substitute-cons (state cons)
  (common-code-substitute-forms state (cdr cons)))

(defun common-code-$subloop-substitute-cons (state cons)
  (setq cons (cdr cons))
  (common-code-$with-derived-state-substitute-cons
   state (cons '$with-derived-state (cons (list) (cons '$_subforms cons)))))

(defvar common-code-default-cons-substitutors
  (common-code-def-alist-key-fun
   "common-code-"
   '($eval-no-subst $eval $with-state $with-derived-state
                    $subloop $_subforms)
   "-substitute-cons"))


;; (common-code-substitute
;;  ((items . ("header" "mode")))
;;  (defun common-$*-line-dfg-$@-ghj ()
;;    (1+ common-$*-line-def)
;;    (1+ common-$0%$1-line-def)
;;    (funcall common-$@-line-fu))
;;  (defun test ()
;;    (funcall common-$@-line-fu common-$*-line-test))
;;  (defun test-${\(1+\ 1\)}$-2 ()
;;    (1+ 1))
;;  (defun test1 ()
;;    (funcall common-$@-line-fu common--line-test))
;;  (:autoload
;;   (defun common-$@-line-foo ()
;;     (1+ common-$*-line-fa))))

(provide 'common-code-substitute)

;;; common-code-substitute.el ends here
