# common-header-mode-line  

## Copyright  

Copyright (C) 2017 Constantin Kulikov  

Author: Constantin Kulikov (Bad_ptr) <zxnotdead@gmail.com>  
Date: 2017/02/12 11:14:08  
License: GPL either version 3 or any later version  
Keywords: emacs, mode-line, header-line, convenience, frames, windows  
X-URL: http://github.com/Bad-ptr/common-header-mode-line.el  

## Intro  

Draws per-frame mode-line and/or header-line and allow to customize per-window header/mode-line.  

[![emacs 24 -Q](screenshots/emacs24Q_th.jpg)](screenshots/emacs24Q.jpg)


## Installation  

```shell

    git clone git@github.com:Bad-ptr/common-header-mode-line.el.git
    cd common-header-mode-line.el
    make package

```

This will create a tar file in the `common-header-mode-line.el/pkg/` directory.  
Or you can download a [release](https://github.com/Bad-ptr/common-header-mode-line.el/releases).  

Then start emacs and `M-x package-install-file RET path/to/common-header-mode-line-{version}.tar RET`.  

Put the following into your emacs config:

```elisp

    (with-eval-after-load "common-header-mode-line-autoloads"
      (common-mode-line-mode)
      (common-header-line-mode))

```

## How to use  

```elisp

M-x customize-group RET common-header-mode-line RET

M-x customize-group RET common-mode-line RET

M-x customize-group RET common-header-line RET

M-x customize-group RET per-window-header-mode-line RET

M-x customize-group RET per-window-mode-line RET

M-x customize-group RET per-window-header-line RET

M-x customize-group RET per-frame-header-mode-line RET

M-x customize-group RET per-frame-mode-line RET

M-x customize-group RET per-frame-header-line RET

```

Activating the `common-header-mode-line-mode` is equivalent to activating 
`common-header-line-mode` and `common-mode-line-mode`  which is equivalent to 
activating `per-frame-header-mode-line-mode` and `per-window-header-mode-line-mode` 
which is equivalent to activating `per-frame-header-line-mode`, `per-frame-mode-line-mode`, 
`per-window-header-line-mode` and `per-window-mode-line-mode`.  

```elisp

    (with-eval-after-load "common-header-mode-line-autoloads"
      (add-hook
       'after-init-hook
       #'(lambda ()
           (common-header-mode-line-mode 1)

           (let ((def-height (face-attribute 'default :height)))
             (set-face-attribute
              'per-window-header-line-active-face nil
              :height (ceiling (* 0.9 def-height)))

             (set-face-attribute
              'per-window-header-line-inactive-face nil
              :height (floor (* 0.7 def-height))))

           ;; (set-face-background
           ;;  'per-window-header-line-active-face
           ;;  (face-background 'mode-line))

           ;; (set-face-background
           ;;  'per-frame-header-line-inactive-face
           ;;  (face-background 'mode-line-inactive))

           (setq per-window-header-line-format-function
                 #'(lambda (win)
                     (let ((frmt '("%e" mode-line-front-space mode-line-mule-info mode-line-client
                                   mode-line-modified mode-line-remote mode-line-frame-identification
                                   mode-line-buffer-identification "   " mode-line-position
                                   (vc-mode vc-mode)
                                   "  " mode-line-misc-info mode-line-end-spaces)))
                       ;; (unless (eq :nil per-window-header-line--saved-emacs-format)
                       ;;   (setq frmt (cons
                       ;;               per-window-header-line--saved-emacs-format
                       ;;               frmt)))
                       frmt)))

           (setq per-frame-mode-line-update-display-function
                 #'(lambda (display)
                     (let ((buf (cdr (assq 'buf display))))
                       (with-current-buffer buf
                         (setq-local buffer-read-only nil)
                         (erase-buffer)
                         (let*
                             ((mode-l-str
                               (format-mode-line
                                '(" " (eldoc-mode-line-string (" " eldoc-mode-line-string " "))
                                  mode-line-modified mode-line-remote " "
                                  mode-line-buffer-identification " " (vc-mode vc-mode) " "
                                  mode-line-modes mode-line-misc-info mode-line-end-spaces)
                                'per-frame-mode-line-face per-frame-header-mode-line--selected-window)))
                           (insert mode-l-str))
                         (setq-local mode-line-format nil)
                         (setq-local header-line-format nil)
                         (goto-char (point-min))
                         (setq-local buffer-read-only t))))))))

```

Result of the above code:  
[![emacs 26 custom](screenshots/emacs26custom_th.jpg)](screenshots/emacs26custom.jpg)


## How to contribute  

Edit a `*-source.el`, then run `make package`, install package, 
then start emacs, activate the `common-header-mode-line-mode`, 
test how it works, if works well commit and push.  

### Cryptic writings

When editing `*-source.el` using `$*` in symbol or string will 
cause the current top level form be repeated two times 
-- first time the `$*` will be replaced by `header`, second time by `mode`.  
The `$@` in symbol or string will be replaced by `header-mode`.  
`$0` -- to `header`, `$1` -- `mode`.  
`($eval expr)` is replaced by the result of evaluation of the expr.  
`($subloop expr)` limit bounds of the `$*` expansion.  
