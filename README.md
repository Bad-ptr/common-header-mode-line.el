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

Put `common-header-mode-line.el` into a directory that in your emacs `load-path` 
or do `M-x package-install-file RET common-header-mode-line.el RET`, 
add the following to your config:  

```elisp

    (require 'common-header-mode-line)
    (common-mode-line-mode)
    (common-header-line-mode)

```

## How to use  

`M-x customize-group RET common-header-mode-line RET`  

`M-x customize-group RET common-mode-line RET`  

`M-x customize-group RET common-header-line RET`  


```elisp

    (with-eval-after-load "common-header-mode-line-autoloads"
      (add-hook
       'after-init-hook
       #'(lambda ()
           (common-mode-line-mode)


           (when (fboundp 'tabbar-mode)
             (common-header-line-mode)
             (face-spec-set
              'common-header-line-face
              '((default :inherit default)))
             (set-face-background 'common-header-line-face "dim gray"))

           (face-spec-reset-face 'common-header-line-active-window-header-line-face)

           (face-spec-set
            'common-header-line-active-window-header-line-face
            '((default :inherit default :height 0.9)))
           (set-face-background
            'common-header-line-active-window-header-line-face
            (face-background 'mode-line))


           (face-spec-set 'common-header-line-inactive-window-header-line-face
                          '((default :inherit default :height 0.8)))
           (set-face-background
            'common-header-line-inactive-window-header-line-face
            (face-background 'mode-line-inactive))

           (face-spec-reset-face 'header-line)
           (face-spec-set
            'header-line
            '((default :inherit common-header-line-inactive-window-header-line-face)))


           (defvar-local common-header-line-active-inactive-face-remapping-cookie nil
             "Cookie used to remap header-line font for inactive windows.")


           (add-hook 'pre-command-hook
                     #'(lambda ()
                         (when (and
                                (or common-header-line-mode common-mode-line-mode)
                                (symbolp this-command)
                                (string-match-p
                                 "^\\(windmove-.*\\|.*window.*\\|.*mouse.*\\)$"
                                 (symbol-name this-command)))
                           (let* ((win (selected-window)))
                             (with-current-buffer
                                 (window-buffer win)
                               (when common-header-line-active-inactive-face-remapping-cookie
                                 (face-remap-remove-relative common-header-line-active-inactive-face-remapping-cookie)
                                 (setq-local common-header-line-active-inactive-face-remapping-cookie nil)))
                             (force-window-update win)))))

           (add-hook 'common-header-line-mode
                     #'(lambda ()
                         (unless common-header-line-mode
                           (dolist (buf (buffer-list))
                             (with-current-buffer buf
                               (when common-header-line-active-inactive-face-remapping-cookie
                                 (face-remap-remove-relative common-header-line-active-inactive-face-remapping-cookie)
                                 (setq-local common-header-line-active-inactive-face-remapping-cookie nil)))))))

           (add-hook 'after-make-frame-functions
                     #'(lambda (frame)
                         (when (or common-mode-line-mode common-header-line-mode)
                           (face-spec-recalc 'header-line frame))))


           (setq common-header-line-per-window-format-function
                 #'(lambda (win)
                     (with-current-buffer (window-buffer win)
                       (when common-header-line-active-inactive-face-remapping-cookie
                         (face-remap-remove-relative common-header-line-active-inactive-face-remapping-cookie)
                         (setq-local common-header-line-active-inactive-face-remapping-cookie nil))
                       (setq-local
                        common-header-line-active-inactive-face-remapping-cookie
                        (face-remap-add-relative 'header-line
                                                 'common-header-line-active-window-header-line-face))
                       (face-remap-reset-base 'header-line))
                     '("%e" mode-line-front-space mode-line-mule-info mode-line-client
                       mode-line-modified mode-line-remote mode-line-frame-identification
                       mode-line-buffer-identification "   " mode-line-position
                       (vc-mode vc-mode)
                       "  " mode-line-misc-info mode-line-end-spaces)))

           (setq common-mode-line-update-display-function
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
                                'common-mode-line-face common-header-mode-line--selected-window))
                              (win (cdr (assq 'win display))))
                           (insert mode-l-str))
                         (setq-local mode-line-format nil)
                         (setq-local header-line-format nil)
                         (setq-local buffer-read-only t)))))

           (set-face-attribute
            'common-mode-line-active-window-mode-line-face
            nil :height 0.1)
           (set-face-attribute
            'common-mode-line-inactive-window-mode-line-face
            nil :height 0.3)

           ;; (set-face-attribute
           ;;  'common-mode-line-active-window-mode-line-face
           ;;  nil :height 0.9)
           ;; (set-face-attribute
           ;;  'common-mode-line-inactive-window-mode-line-face
           ;;  nil :height 0.8)

           ;; (setq common-mode-line-per-window-format-function
           ;;       #'(lambda (win)
           ;;           (format-mode-line
           ;;            '("%e" mode-line-front-space mode-line-mule-info mode-line-client
           ;;              mode-line-modified mode-line-remote mode-line-frame-identification
           ;;              mode-line-buffer-identification "   " mode-line-position
           ;;              (vc-mode vc-mode)
           ;;              "  " mode-line-misc-info mode-line-end-spaces)
           ;;            nil win (window-buffer win))))
           )))

```

Result of the above code:  
[![emacs 26 custom](screenshots/emacs26custom_th.jpg)](screenshots/emacs26custom.jpg)


## How to contribute  

Edit `common-header-mode-line-source.el`, then run make or build.sh, 
then start `emacs -Q`, open `common-heade-mode-line.el`, `eval-buffer`, 
test how it works, if works well commit and push.  
When editing `common-header-mode-line-source.el` using `$*` in symbol or string will 
cause the current top level form be repeated two times 
-- first time the `$*` will be replaced by `header`, second time by `mode`.  
The `$@` in symbol or string will be replaced by `header-mode`.  
