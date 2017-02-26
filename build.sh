#!/bin/sh
# Copyright (C) 2017 Constantin Kulikov

# Author: Constantin Kulikov (Bad_ptr) <zxnotdead@gmail.com>
# Date: 2017/02/12 11:14:08
# License: GPL either version 3 or any later version
# Keywords: emacs, mode-line, header-line, convenience, frames, windows
# X-URL: http://github.com/Bad-ptr/common-header-mode-line.el

string_to_eval="(progn
;; (setq debug-on-error t)
(setq make-backup-files nil)
(require 'subr-x)
(setq common-header-mode-line-input-filename
      \"$1\"
      common-header-mode-line-output-filename
      \"$2\")
(require 'generate-common-header-mode-line))"

emacs --batch -L . --eval "$string_to_eval"
