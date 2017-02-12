#!/bin/sh
# Copyright (C) 2017 Constantin Kulikov

# Author: Constantin Kulikov (Bad_ptr) <zxnotdead@gmail.com>
# Date: 2017/02/12 11:14:08
# License: GPL either version 3 or any later version
# Keywords: emacs, mode-line, header-line, convenience, frames, windows
# X-URL: http://github.com/Bad-ptr/common-header-mode-line.el

emacs --batch --eval "(progn (add-to-list 'load-path \"./\") (require 'generate-common-header-mode-line))"
