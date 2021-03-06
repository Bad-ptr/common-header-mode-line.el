# Copyright (C) 2017 Constantin Kulikov

# Author: Constantin Kulikov (Bad_ptr) <zxnotdead@gmail.com>
# Date: 2017/02/12 11:14:08
# License: GPL either version 3 or any later version
# Keywords: emacs, mode-line, header-line, convenience, frames, windows
# URL: http://github.com/Bad-ptr/common-header-mode-line.el


EMACS ?= emacs

PATHSEP ?= /

PACKAGE_NAME      ?= common-header-mode-line
PACKAGE_FILE_NAME := $(PACKAGE_NAME)-pkg.el
PACKAGE_VERSION   ?= $(shell eval echo $$(cat $(PACKAGE_FILE_NAME)| head -n3 | tail -n1))
PACKAGE_NAME_VERSION := $(PACKAGE_NAME)-$(PACKAGE_VERSION)

BUILD_TOP_DIR     ?= pkg
BUILD_PACKAGE_DIR := $(BUILD_TOP_DIR)$(PATHSEP)$(PACKAGE_NAME_VERSION)

SOURCES := $(wildcard *-source.el)

README_OUTPUT       := $(BUILD_PACKAGE_DIR)$(PATHSEP)README
FILES_TO_COPY       := $(PACKAGE_FILE_NAME) LICENSE
COPY_AS_IS          := $(addprefix $(BUILD_PACKAGE_DIR)$(PATHSEP), $(FILES_TO_COPY))

OUTPUTS_COPY     := $(README_OUTPUT) $(COPY_AS_IS)
OUTPUTS_GENERATE := $(patsubst %-source.el, $(BUILD_PACKAGE_DIR)$(PATHSEP)%.el, $(SOURCES))

DEPS1 := generate-common-header-mode-line.el \
	common-code-substitute.el \
	$(PACKAGE_FILE_NAME) \
    build.sh


.PHONY: all build package clean


all: build

$(BUILD_TOP_DIR) $(BUILD_PACKAGE_DIR):
	mkdir -p $@

$(README_OUTPUT): \
README.md | $(BUILD_TOP_DIR) $(BUILD_PACKAGE_DIR)
	cp $< $@
$(COPY_AS_IS): $(BUILD_PACKAGE_DIR)$(PATHSEP)% : % \
$(FILES_TO_COPY) | $(BUILD_TOP_DIR) $(BUILD_PACKAGE_DIR)
	cp $< $@

$(OUTPUTS_GENERATE): \
$(BUILD_PACKAGE_DIR)$(PATHSEP)%.el : %-source.el \
$(DEPS1) | $(BUILD_TOP_DIR) $(BUILD_PACKAGE_DIR)
	.$(PATHSEP)build.sh $< $@

build: $(OUTPUTS_GENERATE) $(OUTPUTS_COPY)

$(BUILD_PACKAGE_DIR).tar: build
	tar -C $(BUILD_TOP_DIR) -cf $@ $(PACKAGE_NAME_VERSION)
#mv $(PACKAGE_NAME_VERSION).tar $(BUILD_TOP_DIR)

package: $(BUILD_PACKAGE_DIR).tar

clean:
	rm -rf $(BUILD_TOP_DIR)
