## Makefile for XLIB   -*-Makefile-*-
##
## Copyright (C) 2004 Steve Youngs
##
## This file is part of XLIB

## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions
## are met:
##
## 1. Redistributions of source code must retain the above copyright
##    notice, this list of conditions and the following disclaimer.
##
## 2. Redistributions in binary form must reproduce the above copyright
##    notice, this list of conditions and the following disclaimer in the
##    documentation and/or other materials provided with the distribution.
##
## 3. Neither the name of the author nor the names of any contributors
##    may be used to endorse or promote products derived from this
##    software without specific prior written permission.
##
## THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
## IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
## WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
## DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
## FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
## CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
## SUBSTITUTE GOODS OR SERVICES# LOSS OF USE, DATA, OR PROFITS# OR
## BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
## WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
## OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
## IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

PACKAGE = xlib-ffi
VER = 1.0

# csh... yell no, we won't go!

SHELL = /bin/sh

# Programs and their flags.
ifndef XEMACS
XEMACS = sxemacs
endif
XEMACS_FLAGS = -batch -no-autoloads
INSTALL = install

PKG_INSTALL = install
TAR = tar
TAR_FLAGS = czf

# Our prefix. Everything hangs off this.  
ifndef PREFIX
  ifeq ('$(XEMACS)','sxemacs')
    PREFIX = /usr/local/share/sxemacs/site-packages
  else
    PREFIX = /usr/local/lib/xemacs/site-packages
  endif
endif

# Where the lisp files go.
LISP_DIR = $(PREFIX)/lisp/$(PACKAGE)

# If you want to make a tarball that you can just unpack on all your
# PC's you can 'make pkg'.  The 'pkg' target uses these directories to
# build the tarball.
STAGING = ../build-pkg
LISP_STAGING = $(STAGING)/lisp/$(PACKAGE)

############################################################################
##                No User Configurable Items Below Here                   ##
############################################################################
AUTO_SOURCES= ./lisp/auto-autoloads.el ./lisp/xlib-version.el ./lisp/xlib-keyconst.el
AUTO_OBJECTS=$(AUTO_SOURCES:.el=.elc)

SOURCES = $(filter-out $(AUTO_SOURCES), $(wildcard ./lisp/*.el))
OBJECTS = $(SOURCES:.el=.elc)

PRELOADS = -eval \("push \"./lisp\" load-path"\) -l ./lisp/auto-autoloads.el

AUTOLOAD_PACKAGE_NAME = (setq autoload-package-name \"$(PACKAGE)\")
AUTOLOAD_FILE = (setq generated-autoload-file \"./lisp/auto-autoloads.el\")


.SUFFIXES:
.SUFFIXES: .elc .el

all:: compile

autoloads: lisp/auto-autoloads.el

compile: autoloads $(AUTO_OBJECTS) $(OBJECTS)

%.elc: %.el
	$(XEMACS) $(XEMACS_FLAGS) $(PRELOADS) -l bytecomp \
		-f batch-byte-compile $<

lisp/auto-autoloads.el: $(SOURCES) ./lisp/xlib-version.el
	$(XEMACS) $(XEMACS_FLAGS) \
		-eval "$(AUTOLOAD_PACKAGE_NAME)" \
		-eval "$(AUTOLOAD_FILE)" \
		-l autoload -l cl-macs -f batch-update-autoloads $^
	$(XEMACS) $(XEMACS_FLAGS) -l bytecomp \
		-f batch-byte-compile ./lisp/auto-autoloads.el

./lisp/xlib-version.el:
	echo ";;; Automatically generated file -- DO NOT EDIT OR DELETE" > $@
	echo ";;;###autoload" >> $@
	echo "(defconst xlib-version" >> $@
	if [ -d "./.git" -a -x `type git 2>/dev/null` ]; then \
		printf '  "%s"' `git descripbe |tail -n1` >> $@; \
	else \
		echo -n '  "$(VER)"' >> $@; \
	fi
	echo ")" >> $@
	echo "(provide 'xlib-version)" >> $@

version: ./lisp/xlib-version.el

./lisp/xlib-keyconst.el:
	$(XEMACS) $(XEMACS_FLAGS) -l ./genkeysymdef.el \
		-eval \("genksd-generate \"./lisp/xlib-keyconst.el\""\)

install: $(SOURCES) $(AUTO_SOURCES) $(OBJECTS) $(AUTO_OBJECTS)
	$(INSTALL) -d $(LISP_DIR)
	$(INSTALL) -m 644 $(SOURCES) $(AUTO_SOURCES) $(OBJECTS) $(AUTO_OBJECTS) \
		$(LISP_DIR)

pkg: $(SOURCES) $(AUTO_SOURCES) $(OBJECTS) $(AUTO_OBJECTS)
	$(PKG_INSTALL) -d $(STAGING) $(LISP_STAGING)
	$(PKG_INSTALL) -m 644 $(SOURCES) $(AUTO_SOURCES) $(OBJECTS) $(AUTO_OBJECTS) \
		$(LISP_STAGING)
	(cd $(STAGING); \
		$(TAR) $(TAR_FLAGS) $(PACKAGE)-$(VER)-pkg.tar.gz \
			./lisp/$(PACKAGE))

upgrade: uninstall install

uninstall:: 
	rm -rf $(LISP_DIR)
	rm -f $(INFO_DIR)/$(INFO_FILES)

clean::
	rm -f $(OBJECTS) $(AUTO_SOURCES) $(AUTO_OBJECTS)

distclean: clean
	rm -f core* *~ TAGS lisp/*~ lisp/*.elc

./lisp/xlib-keyconst.elc: ./lisp/xlib-keyconst.el
./lisp/xlib-const.elc: ./lisp/xlib-keyconst.elc
./lisp/xlib-xlib.elc: ./lisp/ffi-xlib.elc ./lisp/xlib-const.elc ./lisp/xlib-common.elc

# Developer targets
tags: TAGS

TAGS: $(SOURCES)
	etags $(SOURCES)

.PHONY: clean distclean ./lisp/xlib-version.el
