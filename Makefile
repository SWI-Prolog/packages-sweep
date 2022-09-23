BASENAME = sweep

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
    SOEXT = so
endif
ifeq ($(UNAME_S),Darwin)
    SOEXT = dylib
endif

SWIPL      ?= swipl
SWIPLBASE   = $(shell $(SWIPL) --dump-runtime-variables | grep PLBASE   | cut -f 2 -d = | cut -f 1 -d ';')
SWIPLLIBDIR = $(shell $(SWIPL) --dump-runtime-variables | grep PLLIBDIR | cut -f 2 -d = | cut -f 1 -d ';')

EMACS ?= emacs

TARGET   = $(BASENAME)-module.$(SOEXT)
OBJECT   = $(BASENAME).o
SOURCE   = $(BASENAME).c

LDFLAGS += -shared
LDFLAGS += -L$(SWIPLLIBDIR)
LDFLAGS += -lswipl

CFLAGS  += -fPIC
CFLAGS  += -Wall
CFLAGS  += -Wextra
CFLAGS  += -O2
CFLAGS  += -I$(SWIPLBASE)/include

.PHONY: clean all check info

all: $(TARGET)

$(OBJECT): $(SOURCE)
	$(CC) $(CFLAGS) -o $@ -c $(SOURCE)

$(TARGET): $(OBJECT)
	$(CC) -o $@ $(OBJECT) $(LDFLAGS)

clean:
	rm -f $(TARGET) $(OBJECT) $(BASENAME).info $(BASENAME).texi $(BASENAME).html

info: $(BASENAME).info
$(BASENAME).info:: README.org
	$(EMACS) -Q --batch --eval "(require 'ox-texinfo)" \
		--eval "(with-current-buffer (find-file \"README.org\") (org-export-to-file (quote texinfo) \"$@\" nil nil nil nil nil (quote org-texinfo-compile)))"

check: $(TARGET)
	$(EMACS) --batch --eval '(add-to-list (quote load-path) (expand-file-name "."))' \
		-l ert -l sweep -l sweep-tests.el -f ert-run-tests-batch-and-exit
