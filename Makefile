CURRENT_DIR := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))

BASENAME = sweep

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
    SOEXT = so
endif
ifeq ($(UNAME_S),Darwin)
    SOEXT = dylib
endif

TARGET   = $(BASENAME)-module.$(SOEXT)
OBJECT   = $(BASENAME).o
SOURCE   = $(BASENAME).c

LDFLAGS += -shared
LDFLAGS += -lswipl

CFLAGS  += -fPIC
CFLAGS  += -Wall
CFLAGS  += -Wextra
CFLAGS  += -O2
CFLAGS  += -I/usr/local/lib/swipl/include

.PHONY: clean all swipl check

all: $(TARGET)

$(OBJECT): $(SOURCE)
	$(CC) $(CFLAGS) -o $@ -c $(SOURCE)

$(TARGET): $(OBJECT)
	$(CC) -o $@ $(OBJECT) $(LDFLAGS)

clean:
	rm -f $(TARGET) $(OBJECT) $(BASENAME).info

$(BASENAME).info:: README.org
	emacs -Q --batch --eval '(require (quote ox-texinfo))' --eval "(with-current-buffer (find-file \"README.org\") (org-export-to-file (quote texinfo) \"$@\" nil nil nil nil nil (quote org-texinfo-compile)))"

check: $(TARGET)
	emacs -batch --eval '(add-to-list (quote load-path) (expand-file-name "."))' -l ert -l sweep -l sweep-tests.el -f ert-run-tests-batch-and-exit
