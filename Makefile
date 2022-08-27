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
LDFLAGS += -Llib
LDFLAGS += -lswipl

CFLAGS  += -fPIC
CFLAGS  += -Wall
CFLAGS  += -Wextra
CFLAGS  += -O2
CFLAGS  += -Ilib/swipl/include

CMAKE_OPTIONS += -DCMAKE_INSTALL_PREFIX=$(CURRENT_DIR)
CMAKE_OPTIONS += -DUSE_GMP=OFF
CMAKE_OPTIONS += -DSWIPL_PACKAGES_ODBC=OFF
CMAKE_OPTIONS += -DSWIPL_PACKAGES_JAVA=OFF
CMAKE_OPTIONS += -DSWIPL_PACKAGES_X=OFF
CMAKE_OPTIONS += -DSWIPL_INSTALL_IN_LIB=ON

.PHONY: clean all swipl check

all: $(TARGET)

$(OBJECT): $(SOURCE) lib/libswipl.$(SOEXT)
	$(CC) $(CFLAGS) -o $@ -c $(SOURCE)

$(TARGET): $(OBJECT)
	$(CC) -o $@ $(OBJECT) $(LDFLAGS)

clean:
	rm -rf bin lib share swipl/build
	rm -f $(TARGET) $(OBJECT) $(BASENAME).info

lib/libswipl.$(SOEXT):
	cd swipl; \
	rm -rf build; \
	mkdir build; \
	cd build; \
	cmake $(CMAKE_OPTIONS) -G Ninja ..; \
	ninja; \
	ninja install

$(BASENAME).info:: README.org
	emacs -Q --batch --eval '(require (quote ox-texinfo))' --eval "(with-current-buffer (find-file \"README.org\") (org-export-to-file (quote texinfo) \"$@\" nil nil nil nil nil (quote org-texinfo-compile)))"

check: $(TARGET)
	emacs -batch --eval '(add-to-list (quote load-path) (expand-file-name "."))' -l ert -l sweep -l sweep-tests.el -f ert-run-tests-batch-and-exit
