CURRENT_DIR := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))

BASENAME = sweep
SOEXT    = dylib

TARGET   = $(BASENAME)-module.$(SOEXT)
SOURCE   = $(BASENAME).c

LDFLAGS += -shared
LDFLAGS += -Llib
LDFLAGS += -lswipl

CFLAGS  += -fPIC
CFLAGS  += -fdiagnostics-absolute-paths
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

.PHONY: clean all swipl

all: $(TARGET)

$(TARGET): $(SOURCE) swipl
	$(CC) $(CFLAGS) -o $@ $(SOURCE) $(LDFLAGS)

clean:
	rm -rf bin lib share swipl/build
	rm -f $(TARGET)

swipl:
	cd swipl; \
	rm -rf build; \
	mkdir build; \
	cd build; \
	cmake $(CMAKE_OPTIONS) -G Ninja ..; \
	ninja; \
	ninja install
