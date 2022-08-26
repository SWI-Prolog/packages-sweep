BASENAME = sweep
SOEXT    = dylib

TARGET   = $(BASENAME)-module.$(SOEXT)
SOURCE   = $(BASENAME).c

LDFLAGS += -shared
LDFLAGS += -lswipl

CFLAGS  += -fPIC
CFLAGS  += -fdiagnostics-absolute-paths
CFLAGS  += -Wall
CFLAGS  += -Wextra
CFLAGS  += -O2

.PHONY: clean all

all: $(TARGET)

$(TARGET): $(SOURCE)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

clean:
	rm -f $(TARGET)
