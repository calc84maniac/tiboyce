CC = gcc
CFLAGS = -Wall -Wextra -Wno-misleading-indentation -O2
TARGET = tiboyce-romgen
TARGET_JS = romgen.js
TARGET_WASM = romgen.wasm
SRCS = romgen.c zip.c
HEADERS = miniz.h zip.h
all: $(TARGET)
wasm: $(TARGET_JS)
wasm: CC = emcc
wasm: CFLAGS += -s INVOKE_RUN=0 -s NO_EXIT_RUNTIME=1 -s EXPORTED_RUNTIME_METHODS=["callMain","cwrap"] -s INITIAL_MEMORY=33554432
$(TARGET) $(TARGET_JS): $(SRCS) $(HEADERS)
	$(CC) $(CFLAGS) -o $@ $(SRCS)
clean:
	rm -f $(TARGET) $(TARGET_JS) $(TARGET_WASM)
.PHONY: all clean
