CC = gcc
CFLAGS = -Wall -Wextra -Wno-implicit-fallthrough -Wno-expansion-to-defined -O2
TARGET = tiboyce-convertsav
TARGET_JS = convertsav.js
TARGET_WASM = convertsav.wasm
SRCS = convertsav.c lzf_c.c lzf_d.c
HEADERS = lzf.h lzfP.h
all: $(TARGET)
wasm: $(TARGET_JS)
wasm: CC = emcc
wasm: CFLAGS += -s INVOKE_RUN=0 -s NO_EXIT_RUNTIME=1 -s EXPORTED_RUNTIME_METHODS=["callMain","cwrap"] -s INITIAL_MEMORY=33554432
$(TARGET) $(TARGET_JS): $(SRCS) $(HEADERS)
	$(CC) $(CFLAGS) -o $@ $(SRCS)
clean:
	rm -f $(TARGET) $(TARGET_JS) $(TARGET_WASM)
.PHONY: all clean
