CC = gcc
CFLAGS = -Wall -Wextra -O2
TARGET = convertsav
SRCS = convertsav.c lzf_c.c lzf_d.c
HEADERS = lzf.h lzfP.h
OBJS = $(SRCS:.c=.o)
all: $(TARGET)
$(TARGET): $(SRCS) $(HEADERS)
	$(CC) $(CFLAGS) -o $@ $(SRCS)
clean:
	rm -f $(TARGET)
.PHONY: all clean
