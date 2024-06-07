CC = gcc
CFLAGS = -Wall -Wextra -O2
TARGET = romgen
SRCS = romgen.c zip.c
HEADERS = miniz.h zip.h
OBJS = $(SRCS:.c=.o)
all: $(TARGET)
$(TARGET): $(SRCS) $(HEADERS)
	$(CC) $(CFLAGS) -o $@ $(SRCS)
clean:
	rm -f $(TARGET)
.PHONY: all clean
