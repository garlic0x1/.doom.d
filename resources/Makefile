##
# ~{project-name}
#
# @file
# @version 0.1

CC=gcc
CFLAGS=-g -std=gnu11 -Wall
LDLIBS=

# Example of using libuv:
# LDLIBS=`pkg-config --libs libuv`

# CC command:
CCC=$(CC) $(CFLAGS) $(LDLIBS)

# Edit these:
PROGNAME=~{project-name}
INSTALLDIR=~/.local/bin

# Do not edit these:
BINDIR=bin
BIN=$(BINDIR)/$(PROGNAME)
SRC=src
OBJ=obj
SRCS=$(wildcard $(SRC)/**/*.c) $(wildcard $(SRC)/*.c)
OBJS=$(patsubst $(SRC)/%.c, $(OBJ)/%.o, $(SRCS))

all: prepare $(BIN)

prepare:
	-mkdir $(BINDIR)
	-mkdir $(SRC)
	-mkdir $(OBJ)

$(BIN): $(OBJS)
	$(CCC) $(OBJS) -o $(BIN)

$(OBJ)/%.o: $(SRC)/%.c
	$(CCC) -c $< -o $@

build: $(BIN)

run: $(BIN)
	./$(BIN)

install: $(BIN)
	cp $(BIN) $(INSTALLDIR)/$(PROGNAME)

clean:
	-rm -r $(OBJ)/*
	-rm -r $(BINDIR)/*
	-rm $(INSTALLDIR)/$(PROGNAME)

# end
