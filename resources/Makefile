##
# ~{project-name}
#
# @file
# @version 0.1

CC=gcc
CFLAGS=-g -std=gnu11 -Wall
LDLIBS=

# Example of using libuv:
# LDLIBS=`pkg-config --cflags --libs libuv`

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

# prepare and build binary
all: prepare $(BIN)

# create directory structure
prepare:
	-mkdir $(BINDIR)
	-mkdir $(SRC)
	-mkdir $(OBJ)

# run `make bear` to generate a compile_commands.json file for CCLS
bear: clean
	bear -- make

# build binary from objects
$(BIN): $(OBJS)
	$(CCC) $(OBJS) -o $(BIN)

# build objects from source
$(OBJ)/%.o: $(SRC)/%.c
	$(CCC) -c $< -o $@

# build and run program
run: $(BIN)
	./$(BIN)

# install program to specified directory
install: $(BIN)
	cp $(BIN) $(INSTALLDIR)/$(PROGNAME)

# uninstall and remove objects
clean:
	-rm -r $(OBJ)/*
	-rm -r $(BINDIR)/*
	-rm $(INSTALLDIR)/$(PROGNAME)

# end
