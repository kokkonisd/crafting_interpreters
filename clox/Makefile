DEBUG ?= 1
CC=gcc
CFLAGS=-Wall -Wextra -Wpedantic

SRC=src
BIN=bin
BUILD=build
TARGET=clox

ifeq ($(DEBUG), 1)
	CFLAGS+=-g -O0
else
	CFLAGS+=-O3
endif

SRC_FILES=$(wildcard $(SRC)/*.c)
HEADER_FILES=$(wildcard $(SRC)/*.h)
OBJ_FILES=$(patsubst $(SRC)/%,$(BUILD)/%,$(patsubst %.c,%.o,$(SRC_FILES)))


TARGET_PATH=$(BIN)/$(TARGET)

all: $(TARGET_PATH)

run: $(TARGET_PATH)
	./$(TARGET_PATH)

$(TARGET_PATH): $(BIN) $(BUILD) $(OBJ_FILES) $(HEADER_FILES)
	$(CC) $(OBJ_FILES) -o $@

$(BUILD)/%.o: $(SRC)/%.c
	$(CC) $(CFLAGS) -c $^ -o $@

$(BIN):
	@mkdir -p $(BIN)/

$(BUILD):
	@mkdir -p $(BUILD)/

clean:
	rm -rf $(BUILD)/ $(BIN)/


.PHONY: clean run
