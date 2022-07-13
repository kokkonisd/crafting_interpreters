DEBUG ?= 1
CC = gcc
DEBUG_FLAGS = -DDEBUG_TRACE_EXECUTION -DDEBUG_PRINT_CODE -DDEBUG_STRESS_GC \
			  -DDEBUG_LOG_GC
# Define NaN boxing. If a specific target architecture does not support it (i.e. if it
# does not support IEEE 754 or if it uses the full 64 bits of 64-bit pointers) then we
# should disable it. Since this Makefile is made to run on x86_64 targets, we're keeping
# it on.
FEATURE_FLAGS = -DNAN_BOXING
CFLAGS = -Wall -Wextra -Wpedantic $(FEATURE_FLAGS)

SRC=src
INC=inc
BIN=bin
BUILD=build
TARGET=clox

ifeq ($(DEBUG), 1)
	CFLAGS+=-g -O0 $(DEBUG_FLAGS)
else
	CFLAGS+=-O3
endif

SRC_FILES=$(wildcard $(SRC)/*.c)
INC_FILES=$(wildcard $(INC)/*.h)
OBJ_FILES=$(patsubst $(SRC)/%,$(BUILD)/%,$(patsubst %.c,%.o,$(SRC_FILES)))


TARGET_PATH=$(BIN)/$(TARGET)

all: $(TARGET_PATH)

run: $(TARGET_PATH)
	./$(TARGET_PATH)

%.lox: $(TARGET_PATH) FORCE
	./$(TARGET_PATH) $@

$(TARGET_PATH): $(BIN) $(BUILD) $(OBJ_FILES) $(INC_FILES)
	$(CC) $(OBJ_FILES) -o $@

$(BUILD)/%.o: $(SRC)/%.c
	$(CC) $(CFLAGS) -I$(INC)/ -c $^ -o $@

$(BIN):
	@mkdir -p $(BIN)/

$(BUILD):
	@mkdir -p $(BUILD)/

clean:
	rm -rf $(BUILD)/ $(BIN)/


.PHONY: clean run

FORCE: ;
