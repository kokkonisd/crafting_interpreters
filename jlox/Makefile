DEBUG ?= 1
JC=javac

SRC=src
BIN=bin
PKG=com.craftinginterpreters.lox

ifeq ($(DEBUG), 1)
	LOX_FLAGS=-d
else
	LOX_FLAGS=
endif

TOOL_SRC=tools
TOOL_PKG=$(subst lox,tools,$(PKG))

SRC_FILES=$(wildcard $(SRC)/*.java)
TOOL_SRC_FILES=$(wildcard $(TOOL_SRC)/*.java)

TARGET=Lox
TARGET_PATH=$(BIN)/$(subst .,/,$(PKG))/$(TARGET).class

TOOLS=GenerateAst
TOOL_PATHS=$(addprefix $(BIN)/$(subst .,/,$(TOOL_PKG))/,$(addsuffix .class,$(TOOLS)))

all: $(TARGET_PATH)

run: $(TARGET_PATH) FORCE
	java -cp $(BIN)/ $(PKG).$(TARGET) $(LOX_FLAGS)

*.lox: $(TARGET_PATH) FORCE
	java -cp $(BIN)/ $(PKG).$(TARGET) $(LOX_FLAGS) $@

$(TARGET_PATH): generate-ast $(SRC_FILES)
	$(JC) $(subst generate-ast,,$^) -d $(BIN)/

generate-ast: $(TOOL_PATHS)
	java -cp $(BIN)/ $(TOOL_PKG).GenerateAst $(SRC)

$(TOOL_PATHS): $(TOOL_SRC_FILES)
	$(JC) $^ -d $(BIN)/

clean:
	rm -rf $(BIN)/

.PHONY: run clean

FORCE: ;
