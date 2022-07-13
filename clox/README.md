# clox

A C implementation of Lox.


## How to read this (sub)repo

This part of the repo contains the following modules (each having one `.c` file in
`src/` and one `.h` file in `inc/`):

- **chunk**. This module contains the definitions of the operation codes (opcodes)
  supported by the clox Virtual Machine, as well as helper functions to create and
  manipulate chunks of bytecode.
- **compiler**. This module contains the implementation of the compiler, taking in a
  token stream from the scanner and emitting bytecode for the VM to interpret.
- **debug**. This module contains helper functions to disassemble bytecode chunks and
  instructions, to aid in debugging the VM.
- **memory**. This module contains functions to track the allocation/deallocation of
  objects in memory, as well as the implementation of clox's Garbage Collector.
- **object**. This module contains the implementation of clox's objects: strings,
  numbers, functions, methods, classes etc.
- **scanner**. This module contains the implementation of the scanner, taking in raw
  source code and emitting a stream of tokens for the compiler.
- **table**. This module contains the implementation of a hash table, that is used in
  various places in the VM.
- **value**. This module contains the implementation of a dynamic array for Lox Values
  (i.e. types), as well as the definition of the internal representation of those
  values.
- **vm**. This module contains the implementation of the Virtual Machine, taking in
  chunks of bytecode and interpreting them. This is the module that actually _runs_ the
  Lox code.

There are also some extra files that are not really modules:

- `src/main.c`: implementation of the CLI of clox, that handles reading from a file or
  running in interactive (REPL) mode.
- `inc/common.h`: a header file containing common `#include`s.


## How to build/run clox

Building and running clox is done via the `Makefile`. Here are the supported targets:

- `make (all)`: only build clox, with no additional steps. The `DEBUG` flag, defined in
  the `Makefile`, determines whether to build in debug mode or production mode. The
  output of both the compiler and the interpreter is **_very_** verbose, so if you're
  just trying out the interpreter it is strongly recommended that you run
  `make DEBUG=0`.
- `make <example>.lox`: build clox and run the interpreter on the example file.
- `make run`: build clox and run the interpreter in interactive (REPL) mode.
