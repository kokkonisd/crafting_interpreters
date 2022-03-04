#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "value.h"


#define STACK_MAX 256


/**
 * Structure to describe the Virtual Machine running the bytecode.
 **/
typedef struct {
    Chunk * chunk;
    uint8_t * ip;
    Value stack[STACK_MAX];
    Value * stackTop;
} VM;
/**
 * Enum to describe the potential results of interpretation.
 **/
typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

/**
 * Initialize a Virtual Machine.
 **/
void initVM ();
/**
 * Destroy a Virtual Machine.
 **/
void freeVM ();
/**
 * Interpret a chunk of bytecode using the Virtual Machine.
 **/
InterpretResult interpret (Chunk * chunk);
/**
 * Push a value to the VM's stack.
 **/
void push (Value value);
/**
 * Pop a value from the VM's stack.
 **/
Value pop ();

#endif
