#ifndef clox_vm_h
#define clox_vm_h


#include "chunk.h"
#include "table.h"
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
    // Global variables.
    Table globals;
    // Interned strings.
    Table strings;
    Obj * objects;
} VM;

/**
 * Enum to describe the potential results of interpretation.
 **/
typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;


extern VM vm;


/**
 * Initialize a Virtual Machine.
 **/
void initVM ();

/**
 * Destroy a Virtual Machine.
 **/
void freeVM ();

/**
 * Interpret some source code using a Virtual Machine.
 **/
InterpretResult interpret (const char * source);

/**
 * Push a value to the VM's stack.
 **/
void push (Value value);

/**
 * Pop a value from the VM's stack.
 **/
Value pop ();


#endif

