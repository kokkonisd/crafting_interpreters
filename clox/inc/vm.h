#ifndef clox_vm_h
#define clox_vm_h


#include "object.h"
#include "table.h"
#include "value.h"


#define FRAMES_MAX 64
#define STACK_MAX  (FRAMES_MAX * UINT8_COUNT)


typedef struct {
    ObjClosure * closure;
    uint8_t * ip;
    Value * slots;
} CallFrame;


/**
 * Structure to describe the Virtual Machine running the bytecode.
 **/
typedef struct {
    CallFrame frames[FRAMES_MAX];
    int frameCount;
    Chunk * chunk;
    uint8_t * ip;
    Value stack[STACK_MAX];
    Value * stackTop;
    // Global variables.
    Table globals;
    // Interned strings.
    Table strings;
    // Interned "init" string.
    ObjString * initString;
    // Upvalues that are still on the stack.
    ObjUpvalue * openUpvalues;
    // The total number of bytes of managed memory.
    size_t bytesAllocated;
    // The threshold that triggers a garbage collection run.
    size_t nextGC;
    Obj * objects;

    // GC stuff.
    int grayCount;
    int grayCapacity;
    Obj ** grayStack;
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

