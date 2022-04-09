#ifndef clox_chunk_h
#define clox_chunk_h


#include "common.h"
#include "value.h"


/** 
 * Each instruction has a one-byte operation code.
 * These are defined here.
 **/
typedef enum {
    OP_CONSTANT,
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_POP,
    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_GET_GLOBAL,
    OP_DEFINE_GLOBAL,
    OP_SET_GLOBAL,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_NOT,
    OP_NEGATE,
    OP_PRINT,
    OP_RETURN,
} OpCode;

/**
 * Structure to hold the data associated to the instructions of the bytecode.
 * This structure is essentially a dynamic array.
 **/
typedef struct {
    int count;
    int capacity;
    uint8_t * code;
    int * lines;
    ValueArray constants;
} Chunk;


/**
 * Initialize a chunk.
 **/
void initChunk (Chunk * chunk);

/**
 * Write a new chunk to the end of the array.
 **/
void writeChunk (Chunk * chunk, uint8_t byte, int line);

/**
 * Destroy a chunk.
 **/
void freeChunk (Chunk * chunk);

/**
 * Add a constant to a chunk.
 **/
int addConstant (Chunk * chunk, Value value);


#endif

