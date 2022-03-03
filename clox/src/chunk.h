#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"

/** 
 * Each instruction has a one-byte operation code.
 * These are defined here.
 **/
typedef enum {
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
} Chunk;

/**
 * Initialize a chunk.
 **/
void initChunk (Chunk * chunk);
/**
 * Write a new chunk to the end of the array.
 **/
void writeChunk (Chunk * chunk, uint8_t byte);
/**
 * Destroy a chunk.
 **/
void freeChunk (Chunk * chunk);

#endif

