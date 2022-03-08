#ifndef clox_value_h
#define clox_value_h


#include "common.h"


typedef double Value;

/**
 * Dynamic array structure to hold Values.
 **/
typedef struct {
    int capacity;
    int count;
    Value * values;
} ValueArray;


/**
 * Initialize a Value array.
 **/
void initValueArray (ValueArray * array);

/**
 * Write a new Value to the end of the array.
 **/
void writeValueArray (ValueArray * array, Value value);

/**
 * Destroy a Value array.
 **/
void freeValueArray (ValueArray * array);

/**
 * Print a value.
 **/
void printValue (Value value);


#endif

