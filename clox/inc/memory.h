#ifndef clox_memory_h
#define clox_memory_h


#include "common.h"
#include "object.h"


#define ALLOCATE(type, count) \
    (type *) reallocate(NULL, 0, sizeof(type) * (count))

#define FREE(type, pointer) \
    reallocate(pointer, sizeof(type), 0);

#define GROW_CAPACITY(capacity) \
    ((capacity) < 8 ? 8 : (capacity) * 2)

#define GROW_ARRAY(type, pointer, oldCount, newCount) \
    (type *) reallocate(pointer, sizeof(type) * (oldCount), sizeof(type) * (newCount))

#define FREE_ARRAY(type, pointer, oldCount) \
    reallocate(pointer, sizeof(type) * (oldCount), 0)


/**
 * Function to handle all dynamic memory management.
 * This is used instead of a direct call to some `*alloc` function because we need to
 * track memory usage for the garbage collector.
 **/
void * reallocate (void * pointer, size_t oldSize, size_t newSize);

void markObject (Obj * object);

void markValue (Value value);

void collectGarbage ();

void freeObjects ();


#endif

