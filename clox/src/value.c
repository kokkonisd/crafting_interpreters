#include <stdio.h>
#include <string.h>

#include "object.h"
#include "memory.h"
#include "value.h"


void initValueArray (ValueArray * array)
{
    array->values = NULL;
    array->capacity = 0;
    array->count = 0;
}


void writeValueArray (ValueArray * array, Value value)
{
    if (array->capacity < array->count + 1) {
        int oldCapacity = array->capacity;
        array->capacity = GROW_CAPACITY(oldCapacity);
        array->values = GROW_ARRAY(Value, array->values, oldCapacity, array->capacity);
    }

    array->values[array->count] = value;
    array->count++;
}


void freeValueArray (ValueArray * array)
{
    FREE_ARRAY(Value, array->values, array->capacity);
    initValueArray(array);
}


void printValue (Value value)
{
#   ifdef NAN_BOXING

        if (IS_BOOL(value)) {
            printf(AS_BOOL(value) ? "true" : "false");
        } else if (IS_NIL(value)) {
            printf("nil");
        } else if (IS_NUMBER(value)) {
            printf("%g", AS_NUMBER(value));
        } else if (IS_OBJ(value)) {
            printObject(value);
        }

#   else // NAN_BOXING

        switch (value.type) {
            case VAL_BOOL:
                printf(AS_BOOL(value) ? "true" : "false");
                break;
            case VAL_NIL: printf("nil"); break;
            case VAL_NUMBER: printf("%g", AS_NUMBER(value)); break;
            case VAL_OBJ: printObject(value); break;
        }

#   endif // NAN_BOXING
}


bool valuesEqual (Value a, Value b)
{
#   ifdef NAN_BOXING

    // In IEEE 754, NaN values are *not* equal to themselves. For example, the following
    // lox program:
    //
    //     var nan = 0/0;
    //     print nan == nan;
    //
    // should print `false`. That forces us to do a special check here for that case.
    if (IS_NUMBER(a) && IS_NUMBER(b)) {
        return AS_NUMBER(a) == AS_NUMBER(b);
    }

    // In every other case, if the bit representations are different, then the values
    // are not equal.
    return a == b;

#   else // NAN_BOXING

    if (a.type != b.type) return false;

    switch (a.type) {
        case VAL_BOOL: return AS_BOOL(a) == AS_BOOL(b);
        case VAL_NIL: return true;
        case VAL_NUMBER: return AS_NUMBER(a) == AS_NUMBER(b);
        // In the case of strings, because we're interning all of them, there's no need
        // to actually check character by character. If two strings have the same
        // address then they must be the same, as we're interning all of them and only
        // keeping unique ones.
        case VAL_OBJ: return AS_OBJ(a) == AS_OBJ(b);
        default: return false; // Unreachable.
    }

#   endif // NAN_BOXING
}

