#ifndef clox_value_h
#define clox_value_h

#include <string.h>

#include "common.h"


typedef struct Obj Obj;
typedef struct ObjString ObjString;

#ifdef NAN_BOXING


// The sign bit of an IEEE 754 floating-point number is the MSB.
#define SIGN_BIT ((uint64_t) 0x8000000000000000) 
// The definition of a quiet NaN value in IEEE 754.
//
//       type || sign | exponent              | mantissa
//      value || 0    | 1 1 1 1 1 1 1 1 1 1 1 | 1      1           0 ... 0
//               ^                              ^      ^                 ^
//               MSB                            quiet  indefinite*       LSB
//
//  *indefinite: Intel's QNaN Floating-Point Indefinite value has that bit set to 0, so
//  we avoid that value.
#define QNAN     ((uint64_t) 0x7ffc000000000000)

// The following are the value tags, found at the two LSB of the 64-bit value.
#define TAG_NIL   1 // 01.
#define TAG_FALSE 2 // 10.
#define TAG_TRUE  3 // 11.

typedef uint64_t Value;


#define IS_BOOL(value)  (((value) | 1) == TRUE_VAL)
#define IS_NIL(value)   ((value) == NIL_VAL)
#define IS_NUMBER(value) (((value) & QNAN) != QNAN)
// An object is marked by a quiet NaN with its sign bit set to 1.
#define IS_OBJ(value)   (((value) & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT))

#define AS_BOOL(value)   ((value) == TRUE_VAL)
#define AS_NUMBER(value) valueToNum(value)
// In order to get the pointer out of the quiet NaN, we mask off all of the MSBs to
// reveal the pointer sitting on the 50 LSBs.
#define AS_OBJ(value)    ((Obj *) (uintptr_t) ((value) & ~(SIGN_BIT | QNAN)))

#define BOOL_VAL(b)     ((b) ? TRUE_VAL : FALSE_VAL)
#define FALSE_VAL       ((Value) (uint64_t) (QNAN | TAG_FALSE))
#define TRUE_VAL        ((Value) (uint64_t) (QNAN | TAG_TRUE))
#define NIL_VAL         ((Value) (uint64_t) (QNAN | TAG_NIL))
#define NUMBER_VAL(num) numToValue(num)
// Objects are marked by setting the sign bit to 1, and then shoving the pointer in the
// 50 LSBs (because in practice 64-bit pointers only use 48 bits).
#define OBJ_VAL(obj)    (Value) (SIGN_BIT | QNAN | (uint64_t) (uintptr_t) (obj))


static inline double valueToNum (Value value)
{
    double num;
    // See comment in `numToValue()`.
    memcpy(&num, &value, sizeof(Value));

    return num;
}


static inline Value numToValue (double num)
{
    Value value;
    // This only works because we expect the compiler to optimize away the `memcpy()`
    // call. If that's not the case, we should do something like this instead:
    //
    // union { uint64_t bits; double num; } data;
    // data.num = num;
    // return data.bits;
    memcpy(&value, &num, sizeof(double));

    return value;
}


#else // NAN_BOXING


typedef enum {
    VAL_BOOL,
    VAL_NIL,
    VAL_NUMBER,
    VAL_OBJ
} ValueType;

typedef struct {
    ValueType type;
    union {
        bool boolean;
        double number;
        Obj * obj;
    } as;
} Value;

#define IS_BOOL(value)    ((value).type == VAL_BOOL)
#define IS_NIL(value)     ((value).type == VAL_NIL)
#define IS_NUMBER(value)  ((value).type == VAL_NUMBER)
#define IS_OBJ(value)     ((value).type == VAL_OBJ)

#define AS_BOOL(value)    ((value).as.boolean)
#define AS_NUMBER(value)  ((value).as.number)
#define AS_OBJ(value)     ((value).as.obj)

#define BOOL_VAL(value)   ((Value){VAL_BOOL, {.boolean = value}})
#define NIL_VAL           ((Value){VAL_NIL, {.number = 0}})
#define NUMBER_VAL(value) ((Value){VAL_NUMBER, {.number = value}})
#define OBJ_VAL(object)   ((Value){VAL_OBJ, {.obj = (Obj *) object}})

#endif // NAN_BOXING

/**
 * Dynamic array structure to hold Values.
 **/
typedef struct {
    int capacity;
    int count;
    Value * values;
} ValueArray;


/**
 * Check if two values are equal.
 **/
bool valuesEqual (Value a, Value b);

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

