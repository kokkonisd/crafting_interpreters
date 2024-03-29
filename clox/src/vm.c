#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "object.h"
#include "memory.h"
#include "vm.h"


VM vm;


// Native clock function.
static Value clockNative (int argCount, Value * args)
{
    return NUMBER_VAL((double) clock() / CLOCKS_PER_SEC);
}


static void resetStack ()
{
    vm.stackTop = vm.stack;
    vm.frameCount = 0;
    vm.openUpvalues = NULL;
}


static void runtimeError (const char * format, ...)
{
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);


    // Print the stack trace.
    for (int i = vm.frameCount - 1; i >= 0; i--) {
        CallFrame * frame = &vm.frames[i];
        ObjFunction * function = frame->closure->function;
        size_t instruction = frame->ip - function->chunk.code - 1;

        fprintf(stderr, "[line %d] in ", function->chunk.lines[instruction]);
        if (function->name == NULL) fprintf(stderr, "script\n");
        else fprintf(stderr, "%s()\n", function->name->chars);
    }

    resetStack();
}


static void defineNative (const char * name, NativeFn function)
{
    // Store the native function as a global variable.
    push(OBJ_VAL(copyString(name, (int) strlen(name))));
    push(OBJ_VAL(newNative(function)));
    tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);
    pop();
    pop();
}


void initVM ()
{
    resetStack();
    vm.objects = NULL;
    vm.bytesAllocated = 0;
    vm.nextGC = 1024 * 1024;

    // Initialize the working list stack for the GC.
    vm.grayCount = 0;
    vm.grayCapacity = 0;
    vm.grayStack = NULL;

    initTable(&vm.globals);
    initTable(&vm.strings);

    // Create & intern the "init" string to allow faster access to init methods.
    // We need to initialize it to NULL first to make sure the GC doesn't attempt to
    // read it mid-allocation.
    vm.initString = NULL;
    vm.initString = copyString("init", 4);

    // Define native functions.
    defineNative("clock", clockNative);
}


void freeVM ()
{
    freeTable(&vm.globals);
    freeTable(&vm.strings);

    // Set the interned "init" string to NULL since it's about to be freed. That way,
    // we're not leaving a dangling pointer.
    vm.initString = NULL;

    freeObjects();
}


void push (Value value)
{
    *vm.stackTop = value;
    vm.stackTop++;
}


Value pop ()
{
    vm.stackTop--;
    return *vm.stackTop;
}


static Value peek (int distance)
{
    return vm.stackTop[-1 - distance];
}


static bool call (ObjClosure * closure, int argCount)
{
    if (argCount != closure->function->arity) {
        runtimeError(
            "Expected %d arguments but got %d.", closure->function->arity, argCount
        );
        return false;
    }

    if (vm.frameCount == FRAMES_MAX) {
        runtimeError("Stack overflow.");
        return false;
    }

    CallFrame * frame = &vm.frames[vm.frameCount++];
    frame->closure = closure;
    frame->ip = closure->function->chunk.code;
    frame->slots = vm.stackTop - argCount - 1;

    return true;
}


static bool callValue (Value callee, int argCount)
{
    if (IS_OBJ(callee)) {
        switch (OBJ_TYPE(callee)) {
            case OBJ_BOUND_METHOD: {
                ObjBoundMethod * bound = AS_BOUND_METHOD(callee);
                // Put the receiver in the 0th slot.
                vm.stackTop[-argCount - 1] = bound->receiver;
                return call(bound->method, argCount);
            }
            case OBJ_CLASS: {
                ObjClass * klass = AS_CLASS(callee);
                vm.stackTop[-argCount - 1] = OBJ_VAL(newInstance(klass));

                // If there is an `init()` method, call it.
                Value initializer;
                if (tableGet(&klass->methods, vm.initString, &initializer)) {
                    return call(AS_CLOSURE(initializer), argCount);
                } else if (argCount != 0) {
                    runtimeError("Expected 0 arguments but got %d.", argCount);
                    return false;
                }

                return true;
            }
            case OBJ_CLOSURE:
                return call(AS_CLOSURE(callee), argCount);
            case OBJ_NATIVE: {
                NativeFn native = AS_NATIVE(callee);
                Value result = native(argCount, vm.stackTop - argCount);
                vm.stackTop -= argCount + 1;
                push(result);
                return true;
            }
            default:
                break; // Non-callable object type.
        }
    }

    runtimeError("Can only call functions and classes.");

    return false;
}


static bool invokeFromClass (ObjClass * klass, ObjString * name, int argCount)
{
    Value method;
    if (!tableGet(&klass->methods, name, &method)) {
        runtimeError("Undefined property '%s'.", name->chars);

        return false;
    }

    return call(AS_CLOSURE(method), argCount);
}


static bool invoke (ObjString * name, int argCount)
{
    Value receiver = peek(argCount);
    if (!IS_INSTANCE(receiver)) {
        runtimeError("Only instances have methods.");
    }

    ObjInstance * instance = AS_INSTANCE(receiver);

    // If there is a field with the same name, we fetch the field instead of attempting
    // to invoke a method.
    Value value;
    if (tableGet(&instance->fields, name, &value)) {
        vm.stackTop[-argCount - 1] = value;

        return callValue(value, argCount);
    }
    
    return invokeFromClass(instance->klass, name, argCount);
}


static bool bindMethod (ObjClass * klass, ObjString * name)
{
    Value method;
    if (!tableGet(&klass->methods, name, &method)) {
        runtimeError("Undefined property '%s'.", name->chars);
        return false;
    }

    // The receiver is at the top of the stack.
    ObjBoundMethod * bound = newBoundMethod(peek(0), AS_CLOSURE(method));
    // Pop the instance off the stack.
    pop();
    // Push the bound method.
    push(OBJ_VAL(bound));

    return true;
}


static ObjUpvalue * captureUpvalue (Value * local)
{
    ObjUpvalue * prevUpvalue = NULL;
    ObjUpvalue * upvalue = vm.openUpvalues;

    while (upvalue != NULL && upvalue->location > local) {
        prevUpvalue = upvalue;
        upvalue = upvalue->next;
    }

    if (upvalue != NULL && upvalue->location == local) {
        // An upvalue already exists; no need to create a new one.
        return upvalue;
    }

    // No upvalue found; we need to create one.
    ObjUpvalue * createdUpvalue = newUpvalue(local);
    createdUpvalue->next = upvalue;

    if (prevUpvalue == NULL) {
        vm.openUpvalues = createdUpvalue;
    } else {
        prevUpvalue->next = createdUpvalue;
    }

    return createdUpvalue;
}


static void closeUpvalues (Value * last)
{
    while (vm.openUpvalues != NULL && vm.openUpvalues->location >= last) {
        ObjUpvalue * upvalue = vm.openUpvalues;
        // Copy the variable's value on the heap, since it will get popped off the
        // stack.
        upvalue->closed = *upvalue->location;
        // Make the location to point to the `closed` field; that way, we know that the
        // upvalue has been closed.
        upvalue->location = &upvalue->closed;
        vm.openUpvalues = upvalue->next;
    }
}


static void defineMethod (ObjString * name)
{
    Value method = peek(0);
    ObjClass * klass = AS_CLASS(peek(1));
    tableSet(&klass->methods, name, method);
    pop();
}


static bool isFalsey (Value value)
{
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}


static void concatenate ()
{
    // Instead of popping the strings eagerly, peek them instead; this will allow the
    // GC to find them on the stack.
    ObjString * b = AS_STRING(peek(0));
    ObjString * a = AS_STRING(peek(1));

    int length = a->length + b->length;
    char * chars = ALLOCATE(char, length + 1);
    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';

    ObjString * result = takeString(chars, length);

    // Now that the new string is created, we can pop the two old strings off the stack.
    pop();
    pop();

    push(OBJ_VAL(result));
}


static InterpretResult run ()
{
    CallFrame * frame = &vm.frames[vm.frameCount - 1];

#   define READ_BYTE() (*frame->ip++)
#   define READ_SHORT() \
        (frame->ip += 2, (uint16_t) ((frame->ip[-2] << 8) | frame->ip[-1]))
#   define READ_CONSTANT() \
        (frame->closure->function->chunk.constants.values[READ_BYTE()])
#   define READ_STRING() AS_STRING(READ_CONSTANT())
// This is a macro hack to make sure this macro works everywhere.
// We want to make sure all instructions share the same scope; you might think we could
// just wrap them in {}, but that wouldn't be "semicolon-at-the-end"-friendly (meaning
// we couldn't do `BINARY_OP(+);`). So instead we use this dummy do-while loop that only
// runs once.
#   define BINARY_OP(valueType, op) \
        do { \
            if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) { \
                runtimeError("Operands must be numbers."); \
                return INTERPRET_RUNTIME_ERROR; \
            } \
            double b = AS_NUMBER(pop()); \
            double a = AS_NUMBER(pop()); \
            push(valueType(a op b)); \
        } while(false)

    for (;;) {
#       ifdef DEBUG_TRACE_EXECUTION
            printf("STACK     ");
            for (Value * slot = vm.stack; slot < vm.stackTop; slot++) {
                printf("[ ");
                printValue(*slot);
                printf(" ]");
            }
            printf("\n");
            disassembleInstruction(
                &frame->closure->function->chunk,
                (int)(frame->ip - frame->closure->function->chunk.code)
            );
#       endif

        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
            case OP_CONSTANT: {
                Value constant = READ_CONSTANT();
                push(constant);
                break;
            }
            case OP_NIL: push(NIL_VAL); break;
            case OP_TRUE: push(BOOL_VAL(true)); break;
            case OP_FALSE: push(BOOL_VAL(false)); break;
            case OP_POP: pop(); break;
            case OP_GET_LOCAL: {
                uint8_t slot = READ_BYTE();
                push(frame->slots[slot]);
                break;
            }
            case OP_SET_LOCAL: {
                uint8_t slot = READ_BYTE();
                frame->slots[slot] = peek(0);
                break;
            }
            case OP_GET_GLOBAL: {
                ObjString * name = READ_STRING();
                Value value;
                if (!tableGet(&vm.globals, name, &value)) {
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(value);
                break;
            }
            case OP_DEFINE_GLOBAL: {
                ObjString * name = READ_STRING();
                tableSet(&vm.globals, name, peek(0));
                pop();
                break;
            }
            case OP_SET_GLOBAL: {
                ObjString * name = READ_STRING();
                if (tableSet(&vm.globals, name, peek(0))) {
                    tableDelete(&vm.globals, name);
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_GET_UPVALUE: {
                uint8_t slot = READ_BYTE();
                push(*frame->closure->upvalues[slot]->location);
                break;
            }
            case OP_SET_UPVALUE: {
                uint8_t slot = READ_BYTE();
                *frame->closure->upvalues[slot]->location = peek(0);
                break;
            }
            case OP_GET_PROPERTY: {
                if (!IS_INSTANCE(peek(0))) {
                    runtimeError("Only instances have properties.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjInstance * instance = AS_INSTANCE(peek(0));
                ObjString * name = READ_STRING();

                Value value;
                if (tableGet(&instance->fields, name, &value)) {
                    pop(); // Instance.
                    push(value);
                    break;
                }

                // If it's not a field, it might be a (bound) method.
                // If the call to `bindMethod()` succeedes, it will place the method on
                // the stack and return true.
                if (!bindMethod(instance->klass, name)) {
                    return INTERPRET_RUNTIME_ERROR;
                }

                break;
            }
            case OP_SET_PROPERTY: {
                if (!IS_INSTANCE(peek(1))) {
                    runtimeError("Only instances have fields.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                // When we execute this, the top of the stack looks like this:
                //
                // ------------------------
                // ... | instance | value |
                // ------------------------
                //
                // We need to fetch the instance, store the value in the table, pop
                // the value (and hold on to it) and the instance, and then push the
                // value back on the stack. This is because the result of this setter
                // expression is the assigned value, so we need to put it on the stack
                // so that we can potentially use it (e.g. in a `print`).
                ObjInstance * instance = AS_INSTANCE(peek(1));
                tableSet(&instance->fields, READ_STRING(), peek(0));
                // Get the value.
                Value value = pop();
                // Pop the instance off the stack.
                pop();
                // Put value back, to be used as the result of the expression.
                push(value);
                break;
            }
            case OP_GET_SUPER: {
                ObjString * name = READ_STRING();
                ObjClass * superclass = AS_CLASS(pop());

                if (!bindMethod(superclass, name)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_EQUAL: {
                Value b = pop();
                Value a = pop();
                push(BOOL_VAL(valuesEqual(a, b)));
                break;
            }
            case OP_GREATER: BINARY_OP(BOOL_VAL, >); break;
            case OP_LESS: BINARY_OP(BOOL_VAL, <); break;
            case OP_ADD: {
                if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
                    concatenate();
                } else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
                    double b = AS_NUMBER(pop());
                    double a = AS_NUMBER(pop());
                    push(NUMBER_VAL(a + b));
                } else {
                    runtimeError("Operands must be two numbers or two strings.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_SUBTRACT: {
                BINARY_OP(NUMBER_VAL, -);
                break;
            }
            case OP_MULTIPLY: {
                BINARY_OP(NUMBER_VAL, *);
                break;
            }
            case OP_MODULO: {
                // We can't call BINARY_OP here, because we need to cast the two values
                // to integers first for it to work. So we won't use a macro, since we
                // only need this operation here.
                if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) {
                    runtimeError("Operands must be numbers.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                int b = (int) AS_NUMBER(pop());
                int a = (int) AS_NUMBER(pop());
                push(NUMBER_VAL((double) (a % b)));
                break;
            }
            case OP_DIVIDE: {
                BINARY_OP(NUMBER_VAL, /);
                break;
            }
            case OP_NOT: {
                push(BOOL_VAL(isFalsey(pop())));
                break;
            }
            case OP_NEGATE: {
                if (!IS_NUMBER(peek(0))) {
                    runtimeError("Operand must be a number.");
                        return INTERPRET_RUNTIME_ERROR;
                }
                push(NUMBER_VAL(-AS_NUMBER(pop())));
                break;
            }
            case OP_PRINT: {
                printValue(pop());
                printf("\n");
                break;
            }
            case OP_JUMP: {
                uint16_t offset = READ_SHORT();
                frame->ip += offset;
                break;
            }
            case OP_JUMP_IF_FALSE: {
                uint16_t offset = READ_SHORT();
                if (isFalsey(peek(0))) frame->ip += offset;
                break;
            }
            case OP_LOOP: {
                uint16_t offset = READ_SHORT();
                // Loops jump backwards.
                frame->ip -= offset;
                break;
            }
            case OP_CALL: {
                int argCount = READ_BYTE();
                if (!callValue(peek(argCount), argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm.frames[vm.frameCount - 1];
                break;
            }
            case OP_INVOKE: {
                ObjString * method = READ_STRING();
                int argCount = READ_BYTE();
                if (!invoke(method, argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm.frames[vm.frameCount - 1];
                break;
            }
            case OP_SUPER_INVOKE: {
                ObjString * method = READ_STRING();
                int argCount = READ_BYTE();
                ObjClass * superclass = AS_CLASS(pop());

                if (!invokeFromClass(superclass, method, argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }

                frame = &vm.frames[vm.frameCount - 1];
                break;
            }
            case OP_CLOSURE: {
                ObjFunction * function = AS_FUNCTION(READ_CONSTANT());
                ObjClosure * closure = newClosure(function);
                push(OBJ_VAL(closure));

                for (int i = 0; i < closure->upvalueCount; i++) {
                    uint8_t isLocal = READ_BYTE();
                    uint8_t index = READ_BYTE();
                    if (isLocal) {
                        closure->upvalues[i] = captureUpvalue(frame->slots + index);
                    } else {
                        closure->upvalues[i] = frame->closure->upvalues[index];
                    }
                }

                break;
            }
            case OP_CLOSE_UPVALUE: {
                closeUpvalues(vm.stackTop - 1);
                pop();
                break;
            }
            case OP_RETURN: {
                Value result = pop();
                // Closing the function's parameters and any locals declared immediately
                // inside the function.
                closeUpvalues(frame->slots);
                vm.frameCount--;
                if (vm.frameCount == 0) {
                    // This was the very last frame; we've finished executing the
                    // top-level code.
                    // We need to pop the main <script> function and exit the
                    // interpreter.
                    pop();
                    return INTERPRET_OK;
                }

                // Discard all the parameter slots that the callee was using.
                vm.stackTop = frame->slots;
                push(result);
                frame = &vm.frames[vm.frameCount - 1];
                break;
            }
            case OP_CLASS: {
                push(OBJ_VAL(newClass(READ_STRING())));
                break;
            }
            case OP_INHERIT: {
                Value superclass = peek(1);

                // Safeguard against inheriting from non-class objects.
                if (!IS_CLASS(superclass)) {
                    runtimeError("Superclass must be a class.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjClass * subclass = AS_CLASS(peek(0));
                // Copy the methods of the superclass in the subclass.
                tableAddAll(&AS_CLASS(superclass)->methods, &subclass->methods);
                pop(); // Subclass.
                break;
            }
            case OP_METHOD: {
                defineMethod(READ_STRING());
                break;
            }
        }
     }
#    undef READ_BYTE
#    undef READ_SHORT
#    undef READ_CONSTANT
#    undef READ_STRING
#    undef BINARY_OP
}


InterpretResult interpret (const char * source)
{
    ObjFunction * function = compile(source);
    if (function == NULL) return INTERPRET_COMPILE_ERROR;

    push(OBJ_VAL(function));
    ObjClosure * closure = newClosure(function);
    pop();
    push(OBJ_VAL(closure));
    call(closure, 0);

    return run();
}

