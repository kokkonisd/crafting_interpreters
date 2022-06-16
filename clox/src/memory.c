#include <stdlib.h>
#include <stdio.h>

#include "compiler.h"
#include "memory.h"
#include "vm.h"


#ifdef DEBUG_LOG_GC
#include "debug.h"
#endif

#define GC_HEAP_GROW_FACTOR 2


void * reallocate (void * pointer, size_t oldSize, size_t newSize)
{
    vm.bytesAllocated += newSize - oldSize;

    if (newSize > oldSize) {
#       ifdef DEBUG_STRESS_GC
            collectGarbage();
#       endif

        if (vm.bytesAllocated > vm.nextGC) {
            collectGarbage();
        }
    }

    if (newSize == 0) {
        free(pointer);
        return NULL;
    }

    void * result = realloc(pointer, newSize);
    if (result == NULL) {
        printf("clox: out of memory.\n");
        exit(1);
    }

    return result;
}


void markObject (Obj * object)
{
    if (object == NULL) return;
    if (object->isMarked) return;

#   ifdef DEBUG_LOG_GC
        printf("%p mark ", (void *) object);
        printValue(OBJ_VAL(object));
        printf("\n");
#   endif

    object->isMarked = true;

    // Add object to the worklist (because it's now colored gray, meaning it's being
    // processed by the GC (but we're not finished yet).
    if (vm.grayCapacity < vm.grayCount + 1) {
        vm.grayCapacity = GROW_CAPACITY(vm.grayCapacity);
        // We use "manual" `realloc()` here instead of the `reallocate()` wrapper
        // because we don't want growing the stack during a GC run to cause the GC
        // to recursively start a new run.
        vm.grayStack = (Obj **) realloc(vm.grayStack, sizeof(Obj *) * vm.grayCapacity);

        // If there's no more space for the gray stack, then we have to shut down.
        if (vm.grayStack == NULL) exit(1);
    }

    vm.grayStack[vm.grayCount++] = object;
}


void markValue (Value value)
{
    if (IS_OBJ(value)) markObject(AS_OBJ(value));
}


static void markArray (ValueArray * array)
{
    for (int i = 0; i < array->count; i++) {
        markValue(array->values[i]);
    }
}


static void blackenObject (Obj * object)
{
#   ifdef DEBUG_LOG_GC
        printf("%p blacken ", (void *) object);
        printValue(OBJ_VAL(object));
        printf("\n");
#   endif

    switch (object->type) {
        case OBJ_CLASS: {
            ObjClass * klass = (ObjClass *) object;
            markObject((Obj *) klass->name);
            break;
        }
        case OBJ_CLOSURE: {
            ObjClosure * closure = (ObjClosure *) object;
            markObject((Obj *) closure->function);
            for (int i = 0; i < closure->upvalueCount; i++) {
                markObject((Obj *) closure->upvalues[i]);
            }
            break;
        }
        case OBJ_FUNCTION: {
            ObjFunction * function = (ObjFunction *) object;
            markObject((Obj *) function->name);
            markArray(&function->chunk.constants);
            break;
        }
        case OBJ_INSTANCE: {
            ObjInstance * instance = (ObjInstance *) object;
            markObject((Obj *) instance->klass);
            markTable(&instance->fields);
            break;
        }
        case OBJ_UPVALUE:
            markValue(((ObjUpvalue *) object)->closed);
            break;
        case OBJ_NATIVE:
        case OBJ_STRING:
            // Nothing to traverse here.
            break;
    }
}


static void freeObject (Obj * object)
{
#   ifdef DEBUG_LOG_GC
        printf("%p free type %d\n", (void *) object, object->type);
#   endif

    switch (object->type) {
        case OBJ_CLASS: {
            FREE(ObjClass, object);
            break;
        }
        case OBJ_CLOSURE: {
            ObjClosure * closure = (ObjClosure *) object;
            FREE_ARRAY(ObjUpvalue *, closure->upvalues, closure->upvalueCount);
            FREE(ObjClosure, object);
            break;
        }
        case OBJ_FUNCTION: {
            ObjFunction * function = (ObjFunction *) object;
            freeChunk(&function->chunk);
            FREE(ObjFunction, object);
            break;
        }
        case OBJ_INSTANCE: {
            ObjInstance * instance = (ObjInstance *) object;
            freeTable(&instance->fields);
            FREE(ObjInstance, object);
            break;
        }
        case OBJ_NATIVE: {
            FREE(ObjNative, object);
            break;
        }
        case OBJ_STRING: {
            ObjString * string = (ObjString *) object;
            FREE_ARRAY(char, string->chars, string->length + 1);
            FREE(ObjString, object);
            break;
        }
        case OBJ_UPVALUE: {
            FREE(ObjUpvalue, object);
            break;
        }
    }
}


static void markRoots ()
{
    // Marking the stack.
    for (Value * slot = vm.stack; slot < vm.stackTop; slot++) {
        markValue(*slot);
    }

    // Marking the callframe stack.
    for (int i = 0; i < vm.frameCount; i++) {
        markObject((Obj *) vm.frames[i].closure);
    }

    // Marking the upvalue list.
    for (
        ObjUpvalue * upvalue = vm.openUpvalues;
        upvalue != NULL;
        upvalue = upvalue->next
    ) {
        markObject((Obj *) upvalue);
    }

    // Marking the hash table (for globals).
    markTable(&vm.globals);

    // Marking the roots created by the compiler (before the program runs).
    markCompilerRoots();
}


static void traceReferences ()
{
    while (vm.grayCount > 0) {
        Obj * object = vm.grayStack[--vm.grayCount];
        blackenObject(object);
    }
}


static void sweep ()
{
    Obj * previous = NULL;
    Obj * object = vm.objects;

    while (object != NULL) {
        if (object->isMarked) {
            // The object is marked; skip it.
            // We need to reset the `isMarked` field, so that we're ready for the next
            // run.
            object->isMarked = false;
            previous = object;
            object = object->next;
        } else {
            // The object is unmarked, thus unreachable.
            // Before we free it, we need to remove it from the linked list and
            // reestablish the link between the objects.
            Obj * unreached = object;
            object = object->next;
            if (previous != NULL) {
                previous->next = object;
            } else {
                vm.objects = object;
            }

            freeObject(unreached);
        }
    }
}


void collectGarbage ()
{
#   ifdef DEBUG_LOG_GC
        printf("-- gc begin\n");
        size_t before = vm.bytesAllocated;
#   endif

    markRoots();
    traceReferences();
    // Remove dangling references to strings that are going to be freed in the VM's
    // string intern table.
    tableRemoveWhite(&vm.strings);
    sweep();

    // Adjust the threshold of the next GC run.
    vm.nextGC = vm.bytesAllocated * GC_HEAP_GROW_FACTOR;

#   ifdef DEBUG_LOG_GC
        printf("-- gc end\n");
        printf(
            "   collected %zu bytes (from %zu to %zu) next at %zu\n",
            before - vm.bytesAllocated,
            before,
            vm.bytesAllocated,
            vm.nextGC
        );
#   endif
}


void freeObjects ()
{
    Obj * object = vm.objects;

    while (object != NULL) {
        Obj * next = object->next;
        freeObject(object);
        object = next;
    }

    free(vm.grayStack);
}

