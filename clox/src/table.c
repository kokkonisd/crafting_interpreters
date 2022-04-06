#include <stdlib.h>
#include <string.h>


#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"


#define TABLE_MAX_LOAD 0.75


void initTable (Table * table)
{
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}


void freeTable (Table * table)
{
    FREE_ARRAY(Entry, table->entries, table->capacity);
    initTable(table);
}


static Entry * findEntry (Entry * entries, int capacity, ObjString * key)
{
    // We are using open addressing to handle collisions, which means that if a
    // collision is detected, we'll find a different bucket to put the key in.
    //
    // For probing (i.e. figuring out which bucket we'll add the key to in case of a
    // collision), we use linear probing, meaning that we'll just pick the next
    // available bucket in the bucket array (wrapping up to the beginning if we reach
    // the end).
    uint32_t index = key->hash % capacity;
    Entry * tombstone = NULL;

    for (;;) {
        Entry * entry = &entries[index];

        if (entry->key == NULL) {
            if (IS_NIL(entry->value)) {
                // Empty entry.
                return tombstone != NULL ? tombstone : entry;
            } else {
                // We found a tombstone (see tableDelete for a more detailed
                // explanation).
                if (tombstone == NULL) tombstone = entry;
            }
        } else if (entry->key == key) {
            // We found the key.
            return entry;
        }

        index = (index + 1) % capacity;
    }
}


static void adjustCapacity (Table * table, int capacity)
{
    Entry * entries = ALLOCATE(Entry, capacity);

    for (int i = 0; i < capacity; i++) {
        entries[i].key = NULL;
        entries[i].value = NIL_VAL;
    }

    // Clear count, otherwise we'll take tombstones into account (we don't want that).
    table->count = 0;
    for (int i = 0; i < table->capacity; i++) {
        Entry * entry = &table->entries[i];
        if (entry->key == NULL) continue;

        Entry * dest = findEntry(entries, capacity, entry->key);
        dest->key = entry->key;
        dest->value = entry->value;
        table->count++;
    }

    FREE_ARRAY(Entry, table->entries, table->capacity);
    table->entries = entries;
    table->capacity = capacity;
}


bool tableGet (Table * table, ObjString * key, Value * value)
{
    if (table->count == 0) return false;

    Entry * entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL) return false;

    *value = entry->value;

    return true;
}


bool tableSet (Table * table, ObjString * key, Value value)
{
    if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
        int capacity = GROW_CAPACITY(table->capacity);
        adjustCapacity(table, capacity);
    }

    Entry * entry = findEntry(table->entries, table->capacity, key);
    bool isNewKey = entry->key == NULL;
    if (isNewKey && IS_NIL(entry->value)) table->count++;

    entry->key = key;
    entry->value = value;

    return isNewKey;
}


bool tableDelete (Table * table, ObjString * key)
{
    if (table->count == 0) return false;

    // Find the entry.
    Entry * entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL) return false;

    // Place a tombstone in the entry.
    // This is done to avoid breaking linear probing. Imagine the following scenario:
    //
    // ----------------------
    // | key1 | key2 | key3 |
    // ----------------------
    //
    // In this case, key1, key2, and key3 all collide. So key2 and key3 are supposed to
    // be in the same bucket as key1, but are placed in the next available buckets as
    // a result of linear probing after a collision.
    // If we naively delete key2, the buckets will look like this:
    //
    // ----------------------
    // | key1 | NULL | key3 |
    // ----------------------
    //
    // Consequently, key3 will never be able to be found again; the probe will look in
    // key1's bucket, then in the next bucket, it will find a NULL and determine that
    // key3 does not exist (because if it did there would not be a NULL after the
    // collision bucket). The solution to this is adding a tombstone, which is just
    // a non-NULL value to indicate that the probe should keep looking.
    entry->key = NULL;
    entry->value = BOOL_VAL(true);
    
    return true;
}


void tableAddAll (Table * from, Table * to)
{
    for (int i = 0; i < from->capacity; i++) {
        Entry * entry = &from->entries[i];
        if (entry->key != NULL) {
            tableSet(to, entry->key, entry->value);
        }
    }
}


ObjString * tableFindString (
    Table * table, const char * chars, int length, uint32_t hash
)
{
    if (table->count == 0) return NULL;

    uint32_t index = hash % table->capacity;

    for (;;) {
        Entry * entry = &table->entries[index];

        if (entry->key == NULL) {
            // Stop if we find an empty non-tombstone entry.
            if (IS_NIL(entry->value)) return NULL;
        } else if (
            entry->key->length == length
            && entry->key->hash == hash
            && memcmp(entry->key->chars, chars, length) == 0
        ) {
            // We found it.
            return entry->key;
        }

        index = (index + 1) % table->capacity;
    }
}

