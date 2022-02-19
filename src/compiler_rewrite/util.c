/*
 * util.c - commonly used utility functions.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "util.h"

void *checked_malloc(int len) {
    void *p = malloc(len);
    if (!p) {
        fprintf(stderr, "\nRan out of memory!\n");
        exit(1);
    }
    return p;
}

string String(char *s) {
    string p = checked_malloc(strlen(s) + 1);
    strcpy(p, s);
    return p;
}

U_boolList U_BoolList(bool head, U_boolList tail) {
    U_boolList list = checked_malloc(sizeof(*list));
    list->head = head;
    list->tail = tail;
    return list;
}

int U_listSize(U_boolList l) {
    int i = 0;
    for (; l; l = l->tail) {
        i++;
    }
    return i;
}

string Strcat(char *dest, string src) {
    u32 destLen = strlen(dest);
    u32 srcLen = strlen(src);
    char *buffer = checked_malloc((int)(destLen + srcLen));
    strcat(buffer, dest);
    strcat(buffer, src);
    return buffer;
}

string Sprintf(string format, ...) {
    char *str = checked_malloc(512);
    va_list args;
    va_start(args, format);
    vsprintf(str, format, args);
    va_end(args);
    return str;
}