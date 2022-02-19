#pragma once
#include <assert.h>

typedef char *string;
typedef char bool;
typedef unsigned long u32;

#define TRUE 1
#define FALSE 0

void *checked_malloc(int);
string String(char *);
string Sprintf(string format, ...);
string Strcat(char* dest, string src);

typedef struct U_boolList_ *U_boolList;
struct U_boolList_ {bool head; U_boolList tail;};
U_boolList U_BoolList(bool head, U_boolList tail);
int U_listSize(U_boolList l);
