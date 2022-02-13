#ifndef BASE_H
#define BASE_H

#include "util.h"

typedef struct B_Flags_ B_Flags;

extern int targetArch;

enum {
    x86,
    AMD64,
} B_targetArch;


struct B_Flags_ {
    bool f_print_tree;
    bool f_major_s;
    bool f_escape_analysis;
    bool f_verbose;
    const char *object_name;
    const char *runtime_path;
};


int parseFlags(string *flags, B_Flags *fs);

#endif