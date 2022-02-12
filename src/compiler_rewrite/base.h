#pragma once

typedef struct B_Flags_ B_Flags;

enum {
    x86,
    AMD64,
} B_targetArch;

extern int targetArch;

struct B_Flags_ {
    bool f_print_tree;
    bool f_major_s;
    bool f_escape_analysis;
    bool f_verbose;
    const char *object_name;
    const char *runtime_path;
};


int parseFlags(string *flags, B_Flags *fs);