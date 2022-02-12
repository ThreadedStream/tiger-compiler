#include <string.h>
#include "base.h"
#include "util.h"


int parseFlags(string *flags, B_Flags *fs) {
    string f = NULL;
    int i = 0;
    while ((f = *(flags + i))) {
        if (!strcmp(f, "-printtree")) {
            fs->f_print_tree = TRUE;
        } else if (!strcmp(f, "-S")) {
            fs->f_major_s = TRUE;
        } else if (!strcmp(f, "-escape")) {
            fs->f_escape_analysis = TRUE;
        } else if (!strcmp(f, "-verbose")) {
            fs->f_verbose = TRUE;
        } else if (!strcmp(f, "-o")) {
            assert(*(flags + i + 1));
            fs->object_name = strdup(*(flags + i + 1));
        } else if (!strcmp(f, "-runtime")) {
            assert(*(flags + i + 1));
            fs->runtime_path = strdup(*(flags + i + 1));
        }
        i++;
    }
    return i - 1;
}
