/*
 * main.c
 */

#include <stdio.h>
#include <string.h>
#include "util.h"
#include "symbol.h"
#include "types.h"
#include "absyn.h"
#include "errormsg.h"
#include "temp.h" /* needed by translate.h */
#include "tree.h" /* needed by frame.h */
#include "assem.h"
#include "frame.h" /* needed by translate.h and printfrags prototype */
#include "translate.h"
#include "env.h"
#include "semant.h" /* function prototype for transProg */
#include "canon.h"
#include "prabsyn.h"
#include "printtree.h"
//#include "escape.h" /* needed by escape analysis */
#include "parse.h"
#include "codegen.h"
#include "regalloc.h"
#include "stdlib.h"

extern bool anyErrors;

typedef struct Flags_ Flags;

struct Flags_ {
    bool f_print_tree;
    bool f_major_s;
    bool f_escape_analysis;
    bool f_verbose;
    const char *object_name;
    const char* runtime_path;
};

/* print the assembly language instructions to filename.s */
static void doProc(FILE *out, F_frame frame, T_stm body, bool verbose) {
    AS_proc proc;
    T_stmList stmList;
    AS_instrList iList;

    F_tempMap = Temp_empty();
    //printStmList(stdout, T_StmList(body, NULL));

    stmList = C_linearize(body);
    stmList = C_traceSchedule(C_basicBlocks(stmList));
    if (verbose) {
        printStmList(stdout, stmList);
    }
    iList = F_codegen(frame, stmList); /* 9 */
    //AS_printInstrList (out, iList, Temp_layerMap(F_tempMap,Temp_name()));

    struct RA_result ra = RA_regAlloc(frame, iList, verbose);  /* 10, 11 */
    iList = ra.il;

    iList = F_procEntryExit2(iList);
    proc = F_procEntryExit3(frame, iList);

    fprintf(out, "%s\n", proc->prolog);
    AS_printInstrList(out, proc->body,
                      Temp_layerMap(F_tempMap, Temp_layerMap(ra.coloring, Temp_name())));
    fprintf(out, "%s\n", proc->epilog);

//  fprintf(out, "BEGIN function\n");
//  AS_printInstrList (out, iList,
//                     Temp_layerMap(F_tempMap, Temp_layerMap(ra.coloring, Temp_name())));
//  fprintf(out, "END function\n\n");
}

char *expand_escapes(const char *src) {
    char *str = checked_malloc(2 * strlen(src) + 10);

    char *dest = str;
    char c;

    while ((c = *(src++))) {
        switch (c) {
            case '\a':
                *(dest++) = '\\';
                *(dest++) = 'a';
                break;
            case '\b':
                *(dest++) = '\\';
                *(dest++) = 'b';
                break;
            case '\t':
                *(dest++) = '\\';
                *(dest++) = 't';
                break;
            case '\n':
                *(dest++) = '\\';
                *(dest++) = 'n';
                break;
            case '\v':
                *(dest++) = '\\';
                *(dest++) = 'v';
                break;
            case '\f':
                *(dest++) = '\\';
                *(dest++) = 'f';
                break;
            case '\r':
                *(dest++) = '\\';
                *(dest++) = 'r';
                break;
            case '\\':
                *(dest++) = '\\';
                *(dest++) = '\\';
                break;
            case '\"':
                *(dest++) = '\\';
                *(dest++) = '\"';
                break;
            default:
                *(dest++) = c;
        }
    }

    *(dest++) = '\\';
    *(dest++) = '0';

    *(dest++) = '\\';
    *(dest++) = '0';

    *(dest++) = '\\';
    *(dest++) = '0';

    *(dest++) = '\0'; /* Ensure nul terminator */
    return str;
}

static void doStr(FILE *out, string str, Temp_label label) {
    fprintf(out, "%s:\n", Temp_labelstring(label));
    fprintf(out, "    .long 0x%lx\n", strlen(str));
    fprintf(out, "    .ascii \"%s\"\n", expand_escapes(str));
    fprintf(out, "\n");
}

static int parse_flags(string *flags, Flags *fs) {
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

static void assemble(const char *filename, const char *obj_name, const char *runtime_path) {
    assert(filename);
    assert(runtime_path);
    char command[256];
    const char *gcc_compiler_flags = "-m32 -g";
    sprintf(command, "%s -o %s %s %s %s", "gcc ", obj_name ? obj_name : "a.out", runtime_path, gcc_compiler_flags, filename);
    system(command);
}

int main(int argc, char **argv) {
    A_exp absyn_root;
    S_table base_env, base_tenv;
    F_fragList frags, fl;
    Flags fs = {};
    char outfile[100];
    FILE *out = stdout;

    if (argc < 2) {
        EM_error(0, "usage: tiger file.tig");
        return 1;
    }

    int last = parse_flags(argv, &fs);

    absyn_root = parse(argv[last]);
    if (!absyn_root)
        return 1;

    if (fs.f_print_tree) {
        pr_exp(out, absyn_root, 0); /* print absyn data structure */
        fprintf(out, "\n");
    }

    if (fs.f_escape_analysis) {
        //If you have implemented escape analysis, uncomment this
        Esc_findEscape(absyn_root); /* set varDec's escape field */
    }

    frags = SEM_transProg(absyn_root);
    if (anyErrors) return 1; /* don't continue */

    /* convert the filename */
    sprintf(outfile, "%s.s", argv[last]);
    out = fopen(outfile, "w");

    fprintf(out, ".globl tigermain\n\n");
    /* Chapter 8, 9, 10, 11 & 12 */
    fprintf(out, ".text\n\n");
    for (fl = frags; fl; fl = fl->tail)
        if (fl->head->kind == F_procFrag)
            doProc(out, fl->head->u.proc.frame, fl->head->u.proc.body, fs.f_verbose);

    fprintf(out, ".data\n\n");
    for (fl = frags; fl; fl = fl->tail)
        if (fl->head->kind == F_stringFrag) {
            doStr(out, fl->head->u.stringg.str, fl->head->u.stringg.label);
        }

    fclose(out);

    if (!fs.f_major_s) {
        assemble(outfile, fs.object_name, fs.runtime_path);
    }

    return 0;
}
