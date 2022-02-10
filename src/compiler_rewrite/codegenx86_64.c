#include <stdlib.h>
#include "util.h"
#include "symbol.h"
#include "absyn.h"
#include "temp.h"
#include "errormsg.h"
#include "tree.h"
#include "assem.h"
#include "frame.h"
#include "codegenx86_64.h"
#include "table.h"

static AS_instrList iList = NULL, last = NULL;
static bool lastIsLabel = FALSE;  // reserved for "nop"
static void emit(AS_instr inst) {
    lastIsLabel = (inst->kind == I_LABEL);
    if (last != NULL) {
        last = last->tail = AS_InstrList(inst, NULL);
    } else {
        last = iList = AS_InstrList(inst, NULL);
    }
}

Temp_tempList L(Temp_temp h, Temp_tempList t) {
    return Temp_TempList(h, t);
}

static Temp_temp munchExp(T_exp e);
static void munchStm(T_stm s);
static Temp_tempList munchArgs(int i, T_expList args);
static void munchCallerSave();
static void munchCallerRestore(Temp_tempList tl);

AS_instrList F_codegen(F_frame f, T_stmList stmList) {
    AS_instrList list;
    T_stmList sl;


    for (sl = stmList; sl; sl = sl->tail) {
        munchStm(sl->head);
    }

    if (last && last->head->kind == I_LABEL) {
        emit(AS_Oper("nop\n", NULL, NULL, NULL));
    }
    list = iList;
    iList = last = NULL;
    return list;
}