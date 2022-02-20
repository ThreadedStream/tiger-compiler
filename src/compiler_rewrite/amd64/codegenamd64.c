#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "../util.h"
#include "../symbol.h"
#include "../absyn.h"
#include "../temp.h"
#include "../errormsg.h"
#include "../tree.h"
#include "../env.h"
#include "../table.h"
#include "codegenamd64.h"


static S_table base_venv = NULL;
static bool lastIsLabel = FALSE;  // reserved for "nop"
static AS_instrList iList = NULL, last = NULL;

static void emit(AS_instr inst) {
    lastIsLabel = (bool) (inst->kind == I_LABEL);
    if (last != NULL) {
        last = last->tail = AS_InstrList(inst, NULL);
    } else {
        last = iList = AS_InstrList(inst, NULL);
    }
}

static bool isExternalCall(string name) {
    string externalFuncNames[] = {"initArray", "stringEqual", "stringCompare", "allocRecord", "print", "printi"};
    int size = sizeof(externalFuncNames) / sizeof(externalFuncNames[0]);
    for (int i = 0; i < size; i++) {
        if (!strcmp(name, externalFuncNames[i])) {
            return TRUE;
        }
    }

    return FALSE;
}

static Temp_tempList L(Temp_temp h, Temp_tempList t) {
    return Temp_TempList(h, t);
}

static Temp_temp munchCall(T_exp e);

static Temp_temp munchExp(T_exp e, string frameSize);

static Temp_temp munchConst(T_exp e);

static Temp_temp munchBinop(T_exp e, string frameSize);

static void munchStm(T_stm s, string frameSize);

static Temp_tempList munchArgs(T_expList args);

static void munchCallerSave();

static void munchCallerRestore(Temp_tempList tl, bool isLibFunc);

AS_instrList F_codegen_amd64(F_frame f, T_stmList stmList) {
    puts(Temp_labelstring(F_name(f)));
    AS_instrList list;
    T_stmList sl;
    base_venv = E_base_venv();
    string frameSize = Temp_labelstring(f->frameSize);

    for (sl = stmList; sl; sl = sl->tail) {
        munchStm(sl->head, frameSize);
    }

    if (last && last->head->kind == I_LABEL) {
        emit(AS_Oper("nop\n", NULL, NULL, NULL));
    }

    list = iList;
    iList = last = NULL;
    return list;
}

static Temp_temp munchExp(T_exp e, string frameSize) {
    char *inst = checked_malloc(sizeof(char) * 120);
    char *inst2 = checked_malloc(sizeof(char) * 120);

    switch (e->kind) {
        case T_MEM: {
            T_exp mem = e->u.MEM;
            if (mem->kind == T_BINOP) {
                if ((mem->u.BINOP.op == T_plus || mem->u.BINOP.op == T_minus)
                    && mem->u.BINOP.right->kind == T_CONST) {
                    /* MEM(BINOP(PLUS,e1,CONST(i))) */
                    T_exp e1 = mem->u.BINOP.left;
                    int i = mem->u.BINOP.right->u.CONST;
                    if (mem->u.BINOP.op == T_minus) {
                        i = -i;
                    }
                    Temp_temp r = Temp_newtemp();
                    sprintf(inst, "movq %d(`s0), `d0\n", i);
                    emit(AS_Oper(inst, L(r, NULL), L(munchExp(e1), NULL), NULL));
                    return r;
                } else if (mem->u.BINOP.op == T_plus
                           && mem->u.BINOP.left->kind == T_CONST) {
                    /* MEM(BINOP(PLUS,CONST(i),e1)) */
                    T_exp e1 = mem->u.BINOP.right;
                    int i = mem->u.BINOP.left->u.CONST;
                    Temp_temp r = Temp_newtemp();
                    sprintf(inst, "movq %d(`s0), `d0\n", i);
                    emit(AS_Oper(inst, L(r, NULL), L(munchExp(e1, frameSize), NULL), NULL));
                    return r;
                } else {
                    /* MEM(e1) */
                    T_exp e1 = mem;
                    Temp_temp r = Temp_newtemp();
                    sprintf(inst, "movq (`s0), `d0\n");
                    emit(AS_Oper(inst, L(r, NULL), L(munchExp(e1, frameSize), NULL), NULL));
                    return r;
                }
            } else if (mem->kind == T_CONST) {
                /* MEM(CONST(i)) */
                int i = mem->u.CONST;
                Temp_temp r = Temp_newtemp();
                sprintf(inst, "movq %d, `d0\n", i);
                emit(AS_Oper(inst, L(r, NULL), NULL, NULL));
                return r;
            } else {
                /* MEM(e1) */
                T_exp e1 = mem;
                Temp_temp r = Temp_newtemp();
                sprintf(inst, "movq (`s0), `d0\n");
                emit(AS_Oper(inst, L(r, NULL), L(munchExp(e1, frameSize), NULL), NULL));
                return r;
            }
        }
        case T_BINOP: {
            if (e->u.BINOP.op == T_plus
                && e->u.BINOP.right->kind == T_CONST) {
                /* BINOP(PLUS,e1,CONST(i)) */
                T_exp e1 = e->u.BINOP.left;
                int i = e->u.BINOP.right->u.CONST;
                Temp_temp r = Temp_newtemp();
                sprintf(inst, "movq `s0, `d0\n");
                emit(AS_Move(inst, L(r, NULL), L(munchExp(e1, frameSize), NULL)));
                sprintf(inst2, "addq $%d, `d0\n", i);
                emit(AS_Oper(inst2, L(r, NULL), L(r, NULL), NULL));
                return r;
            } else if (e->u.BINOP.op == T_plus
                       && e->u.BINOP.left->kind == T_CONST) {
                /* BINOP(PLUS,CONST(i),e1) */
                T_exp e1 = e->u.BINOP.right;
                int i = e->u.BINOP.left->u.CONST;
                Temp_temp r = Temp_newtemp();
                sprintf(inst, "movq `s0, `d0\n");
                emit(AS_Move(inst, L(r, NULL), L(munchExp(e1, frameSize), NULL)));
                sprintf(inst2, "addq $%d, `d0\n", i);
                emit(AS_Oper(inst2, L(r, NULL), L(r, NULL), NULL));
                return r;
            } else if (e->u.BINOP.op == T_minus
                       && e->u.BINOP.right->kind == T_CONST) {
                /* BINOP(MINUS,e1,CONST(i)) */
                T_exp e1 = e->u.BINOP.left;
                int i = e->u.BINOP.right->u.CONST;
                Temp_temp r = Temp_newtemp();
                sprintf(inst, "movq `s0, `d0\n");
                emit(AS_Move(inst, L(r, NULL), L(munchExp(e1, frameSize), NULL)));
                sprintf(inst2, "subq $%d, `d0\n", i);
                emit(AS_Oper(inst2, L(r, NULL), L(r, NULL), NULL));
                return r;
            } else if (e->u.BINOP.op == T_plus) {
                /* BINOP(PLUS,e1,e2) */
                T_exp e1 = e->u.BINOP.left, e2 = e->u.BINOP.right;
                Temp_temp r = Temp_newtemp();
                Temp_temp r1 = munchExp(e1, frameSize);
                Temp_temp r2 = munchExp(e2, frameSize);
                sprintf(inst, "movq `s0, `d0\n");
                emit(AS_Move(inst, L(r, NULL), L(r1, NULL)));
                sprintf(inst2, "addq `s0, `d0\n");
                emit(AS_Oper(inst2, L(r, NULL), L(r2, L(r, NULL)), NULL));
                return r;
            } else if (e->u.BINOP.op == T_minus) {
                /* BINOP(MINUS,e1,e2) */
                T_exp e1 = e->u.BINOP.left, e2 = e->u.BINOP.right;
                Temp_temp r = Temp_newtemp();
                Temp_temp r1 = munchExp(e1, frameSize);
                Temp_temp r2 = munchExp(e2, frameSize);
                sprintf(inst, "movq `s0, `d0\n");
                emit(AS_Move(inst, L(r, NULL), L(r1, NULL)));
                sprintf(inst2, "subq `s0, `d0\n");
                emit(AS_Oper(inst2, L(r, NULL), L(r2, L(r, NULL)), NULL));
                return r;
            } else if (e->u.BINOP.op == T_mul) {
                /* BINOP(MUL,e1,e2) */
                T_exp e1 = e->u.BINOP.left, e2 = e->u.BINOP.right;
                Temp_temp r = Temp_newtemp();
                Temp_temp r1 = munchExp(e1, frameSize);
                Temp_temp r2 = munchExp(e2, frameSize);
                sprintf(inst, "movq `s0, `d0\n");
                emit(AS_Move(inst, L(r, NULL), L(r1, NULL)));
                sprintf(inst2, "imulq `s0, `d0\n");
                emit(AS_Oper(inst2, L(r, NULL), L(r2, L(r, NULL)), NULL));
                return r;
            } else if (e->u.BINOP.op == T_div) {
                /* BINOP(DIV,e1,e2) */
                T_exp e1 = e->u.BINOP.left, e2 = e->u.BINOP.right;
                Temp_temp r = Temp_newtemp();
                Temp_temp r1 = munchExp(e1, frameSize);
                Temp_temp r2 = munchExp(e2, frameSize);
                emit(AS_Move("movq `s0, `d0\n", L(F_AX(), NULL), L(r1, NULL)));
                emit(AS_Oper("movq $0, `d0\n", L(F_DX(), NULL), NULL, NULL));
                emit(AS_Oper("divq `s0\n", L(F_AX(), L(F_DX(), NULL)),
                             L(r2, L(F_DX(), L(F_AX(), NULL))), NULL));
                emit(AS_Move("movq `s0, `d0\n", L(r, NULL), L(F_AX(), NULL)));
                return r;
            } else {
                assert(0);
                break;
            }
        }
        case T_CONST: {
            /* CONST(i) */
            return munchConst(e);
//            int i = e->u.CONST;
//            Temp_temp r = Temp_newtemp();
//            sprintf(inst, "movq $%d, `d0\n", i);
//            emit(AS_Oper(inst, L(r, NULL), NULL, NULL));
//            return r;
        }
        case T_TEMP: {
            /* TEMP(t) */
            return e->u.TEMP;
        }
        case T_NAME: {
            /* NAME(lab) */
            Temp_label lab = e->u.NAME;
            Temp_temp r = Temp_newtemp();
            sprintf(inst, "movq $%s, `d0\n", Temp_labelstring(lab));
            emit(AS_Oper(inst, L(r, NULL), NULL, NULL));
            return r;
        }
        case T_CALL: {
            /* CALL(NAME(lab),args) */
            return munchCall(e);
//            munchCallerSave();
//            Temp_label lab = e->u.CALL.fun->u.NAME;
//            bool isLibFunc = isExternalCall(Temp_labelstring(lab));
//            T_expList args = e->u.CALL.args;
//            Temp_temp t = Temp_newtemp();
//            // push static link right away, as it complicates the matters quite a bit if handled
//            // in munchArgs
//            if (!isLibFunc) {
//                Temp_temp r = munchExp(args->head);
//                emit(AS_Oper("push `s0\n", L(F_SP(), NULL), L(r, NULL), NULL));
//                args = args->tail;
//            }
//            Temp_tempList l = munchArgs(args);
//            Temp_tempList calldefs = F_callersaves();
//            sprintf(inst, "call %s\n", Temp_labelstring(lab));
//            emit(AS_Oper(inst, L(F_RV(), calldefs), l, NULL));
//            munchCallerRestore(l, isLibFunc);
//            sprintf(inst2, "movq `s0, `d0\n");
//            emit(AS_Move(inst2, L(t, NULL), L(F_RV(), NULL)));
//            return t;
        }
        default: {
            assert(0);
        }
    }
}

static Temp_temp munchBinop(T_exp exp, string frameSize) {
    char inst[512];
    char* assemInstr;
    if (exp->u.BINOP.op == T_plus && (exp->u.BINOP.right->kind == T_NAME)
        && !strcmp(Temp_labelstring(exp->u.BINOP.right->u.NAME), frameSize)) {

        Temp_temp leftReg = munchExp(exp->u.BINOP.left, frameSize);
        Temp_temp resReg  = Temp_newtemp();
        sprintf(inst, "leaq %s(`s0), `d0", frameSize);
        emit(AS_Oper(String(inst), Temp_TempList(resReg, NULL), Temp_TempList(leftReg, NULL), NULL));
        return resReg;
    }
    int op = exp->u.BINOP.op;
    if (op == T_plus || op == T_minus) {
        if (op == T_plus) {
            assemInstr = "addq";
        } else {
            assemInstr = "subq";
        }

        Temp_temp leftReg = munchExp(exp->u.BINOP.left);
        Temp_temp resReg = Temp_newtemp();
        emit(AS_Move("movq `s0, `d0", Temp_TempList(resReg, NULL), Temp_TempList(leftReg, NULL)));

        if (exp->u.BINOP.right->kind == T_CONST) {
            sprintf(inst, "%s $%d, `d0", assemInstr, exp->u.BINOP.right->u.CONST);
            emit(AS_Oper(String(inst), Temp_TempList(resReg, NULL), Temp_TempList(resReg, NULL), NULL));
            return resReg;
        } else {
            Temp_temp rightReg = munchExp(exp->u.BINOP.right, frameSize);

        }
    }
}

static Temp_temp munchConst(T_exp exp) {
    char inst[512];
    Temp_temp reg = Temp_newtemp();
    sprintf(inst, "movq $%d, `d0", exp->u.CONST);
    emit(AS_Oper(String(inst), Temp_TempList(reg, NULL), NULL, NULL));
    return reg;
}



static Temp_temp munchCall(T_exp exp) {
    char inst[512];
    Temp_temp rax = F_RV();
    if (exp->u.CALL.fun->kind != T_NAME) {
        return rax;
    }
    Temp_tempList argList = munchArgs(exp->u.CALL.args);
    Temp_tempList calldefs = F_callersaves();
    Temp_splice(calldefs, Temp_TempList(rax, NULL));

    sprintf(inst, "callq %s", Temp_labelstring(exp->u.CALL.fun->u.NAME));
    emit(AS_Oper(String(inst), calldefs, argList, NULL));
    return rax;
}

static AS_memFetch munchMem(T_exp mem, int ordinal) {
    char inst[512];
    T_exp exp = mem->u.MEM;
    if (exp->kind == T_BINOP && exp->u.BINOP.op == T_plus) {
        if (exp->u.BINOP.right->kind == T_CONST) {
            T_exp offsetExp = exp->u.BINOP.right;
            Temp_temp baseReg = munchExp(exp->u.BINOP.left);
            if (offsetExp->u.CONST == 0) {
                sprintf(inst, "(`s%d)", ordinal);
            } else {
                sprintf(inst, "%d(`s%d)", offsetExp->u.CONST, ordinal);
            }
            return AS_MemFetch(String(inst), Temp_TempList(baseReg, NULL));
        } else if (exp->u.BINOP.left->kind == T_CONST) {
            T_exp offsetExp = exp->u.BINOP.left;
            Temp_temp baseReg = munchExp(exp->u.BINOP.right);
            if (offsetExp->u.CONST == 0) {
                sprintf(inst, "(`s%d)", ordinal);
            } else {
                sprintf(inst, "%d(`s%d)", offsetExp->u.CONST, ordinal);
            }
            return AS_MemFetch(String(inst), Temp_TempList(baseReg, NULL));
        } else {
            Temp_temp memReg = munchExp(exp);
            sprintf(inst, "(`s%d)", ordinal);
            return AS_MemFetch(String(inst), Temp_TempList(memReg, NULL));
        }
    } else {
        Temp_temp memReg = munchExp(exp);
        sprintf(inst, "(`s%d)", ordinal);
        return AS_MemFetch(String(inst), Temp_TempList(memReg, NULL));
    }
}

static void munchMoveStm(T_stm s) {
    char inst[512];
    T_exp dst = s->u.MOVE.dst, src = s->u.MOVE.src;
    if (dst->kind == T_MEM) {
        Temp_temp srcReg = munchExp(src);
        AS_memFetch fetch = munchMem(dst, 1);
        sprintf(inst, "movq `s0, %s", fetch->fetch);
        Temp_tempList srcRegs = Temp_TempList(srcReg, NULL);
        Temp_splice(srcRegs, fetch->regs);
        emit(AS_Oper(String(inst), NULL, srcRegs, NULL));
    } else {
        if (src->kind == T_MEM) {
            AS_memFetch fetch = munchMem(src, 0);
            Temp_temp dstReg = munchExp(dst);
            sprintf(inst, "movq %s, `d0", fetch->fetch);
            emit(AS_Oper(String(inst), Temp_TempList(dstReg, NULL), fetch->regs, NULL));
        } else if (src->kind == T_CONST) {
            Temp_temp dstReg = munchExp(dst);
            sprintf(inst, "movq $%d, `d0", src->u.CONST);
            emit(AS_Oper(String(inst), Temp_TempList(dstReg, NULL), NULL, NULL));
        } else {
            Temp_temp srcReg = munchExp(src);
            Temp_temp dstReg = munchExp(dst);
            emit(AS_Move("movq `s0, `d0", Temp_TempList(dstReg, NULL), Temp_TempList(srcReg, NULL)));
        }
    }
}
//    if (dst->kind == T_MEM) {
//        if (dst->u.MEM->kind == T_BINOP
//            && dst->u.MEM->u.BINOP.op == T_plus
//            && dst->u.MEM->u.BINOP.right->kind == T_CONST) {
//
//            if (src->kind == T_CONST) {
//                /* MOVE(MEM(BINOP(PLUS,e1,CONST(i))),CONST(j)) */
//                T_exp e1 = dst->u.MEM->u.BINOP.left, e2 = src;
//                int i = dst->u.MEM->u.BINOP.right->u.CONST;
//                int j = src->u.CONST;
//                sprintf(inst,
//                        "movq $%d, %d(`s0)\n", j, i);
//
//                emit(AS_Oper(inst, NULL,
//
//                             L(munchExp(e1), NULL), NULL));
//            } else {
//                /* MOVE(MEM(BINOP(PLUS,e1,CONST(i))),e2) */
//                T_exp e1 = dst->u.MEM->u.BINOP.left, e2 = src;
//                int i = dst->u.MEM->u.BINOP.right->u.CONST;
//                sprintf(inst,
//                        "movq `s1, %d(`s0)\n", i);
//
//                emit(AS_Oper(inst, NULL,
//
//                             L(munchExp(e1), L(munchExp(e2), NULL)), NULL));
//            }
//        } else if (dst->u.MEM->kind == T_BINOP
//                   && dst->u.MEM->u.BINOP.op == T_plus
//                   && dst->u.MEM->u.BINOP.left->kind == T_CONST) {
//            if (src->kind == T_CONST) {
//                /* MOVE(MEM(BINOP(PLUS,CONST(i),e1)),CONST(j)) */
//                T_exp e1 = dst->u.MEM->u.BINOP.right, e2 = src;
//                int i = dst->u.MEM->u.BINOP.left->u.CONST;
//                int j = src->u.CONST;
//                sprintf(inst,
//                        "movq $%d, %d(`s0)\n", j, i);
//
//                emit(AS_Oper(inst, NULL,
//
//                             L(munchExp(e1), NULL), NULL));
//            } else {
//                /* MOVE(MEM(BINOP(PLUS,CONST(i),e1)),e2) */
//                // MEM[i + e1] = e2
//                T_exp e1 = dst->u.MEM->u.BINOP.right, e2 = src;
//                int i = dst->u.MEM->u.BINOP.left->u.CONST;
//                sprintf(inst,
//                        "movq `s1, %d(`s0)\n", i);
//
//                emit(AS_Oper(inst, NULL,
//
//                             L(munchExp(e1), L(munchExp(e2), NULL)), NULL));
//            }
//        } else if (src->kind == T_MEM) {
//            /* MOVE(MEM(e1), MEM(e2)) */
//            T_exp e1 = dst->u.MEM, e2 = src->u.MEM;
//            Temp_temp r = Temp_newtemp();
//            sprintf(inst,
//                    "movq (`s0), `d0\n");
//
//            emit(AS_Oper(String(inst), L(r, NULL),
//
//                         L(munchExp(e2), NULL), NULL));
//            sprintf(inst,
//                    "movq `s0, (`s1)\n");
//
//            emit(AS_Oper(String(inst), NULL,
//                         L(r, L(munchExp(e1), NULL)), NULL));
//        } else if (dst->u.MEM->kind == T_CONST) {
//            /* MOVE(MEM(CONST(i)), e2) */
//            T_exp e2 = src;
//            int i = dst->u.MEM->u.CONST;
//            sprintf(inst,
//                    "movq `s0, %d\n", i);
//
//            emit(AS_Oper(inst, NULL,
//
//                         L(munchExp(e2), NULL), NULL));
//        } else {
//            /* MOVE(MEM(e1), e2) */
//            T_exp e1 = dst->u.MEM, e2 = src;
//            sprintf(inst,
//                    "movq `s1, (`s0)\n");
//
//            emit(AS_Oper(inst, NULL,
//
//                         L(munchExp(e1), L(munchExp(e2), NULL)), NULL));
//        }
//    } else if (dst->kind == T_TEMP) {
//        if (src->kind == T_CALL) {
//            if (src->u.CALL.fun->kind == T_NAME) {
//                /* MOVE(TEMP(t),CALL(NAME(lab),args)) */
//                munchCallerSave();
//
//                Temp_label lab = src->u.CALL.fun->u.NAME;
//                bool isLibFunc = isExternalCall(Temp_labelstring(lab));
//                T_expList args = src->u.CALL.args;
//                Temp_temp t = dst->u.TEMP;
//                if (!isLibFunc) {
//                    Temp_temp r = munchExp(args->head);
//
//                    emit(AS_Oper(
//
//                            "push `s0\n",
//
//                            L(F_SP(), NULL),
//                            L(r, NULL), NULL));
//                    args = args->tail;
//                }
//                Temp_tempList l = munchArgs(F_argregisters(), T_reverseList(args), 0, isLibFunc);
//                Temp_tempList calldefs = F_callersaves();
//                sprintf(inst,
//                        "call %s\n",
//                        Temp_labelstring(lab)
//                );
//
//                emit(AS_Oper(inst, L(F_RV(), calldefs), l, NULL));
//                munchCallerRestore(l, isLibFunc
//                );
//                sprintf(inst2,
//                        "movq `s0, `d0\n");
//
//                emit(AS_Move(inst2, L(t, NULL),
//
//                             L(F_RV(), NULL)));
//            } else {
//                /* MOVE(TEMP(t),CALL(e,args)) */
//                assert(0);
//            }
//        } else {
//            /* MOVE(TEMP(i),e2) */
//            T_exp e2 = src;
//            Temp_temp i = dst->u.TEMP;
//            sprintf(inst,
//                    "movq `s0, `d0\n");
//
//            emit(AS_Move(inst, L(i, NULL),
//
//                         L(munchExp(e2), NULL)));
//        } else {
//            assert(0);
//        }
//    }

static void munchStm(T_stm s) {
    switch (s->kind) {
        case T_MOVE: {
            return munchMoveStm(s);
        }
        case T_LABEL: {
            /* LABEL(lab) */

            // Avoid two labels in same palce
            if (lastIsLabel) {
                emit(AS_Oper("nop\n", NULL, NULL, NULL));
            }

            Temp_label lab = s->u.LABEL;
            sprintf(inst, "%s:\n", Temp_labelstring(lab));
            emit(AS_Label(inst, lab));
            break;
        }
        case T_EXP: {
            if (s->u.EXP->kind == T_CALL) {
                T_exp call = s->u.EXP;
                if (call->u.CALL.fun->kind == T_NAME) {
                    /* EXP(CALL(NAME(lab),args)) */
                    munchCallerSave();
                    Temp_label lab = call->u.CALL.fun->u.NAME;
                    bool isLibFunc = isExternalCall(Temp_labelstring(lab));
                    T_expList args = call->u.CALL.args;
                    if (!isLibFunc) {
                        Temp_temp r = munchExp(args->head);
                        emit(AS_Oper("push `s0\n", L(F_SP(), NULL), L(r, NULL), NULL));
                        args = args->tail;
                    }
                    Temp_tempList l = munchArgs(args);
                    Temp_tempList calldefs = F_callersaves();
                    sprintf(inst, "call %s\n", Temp_labelstring(lab));
                    emit(AS_Oper(inst, calldefs, l, NULL));
                    munchCallerRestore(l, isLibFunc);
                } else {
                    assert(0);
                }
            } else {
                /* EXP(e) */
                munchExp(s->u.EXP);
            }
            break;
        }
        case T_JUMP: {
            if (s->u.JUMP.exp->kind == T_NAME) {
                /* JUMP(NAME(lab)) */
                Temp_label lab = s->u.JUMP.exp->u.NAME;
                Temp_labelList jumps = s->u.JUMP.jumps;
                sprintf(inst, "jmp `j0\n");
                emit(AS_Oper(inst, NULL, NULL, AS_Targets(jumps)));
            } else {
                /* JUMP(e) */
                T_exp e = s->u.JUMP.exp;
                Temp_labelList jumps = s->u.JUMP.jumps;
                sprintf(inst, "jmp *`s0\n");
                emit(AS_Oper(inst, NULL, L(munchExp(e), NULL), AS_Targets(jumps)));
            }
            break;
        }
        case T_CJUMP: {
            /* CJUMP(op,e1,e2,jt,jf) */
            T_relOp op = s->u.CJUMP.op;
            T_exp e1 = s->u.CJUMP.left;
            T_exp e2 = s->u.CJUMP.right;
            Temp_temp r1 = munchExp(e1);
            Temp_temp r2 = munchExp(e2);
            Temp_temp r3 = Temp_newtemp();
            Temp_temp r4 = Temp_newtemp();
            Temp_label jt = s->u.CJUMP.true;
            Temp_label jf = s->u.CJUMP.false;
            emit(AS_Move("movq `s0, `d0\n", L(r3, NULL), L(r1, NULL)));
            emit(AS_Move("movq `s0, `d0\n", L(r4, NULL), L(r2, NULL)));
            sprintf(inst, "cmpq `s1, `s0\n");
            emit(AS_Oper(inst, NULL, L(r3, L(r4, NULL)), NULL));

            char *opcode = "";
            switch (op) {
                case T_eq:
                    opcode = "je";
                    break;
                case T_ne:
                    opcode = "jne";
                    break;
                case T_lt:
                    opcode = "jl";
                    break;
                case T_gt:
                    opcode = "jg";
                    break;
                case T_le:
                    opcode = "jle";
                    break;
                case T_ge:
                    opcode = "jge";
                    break;
                case T_ult:
                    opcode = "jb";
                    break;
                case T_ule:
                    opcode = "jbe";
                    break;
                case T_ugt:
                    opcode = "ja";
                    break;
                case T_uge:
                    opcode = "jae";
                    break;
            }
            sprintf(inst2, "%s `j0\n", opcode);
            emit(AS_Oper(inst2, NULL, NULL, AS_Targets(Temp_LabelList(jt, NULL))));

            sprintf(inst3, "jmp `j0\n");
            emit(AS_Oper(inst3, NULL, NULL, AS_Targets(Temp_LabelList(jf, NULL))));
            break;
        }
        default: {
            assert(0);
        }
    }
}

static void munchCallerSave() {
    Temp_tempList callerSaves = F_callersaves();
    for (; callerSaves; callerSaves = callerSaves->tail) {
        emit(AS_Oper("push `s0\n", L(F_SP(), NULL), L(callerSaves->head, NULL), NULL));
    }
}

static void munchCallerRestore(Temp_tempList tl, bool isLibFunc) {
    int restoreCount = 0;
    char inst[128];
    for (; tl; tl = tl->tail) {
        ++restoreCount;
    }

    if (!isLibFunc) {
        sprintf(inst, "addq $%d, `s0\n", 8);
        emit(AS_Oper(String(inst), L(F_SP(), NULL), L(F_SP(), NULL), NULL));
    }

    Temp_tempList callerSaves = Temp_reverseList(F_callersaves());
    for (; callerSaves; callerSaves = callerSaves->tail) {
        emit(AS_Oper("pop `d0\n", L(callerSaves->head, NULL), L(F_SP(), NULL), NULL));
    }
}


static Temp_tempList munchArgs(T_expList args) {
    char instr[512];
    Temp_tempList argList = NULL;
    int argRegCount = Temp_listSize(F_argregisters());
    int i = 0;

    for (; args; args = args->tail) {
        if (i < argRegCount) {
            Temp_temp dstReg = Temp_nth(F_argregisters(), i);
            if (args->head->kind == T_CONST) {
                T_exp constExp = args->head;
                sprintf(instr, "movq $%d, `d0", constExp->u.CONST);
                emit(AS_Oper(String(instr), Temp_TempList(dstReg, NULL), NULL, NULL));
            } else {
                Temp_temp srcReg = munchExp(args->head);
                emit(AS_Move("movq `s0, `d0", Temp_TempList(dstReg, NULL), Temp_TempList(srcReg, NULL)));
            }
            argList = Temp_TempList(dstReg, argList);
        } else {
            if (args->head->kind == T_CONST) {
                T_exp constExp = args->head;
                sprintf(instr, "movq $%d, ", constExp->u.CONST);
                if (i != argRegCount) {
                    strcat(instr, Sprintf("%d", (i - argRegCount) * F_wordSize));
                }
                strcat(instr, Sprintf("(%s)", Temp_look(Temp_empty(), F_SP())));
                emit(AS_Oper(instr, NULL, Temp_TempList(F_RA(), NULL), NULL));
            } else {
                Temp_temp srcReg = munchExp(args->head);
                sprintf(instr, "movq `s0, ");
                if (i != argRegCount)
                    strcat(instr, Sprintf("%d", (i - argRegCount) * F_wordSize));
                strcat(instr, Sprintf("(%s)", Temp_look(Temp_empty(), F_SP())));
                emit(AS_Oper(String(instr), NULL, Temp_TempList(srcReg, Temp_TempList(F_SP(), NULL)), NULL));
            }
        }
        ++i;
    }

    if (i > argRegCount)
        argList = Temp_TempList(F_SP(), argList);

    return argList;
//    if (args == NULL) {
//        return NULL;
//    }
//
//    Temp_tempList old = munchArgs(argregs->tail, args->tail, idx + 1, isLibFunc);
//
//    Temp_temp r = munchExp(args->head);
//    // TODO(threadedstream): avoid doing this in case if it's a lib function
//    emit(AS_Move("movq `s0, `d0\n", L(argregs->head, NULL), L(r, NULL)));
//
//    // No need to reserve values before calling in x86
//    return Temp_TempList(r, old);
}

void destroy() {
    AS_instr currInstr = iList->head;

    while (iList) {
        free(currInstr);
        free(iList);
        iList = iList->tail;
    }
}