#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../util.h"
#include "../symbol.h"
#include "../table.h"
#include "../tree.h"
#include "../errormsg.h"

#include "amd64frame.h"

// Machine-related features



static Temp_tempList argumentRegisters = NULL;
static Temp_temp rax = NULL;
static Temp_temp rcx = NULL;
static Temp_temp rdx = NULL;
static Temp_temp rbx = NULL;
static Temp_temp rsi = NULL;
static Temp_temp rdi = NULL;
static Temp_temp r8 = NULL;
static Temp_temp r9 = NULL;
static Temp_temp r10 = NULL;
static Temp_temp r11 = NULL;
static Temp_temp r12 = NULL;
static Temp_temp r13 = NULL;
static Temp_temp r14 = NULL;
static Temp_temp r15 = NULL;

static Temp_temp fp = NULL;
static Temp_temp sp = NULL;
static Temp_temp zero = NULL;
static Temp_temp ra = NULL;
static Temp_temp rv = NULL;

static Temp_tempList registers = NULL;
static Temp_tempList specialregs = NULL;


static F_frameList F_FrameList(F_frame head, F_frameList tail) {
    F_frameList l = checked_malloc(sizeof(*l));
    l->head = head;
    l->tail = tail;
    return l;
}

static F_access InFrame(int offset) {
    F_access a = checked_malloc(sizeof(*a));
    a->kind = inFrame;
    a->u.offset = offset;
    return a;
}

static F_access InReg(Temp_temp reg) {
    F_access a = checked_malloc(sizeof(*a));
    a->kind = inReg;
    a->u.reg = reg;
    return a;
}

static F_frameList frameStack = NULL;

F_accessList F_AccessList_amd64(F_access head, F_accessList tail) {
    F_accessList l = checked_malloc(sizeof(*l));
    l->head = head;
    l->tail = tail;
    return l;
}

F_frame F_newFrame_amd64(Temp_label name, U_boolList formals) {
    F_frame f = checked_malloc(sizeof(*f));
    int frameOffset = F_wordSize;
    f->name = name;
    f->frameSize = Temp_namedlabel(Strcat(Temp_labelstring(name), "_framesize"));

    T_exp fpExp = T_Temp(Temp_newtemp());
    f->viewShift = T_Move(fpExp, F_staticLinkExp_amd64(fpExp));

    T_exp dstExp;
    T_exp fpExpCopy;
    T_stm singleViewShift;
    int argRegCount = Temp_listSize(F_argregisters());
    int formalsSize = U_listSize(formals);

    if (formalsSize > argRegCount) {
        fpExpCopy = T_Temp(Temp_newtemp());
        f->viewShift = T_Seq(f->viewShift, T_Move(fpExpCopy, F_staticLinkExp_amd64(fpExp)));
    }

    U_boolList formalEscape = formals;
    F_accessList formal = NULL;
    for (int i = 0; i < formalsSize; i++) {
        if (formalEscape) {
            formal = F_AccessList(InFrame(frameOffset), formal);
            dstExp = T_Mem(T_Binop(T_minus, fpExp, T_Const((i + 1) * F_wordSize)));
            frameOffset += F_wordSize;
            f->localCount++;
        } else {
            Temp_temp reg = Temp_newtemp();
            formal = F_AccessList(InReg(reg), NULL);
            dstExp = T_Temp(reg);
        }

        if (i < argRegCount) {
            singleViewShift = T_Move(dstExp, T_Temp(Temp_nth(F_argregisters(), i)));
        } else {
            singleViewShift = T_Move(dstExp, T_Mem(
                    T_Binop(T_plus, fpExpCopy,
                            T_Const((i - argRegCount + 1) * F_wordSize))
            ));
        }
        f->viewShift = T_Seq(f->viewShift, singleViewShift);
    }

    Temp_tempList calleeSaves = F_calleesaves();
    Temp_temp storeReg;
    T_stm singleSave;
    T_stm singleRestore;

    storeReg = Temp_newtemp();
    f->saveCalleeSaves = T_Move(T_Temp(storeReg),
                                T_Temp(calleeSaves->head));
    f->restoreCalleeSaves = T_Move(T_Temp(calleeSaves->head),
                                   T_Temp(storeReg));

    calleeSaves = calleeSaves->tail;

    for (; calleeSaves; calleeSaves = calleeSaves->tail) {
        storeReg = Temp_newtemp();
        singleSave = T_Move(T_Temp(storeReg), T_Temp(calleeSaves->head));
        singleRestore = T_Move(T_Temp(calleeSaves->head), T_Temp(storeReg));
        f->saveCalleeSaves = T_Seq(singleSave, f->saveCalleeSaves);
        f->restoreCalleeSaves = T_Seq(singleRestore, f->restoreCalleeSaves);
    }

    f->formals = formal;
    f->locals = NULL;
    f->temp = Temp_empty();
    return f;

//    // first six arguments are passed in registers
//    // the rest resides on the stack
//    // TODO(threadedstream): handle on-stack residing arguments later
//    int offset = 8;
//    U_boolList formalEscape = formals;
//    F_accessList formal = NULL;
//    while (formalEscape) {
//        offset += 8;
//        formal = F_AccessList(InReg(argumentRegisters->head), formal);
//        formalEscape = formalEscape->tail;
//        argument_registers = argument_registers->tail;
//    }
//
//    f->formals = formal;
//    f->locals = NULL;
//    f->temp = Temp_empty();
//
//    frameStack = F_FrameList(f, frameStack);
//    return f;
}

string F_string_amd64(Temp_label lab, string str) {
    string buf = (string) checked_malloc(sizeof(char) * (strlen(str) + 100));
    sprintf(buf, "%s: .ascii \"%s\"\n", Temp_labelstring(lab), str);
    return buf;
}


static Temp_tempList L(Temp_temp h, Temp_tempList t) {
    return Temp_TempList(h, t);
}

F_frag F_newProcFrag_amd64(T_stm body, F_frame frame) {
    return F_ProcFrag(body, frame);
}

Temp_label F_name_amd64(F_frame f) {
    return f->name;
}

F_fragList F_FragList_amd64(F_frag head, F_fragList tail) {
    F_fragList l = checked_malloc(sizeof(*l));
    l->head = head;
    l->tail = tail;
    return l;
}

static AS_instrList appendCalleeSave(AS_instrList il) {
    Temp_tempList calleesaves = Temp_reverseList(F_calleesaves());
    AS_instrList ail = il;
    for (; calleesaves; calleesaves = calleesaves->tail) {
        ail = AS_InstrList(
                AS_Oper("push `s0\n", L(F_SP(), NULL), L(calleesaves->head, NULL), NULL), ail);

    }
    return ail;
}

static AS_instrList restoreCalleeSave(AS_instrList il) {
    Temp_tempList calleesaves = F_calleesaves();
    AS_instrList ail = NULL;
    for (; calleesaves; calleesaves = calleesaves->tail) {
        ail = AS_InstrList(
                AS_Oper("pop `s0\n", L(F_SP(), NULL), L(calleesaves->head, NULL), NULL), ail);
    }
    return AS_splice(ail, il);
}

T_stm F_procEntryExit1_amd64(F_frame frame, T_stm stm) {
//    frameStack = frameStack->tail;
//    return stm;

    stm = T_Seq(frame->saveCalleeSaves, stm);
    stm = T_Seq(frame->viewShift, stm);
    stm = T_Seq(stm, frame->restoreCalleeSaves);

    return stm;
}

static Temp_tempList returnSink = NULL;

AS_instrList F_procEntryExit2_amd64(AS_instrList body) {
    Temp_tempList calleeSaves = F_calleesaves();
    if (!returnSink)
        returnSink = Temp_TempList(F_RA(),
                                   Temp_TempList(F_SP(), calleeSaves));
//
//    char inst_add[128];
//    // may also make frame size variable
//    int frame_size = 100;
//    sprintf(inst_add, "addq $%d, `s0\n", frame_size);
//    AS_instr leaveInstr = AS_Oper("leave\n", L(F_SP(), L(F_FP(), NULL)), L(F_SP(), NULL), NULL);
//    AS_instr retInstr = AS_Oper("ret\n", NULL, returnSink, NULL);
//    AS_instr addInstr = AS_Oper(String(inst_add), L(F_SP(), NULL), L(F_SP(), NULL), NULL);
//    AS_instrList restoredCalleeSaves = restoreCalleeSave(AS_InstrList(leaveInstr, AS_InstrList(retInstr, NULL)));
//    return AS_splice(body, AS_InstrList(addInstr, restoredCalleeSaves));
    AS_instr retSink = AS_Oper("", NULL, returnSink, NULL);
    body = AS_InstrList(retSink, body);
    return body;
}

AS_proc F_procEntryExit3_amd64(F_frame frame, AS_instrList body) {
//    char buf[1024], inst_lbl[128], inst_sub[128];
//    int frame_size = 100;
//    sprintf(buf, "# PROCEDURE %s\n", S_name(frame->name));
//    sprintf(inst_lbl, "%s:\n", S_name(frame->name));
//    sprintf(inst_sub, "subq $%d, `s0\n", frame_size);
//    AS_instr subInstr = AS_Oper(String(inst_sub), L(F_SP(), NULL), L(F_SP(), NULL), NULL);
//    AS_instr movInstr = AS_Move("movq `s0, `d0\n", L(F_FP(), NULL), L(F_SP(), NULL));
//    AS_instr pushInstr = AS_Oper("push `s0\n", L(F_FP(), L(F_SP(), NULL)), L(F_FP(), NULL), NULL);
//    AS_instrList appendedCalleeSave = appendCalleeSave(AS_InstrList(subInstr, body));
//    body = AS_InstrList(AS_Label(String(inst_lbl), frame->name),
//                        AS_InstrList(pushInstr, AS_InstrList(movInstr, appendedCalleeSave)));
//    return AS_Proc(String(buf), body, "# END\n");
    char prologue[512], epilogue[512];
    int fs = (frame->localCount + frame->maxOutgoingArgs) * F_wordSize;
    sprintf(prologue, ".set %s, %d", Temp_labelstring(frame->frameSize), fs);
    sprintf(prologue, "%s:\n", Temp_labelstring(frame->name));
    if (fs != 0) {
        sprintf(prologue, "subq $%d, %s\n", fs, Temp_look(Temp_empty(), F_SP()));
        sprintf(epilogue, "addq $%d, %s\n", fs, Temp_look(Temp_empty(), F_SP()));
    }

    sprintf(epilogue, "retq\n");
    return AS_Proc(String(prologue), body, String(epilogue));
}

F_accessList F_formals_amd64(F_frame f) {
    return f->formals;
}

F_access F_allocLocal_amd64(F_frame f, bool escape) {
    if (!escape) {
        F_access l = InReg(Temp_newtemp());
        f->locals = F_AccessList(l, f->locals);
        return l;
    }

    // Locals start from %ebp - 8 - 40 (callee save registers)
    int offset = -8 - 40;
    F_accessList locals = f->locals;
    while (locals) {
        if (locals->head->kind == inFrame)
            offset -= 8;
        locals = locals->tail;
    }

    F_access l = InFrame(offset);
    f->locals = F_AccessList(l, f->locals);
    return l;
}


Temp_temp F_FP_amd64(void) {
    if (fp == NULL) {
        F_initRegisters();
    }
    return fp;
}

Temp_temp F_SP_amd64(void) {
    if (sp == NULL) {
        F_initRegisters();
    }
    return sp;
}

// Not available in amd64 (yet present in ARM)
Temp_temp F_ZERO_amd64(void) {
    if (zero == NULL) {
        F_initRegisters();
    }
    return zero;
}

// Not available in amd64
Temp_temp F_RA_amd64(void) {
    if (ra == NULL) {
        F_initRegisters();
    }
    return ra;
}

Temp_temp F_RV_amd64(void) {
    if (rv == NULL) {
        F_initRegisters();
    }
    return rv;
}

Temp_temp F_AX_amd64(void) {
    if (rax == NULL) {
        F_initRegisters();
    }
    return rax;
}

Temp_temp F_DI_amd64(void) {
    if (rdi == NULL) {
        F_initRegisters();
    }
    return rdi;
}

Temp_temp F_DX_amd64(void) {
    if (rdx == NULL) {
        F_initRegisters();
    }
    return rdx;
}

void F_initRegisters_amd64(void) {
    fp = Temp_newtemp();
    sp = Temp_newtemp();
    zero = Temp_newtemp();
    ra = Temp_newtemp();
    rv = Temp_newtemp();
    rax = Temp_newtemp();
    rcx = Temp_newtemp();
    rdx = Temp_newtemp();
    rbx = Temp_newtemp();
    rsi = Temp_newtemp();
    rdi = Temp_newtemp();
    r8 = Temp_newtemp();
    r9 = Temp_newtemp();
    r10 = Temp_newtemp();
    r11 = Temp_newtemp();
    r12 = Temp_newtemp();
    r13 = Temp_newtemp();
    r14 = Temp_newtemp();
    r15 = Temp_newtemp();

    Temp_enter(Temp_name(), rax, "%rax");
    Temp_enter(Temp_name(), rcx, "%rcx");
    Temp_enter(Temp_name(), rdx, "%rdx");
    Temp_enter(Temp_name(), rbx, "%rbx");
    Temp_enter(Temp_name(), rsi, "%rsi");
    Temp_enter(Temp_name(), rdi, "%rdi");
    Temp_enter(Temp_name(), r8, "%r8");
    Temp_enter(Temp_name(), r9, "%r9");
    Temp_enter(Temp_name(), r10, "%r10");
    Temp_enter(Temp_name(), r11, "%r11");
    Temp_enter(Temp_name(), r12, "%r12");
    Temp_enter(Temp_name(), r13, "%r13");
    Temp_enter(Temp_name(), r14, "%r14");
    Temp_enter(Temp_name(), r15, "%r15");

    specialregs = Temp_TempList(rv,
                                Temp_TempList(fp,
                                              Temp_TempList(ra, NULL)));
}

Temp_map F_initialRegisters_amd64(F_frame f) {
    Temp_map m = Temp_empty();
    Temp_enter(m, fp, "%rbp");
    Temp_enter(m, sp, "%rsp");
    Temp_enter(m, rv, "%rax");

    Temp_enter(m, rax, "%rax");
    Temp_enter(m, rcx, "%rcx");
    Temp_enter(m, rdx, "%rdx");
    Temp_enter(m, rbx, "%rbx");
    Temp_enter(m, rsi, "%rsi");
    Temp_enter(m, rdi, "%rdi");
    Temp_enter(m, r8, "%r8");
    Temp_enter(m, r9, "%r9");
    Temp_enter(m, r10, "%r10");
    Temp_enter(m, r11, "%r11");
    Temp_enter(m, r12, "%r12");
    Temp_enter(m, r13, "%r13");
    Temp_enter(m, r14, "%r14");
    Temp_enter(m, r15, "%r15");

    return m;
}

Temp_tempList F_argregisters_amd64(void) {
    if (!fp) {
        F_initRegisters();
        if (argumentRegisters) {
            return argumentRegisters;
        }
    }

    argumentRegisters = Temp_TempList(rdi, NULL);
    Temp_tempList t_rsi = argumentRegisters->tail = Temp_TempList(rsi, NULL);
    Temp_tempList t_rdx = t_rsi->tail = Temp_TempList(rdx, NULL);
    Temp_tempList t_r10 = t_rdx->tail = Temp_TempList(r10, NULL);
    Temp_tempList t_r8 = t_r10->tail = Temp_TempList(r8, NULL);
    Temp_tempList t_r9 = t_r8->tail = Temp_TempList(r9, NULL);

    return argumentRegisters;
}

Temp_tempList F_registers_amd64(void) {
    if (!fp) {
        F_initRegisters();
    }
    Temp_tempList t_rcx = Temp_TempList(rcx, NULL);
    Temp_tempList t_rdx = t_rcx->tail = Temp_TempList(rdx, NULL);
    Temp_tempList t_rbx = t_rdx->tail = Temp_TempList(rbx, NULL);
    Temp_tempList t_rsi = t_rbx->tail = Temp_TempList(rsi, NULL);
    Temp_tempList t_rdi = t_rsi->tail = Temp_TempList(rdi, NULL);
    Temp_tempList t_r8 = t_rdi->tail = Temp_TempList(r8, NULL);
    Temp_tempList t_r9 = t_r8->tail = Temp_TempList(r9, NULL);
    Temp_tempList t_r10 = t_r9->tail = Temp_TempList(r10, NULL);
    Temp_tempList t_r11 = t_r10->tail = Temp_TempList(r11, NULL);
    Temp_tempList t_r12 = t_r11->tail = Temp_TempList(r12, NULL);
    Temp_tempList t_r13 = t_r12->tail = Temp_TempList(r13, NULL);
    Temp_tempList t_r14 = t_r13->tail = Temp_TempList(r14, NULL);
    Temp_tempList t_r15 = t_r14->tail = Temp_TempList(r15, NULL);

    return t_rcx;
}

Temp_tempList F_callersaves_amd64(void) {
    if (!fp) {
        F_initRegisters();
    }
    Temp_tempList t_rdx = Temp_TempList(rdx, NULL);
    Temp_tempList t_rcx = t_rdx->tail = Temp_TempList(rcx, NULL);
    Temp_tempList t_r8 = t_rcx->tail = Temp_TempList(r8, NULL);
    Temp_tempList t_r9 = t_r8->tail = Temp_TempList(r9, NULL);
    Temp_tempList t_r10 = t_r9->tail = Temp_TempList(r10, NULL);
    Temp_tempList t_r11 = t_r10->tail = Temp_TempList(r11, NULL);

    return t_rdx;
}

Temp_tempList F_calleesaves_amd64(void) {
    if (!fp) {
        F_initRegisters();
    }
    Temp_tempList t_rbx = Temp_TempList(rbx, NULL);
    Temp_tempList t_r12 = t_rbx->tail = Temp_TempList(r12, NULL);
    Temp_tempList t_r13 = t_r12->tail = Temp_TempList(r13, NULL);
    Temp_tempList t_r14 = t_r13->tail = Temp_TempList(r14, NULL);
    Temp_tempList t_r15 = t_r14->tail = Temp_TempList(r15, NULL);

    return t_rbx;
}

string F_getlabel_amd64(F_frame frame) {
    return Temp_labelstring(frame->name);
}

int F_accessOffset_amd64(F_access a) {
    if (a->kind != inFrame) {
        EM_error(0, "Offset of a reg access is invalid");
    }
    return a->u.offset;
}

F_frag F_StringFrag_amd64(Temp_label label, string str) {
    F_frag f = checked_malloc(sizeof(*f));
    f->kind = F_stringFrag;
    f->u.stringg.label = label;
    f->u.stringg.str = String(str);
    return f;
}

Temp_temp F_accessReg_amd64(F_access a) {
    if (a->kind != inReg) {
        EM_error(0, "Reg of a frame access is invalid");
    }

    return a->u.reg;
}

F_frag F_ProcFrag_amd64(T_stm body, F_frame frame) {
    F_frag f = checked_malloc(sizeof(*f));
    f->kind = F_procFrag;
    f->u.proc.body = body;
    f->u.proc.frame = frame;
    return f;
}

T_exp F_Exp_amd64(F_access acc, T_exp framePtr) {
    if (acc->kind == inReg) {
        return T_Temp(F_accessReg(acc));
    }
    return T_Mem(T_Binop(T_plus, framePtr, T_Const(F_accessOffset(acc))));
}

T_exp F_ExpWithStaticLink_amd64(F_access acc, T_exp staticLink) {
    if (acc->kind == inReg) {
        return T_Temp(F_accessReg(acc));
    }
    return T_Mem(T_Binop(T_plus, staticLink, T_Const(F_accessOffset(acc) - 16)));
}

T_exp F_FPExp_amd64(T_exp framePtr) {
    return T_Mem(framePtr);
}

T_exp F_staticLinkExp_amd64(T_exp framePtr) {
    // static link at fp + 16
    return T_Binop(T_plus, framePtr, T_Const(2 * F_wordSize));
}

T_exp F_upperStaticLinkExp_amd64(T_exp staticLink) {
    return T_Mem(staticLink);
}

T_exp F_staticLink2FP_amd64(T_exp staticLink) {
    return T_Binop(T_minus, T_Mem(staticLink), T_Const(2 * F_wordSize));
}

T_exp F_externalCall_amd64(string s, T_expList args) {
    return T_Call(T_Name(Temp_namedlabel(s)), args);
}