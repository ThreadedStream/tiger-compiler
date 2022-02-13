#include "frame.h"
#include "base.h"
#include "util.h"
#include "amd64/amd64frame.h"
#include "x86/x86frame.h"


Temp_map F_tempMap = NULL;
int F_wordSize = 8; // amd64


F_fragList F_FragList(F_frag head, F_fragList tail) {
    switch (targetArch) {
        case x86:
            return F_FragList_x86(head, tail);
        case AMD64:
            return F_FragList_amd64(head, tail);
        default:
            return NULL;
    }
}


Temp_tempList F_registers(void) {
    switch (targetArch) {
        case x86:
            return F_registers_x86();
        case AMD64:
            return F_registers_amd64();
        default:
            return NULL;
    }
}

string F_getlabel(F_frame frame) {
    switch (targetArch) {
        case x86:
            return F_getlabel_x86(frame);
        case AMD64:
            return F_getlabel_amd64(frame);
        default:
            return NULL;
    }
}

T_exp F_Exp(F_access acc, T_exp framePtr) {
    switch (targetArch) {
        case x86:
            return F_Exp_x86(acc, framePtr);
        case AMD64:
            return F_Exp_amd64(acc, framePtr);
        default:
            return NULL;
    }
}

T_exp F_ExpWithStaticLink(F_access acc, T_exp staticLink) {
    switch (targetArch) {
        case x86:
            return F_ExpWithStaticLink_x86(acc, staticLink);
        case AMD64:
            return F_ExpWithStaticLink_amd64(acc, staticLink);
        default:
            return NULL;
    }
}

T_exp F_FPExp(T_exp framePtr) {
    switch (targetArch) {
        case x86:
            return F_FPExp_x86(framePtr);
        case AMD64:
            return F_FPExp_amd64(framePtr);
        default:
            return NULL;
    }
}

T_exp F_upperStaticLinkExp(T_exp staticLink) {
    switch (targetArch) {
        case x86:
            return F_upperStaticLinkExp_x86(staticLink);
        case AMD64:
            return F_upperStaticLinkExp_amd64(staticLink);
        default:
            return NULL;
    }
}

T_exp F_staticLinkExp(T_exp framePtr) {
    switch (targetArch) {
        case x86:
            return F_staticLinkExp_x86(framePtr);
        case AMD64:
            return F_staticLinkExp_amd64(framePtr);
        default:
            return NULL;
    }
}

T_exp F_staticLink2FP(T_exp staticLink) {
    switch (targetArch) {
        case x86:
            return F_staticLink2FP_x86(staticLink);
        case AMD64:
            return F_staticLink2FP_amd64(staticLink);
        default:
            return NULL;
    }
}

F_access F_allocLocal(F_frame f, bool escape) {
    switch (targetArch) {
        case x86:
            return F_allocLocal_x86(f, escape);
        case AMD64:
            return F_allocLocal_amd64(f, escape);
        default:
            return NULL;
    }
}

F_accessList F_formals(F_frame f) {
    switch (targetArch) {
        case x86:
            return F_formals_x86(f);
        case AMD64:
            return F_formals_amd64(f);
        default:
            return NULL;
    }
}

Temp_label F_name(F_frame f) {
    switch (targetArch) {
        case x86:
            return F_name_x86(f);
        case AMD64:
            return F_name_amd64(f);
        default:
            return NULL;
    }
}


int F_accessOffset(F_access a) {
    switch (targetArch) {
        case x86:
            return F_accessOffset_x86(a);
        case AMD64:
            return F_accessOffset_amd64(a);
        default:
            return NULL;
    }
}

Temp_temp F_accessReg(F_access a) {
    switch (targetArch) {
        case x86:
            return F_accessReg_x86(a);
        case AMD64:
            return F_accessReg_amd64(a);
        default:
            return NULL;
    }
}

F_accessList F_AccessList(F_access head, F_accessList tail) {
    switch (targetArch) {
        case x86:
            return F_AccessList_x86(head, tail);
        case AMD64:
            return F_AccessList_amd64(head, tail);
        default:
            return NULL;
    }
}

Temp_temp F_FP(void) {
    switch (targetArch) {
        case x86:
            return F_FP_x86();
        case AMD64:
            return F_FP_amd64();
        default:
            return NULL;
    }
}

Temp_temp F_SP(void) {
    switch (targetArch) {
        case x86:
            return F_SP_x86();
        case AMD64:
            return F_SP_amd64();
        default:
            return NULL;
    }
}

Temp_temp F_ZERO(void) {
    switch (targetArch) {
        case x86:
            return F_ZERO_x86();
        case AMD64:
            return F_ZERO_amd64();
        default:
            return NULL;
    }
}

Temp_temp F_RA(void) {
    switch (targetArch) {
        case x86:
            return F_RA_x86();
        case AMD64:
            return F_RA_amd64();
        default:
            return NULL;
    }
}

Temp_temp F_RV(void) {
    switch (targetArch) {
        case x86:
            return F_RV_x86();
        case AMD64:
            return F_RV_amd64();
        default:
            return NULL;
    }
}

Temp_temp F_AX(void) {
    switch (targetArch) {
        case x86:
            return F_AX_x86();
        case AMD64:
            return F_AX_amd64();
        default:
            return NULL;
    }
}

Temp_temp F_DX(void) {
    switch (targetArch) {
        case x86:
            return F_DX_x86();
        case AMD64:
            return F_DX_amd64();
        default:
            return NULL;
    }
}

F_frag F_StringFrag(Temp_label label, string str) {
    switch (targetArch) {
        case x86:
            return F_StringFrag_x86(label, str);
        case AMD64:
            return F_StringFrag_amd64(label, str);
        default:
            return NULL;
    }
}

F_frag F_ProcFrag(T_stm body, F_frame frame) {
    switch (targetArch) {
        case x86:
            return F_ProcFrag_x86(body, frame);
        case AMD64:
            return F_ProcFrag_amd64(body, frame);
        default:
            return NULL;
    }
}

void F_initRegisters(void) {
    switch (targetArch) {
        case x86:
            return F_initRegisters_x86();
        case AMD64:
            return F_initRegisters_amd64();
        default:
            return;
    }
}

Temp_map F_initialRegisters(F_frame f) {
    switch (targetArch) {
        case x86:
            return F_initialRegisters_x86(f);
        case AMD64:
            return F_initialRegisters_amd64(f);
        default:
            return NULL;
    }
}

Temp_tempList F_callersaves(void) {
    switch (targetArch) {
        case x86:
            return F_callersaves_x86();
        case AMD64:
            return F_callersaves_amd64();
        default:
            return NULL;
    }
}

Temp_tempList F_calleesaves(void) {
    switch (targetArch) {
        case x86:
            return F_calleesaves_x86();
        case AMD64:
            return F_calleesaves_amd64();
        default:
            return NULL;
    }
}

Temp_tempList F_argregisters(void) {
    switch (targetArch) {
        case AMD64:
            return F_argregisters_amd64();
        default:
            return NULL;
    }
}

F_frame F_newFrame(Temp_label name, U_boolList formals) {
    switch (targetArch) {
        case x86:
            return F_newFrame_x86(name, formals);
        case AMD64:
            return F_newFrame_amd64(name, formals);
        default:
            return NULL;
    }
}

T_exp F_externalCall(string s, T_expList args) {
    switch (targetArch) {
        case x86:
            return F_externalCall_x86(s, args);
        case AMD64:
            return F_externalCall_amd64(s, args);
        default:
            return NULL;
    }
}

string F_string(Temp_label lab, string str) {
    switch (targetArch) {
        case x86:
            return F_string_x86(lab, str);
        case AMD64:
            return F_string_x86(lab, str);

        default:
            return NULL;
    }
}

F_frag F_newProcFrag(T_stm body, F_frame frame) {
    switch (targetArch) {
        case x86:
            return F_newProcFrag_x86(body, frame);
        case AMD64:
            return F_newProcFrag_amd64(body, frame);
        default:
            return NULL;
    }
}

T_stm F_procEntryExit1(F_frame frame, T_stm stm) {
    switch (targetArch) {
        case x86:
            return F_procEntryExit1_x86(frame, stm);
        case AMD64:
            return F_procEntryExit1_amd64(frame, stm);
        default:
            return NULL;
    }
}

AS_instrList F_procEntryExit2(AS_instrList body) {
    switch (targetArch) {
        case x86:
            return F_procEntryExit2_x86(body);
        case AMD64:
            return F_procEntryExit2_amd64(body);
        default:
            return NULL;
    }
}

AS_proc F_procEntryExit3(F_frame frame, AS_instrList body) {
    switch (targetArch) {
        case x86:
            return F_procEntryExit3_x86(frame, body);
        case AMD64:
            return F_procEntryExit3_amd64(frame, body);
        default:
            return NULL;
    }
}

//void F_procEntryExit(void) {
//    switch (targetArch) {
//        case x86:
//            return F_procEntryExit_x86();
//        case AMD64:
//            return F_procEntryExit_amd64();
//        default:
//            return NULL;
//    }
//}



