#pragma once

F_frame F_newFrame_x86(Temp_label name, U_boolList formals);

int F_accessOffset_x86(F_access a);

Temp_temp F_accessReg_x86(F_access a);

F_accessList F_AccessList_x86(F_access head, F_accessList tail);

F_frag F_StringFrag_x86(Temp_label label, string str);

F_frag F_ProcFrag_x86(T_stm body, F_frame frame);

F_frag F_newProcFrag_x86(T_stm body, F_frame frame);

string F_string_x86(Temp_label lab, string str);

F_fragList F_FragList_x86(F_frag head, F_fragList tail);

T_stm F_procEntryExit1_x86(F_frame frame, T_stm stm);

AS_instrList F_procEntryExit2_x86(AS_instrList body);

AS_proc F_procEntryExit3_x86(F_frame frame, AS_instrList body);

Temp_temp F_FP_x86(void);

F_access F_allocLocal_x86(F_frame f, bool escape);

Temp_temp F_SP_x86(void);

Temp_temp F_ZERO_x86(void);

Temp_temp F_RA_x86(void);

Temp_temp F_RV_x86(void);

Temp_temp F_AX_x86(void);

Temp_temp F_DX_x86(void);

void F_initRegisters_x86(void);

Temp_map F_initialRegisters_x86(F_frame f);

Temp_tempList F_registers_x86(void);

Temp_tempList F_callersaves_x86(void);

Temp_tempList F_calleesaves_x86(void);

string F_getlabel_x86(F_frame frame);

F_accessList F_formals_x86(F_frame f);

Temp_label F_name_x86(F_frame f);

T_exp F_Exp_x86(F_access acc, T_exp framePtr);

T_exp F_ExpWithStaticLink_x86(F_access acc, T_exp staticLink);

T_exp F_FPExp_x86(T_exp framePtr);

T_exp F_staticLinkExp_x86(T_exp framePtr);

T_exp F_upperStaticLinkExp_x86(T_exp staticLink);

T_exp F_staticLink2FP_x86(T_exp staticLink);

T_exp F_externalCall_x86(string s, T_expList args);





