#pragma once

F_frame F_newFrame_amd64(Temp_label name, U_boolList formals);

int F_accessOffset_amd64(F_access a);

Temp_temp F_accessReg_amd64(F_access a);

F_accessList F_AccessList_amd64(F_access head, F_accessList tail);

F_frag F_StringFrag_amd64(Temp_label label, string str);

F_frag F_ProcFrag_amd64(T_stm body, F_frame frame);

F_frag F_newProcFrag_amd64(T_stm body, F_frame frame);

string F_string_amd64(Temp_label lab, string str);

F_fragList F_FragList_amd64(F_frag head, F_fragList tail);

T_stm F_procEntryExit1_amd64(F_frame frame, T_stm stm);

AS_instrList F_procEntryExit2_amd64(AS_instrList body);

AS_proc F_procEntryExit3_amd64(F_frame frame, AS_instrList body);

Temp_temp F_FP_amd64(void);

Temp_temp F_SP_amd64(void);

Temp_temp F_ZERO_amd64(void);

Temp_temp F_RA_amd64(void);

Temp_temp F_RV_amd64(void);

Temp_temp F_AX_amd64(void);

Temp_temp F_DX_amd64(void);

void F_initRegisters_amd64(void);

Temp_map F_initialRegisters_amd64(F_frame f);

Temp_tempList F_registers_amd64(void);

Temp_tempList F_callersaves_amd64(void);

Temp_tempList F_calleesaves_amd64(void);

string F_getlabel_amd64(F_frame frame);

T_exp F_Exp_amd64(F_access acc, T_exp framePtr);


Temp_tempList F_argregisters_amd64(void);

F_accessList F_formals_amd64(F_frame f);

Temp_label F_name_amd64(F_frame f);

F_access F_allocLocal_amd64(F_frame f, bool escape);

F_accessList F_formals_amd64(F_frame f);

T_exp F_ExpWithStaticLink_amd64(F_access acc, T_exp staticLink);

T_exp F_FPExp_amd64(T_exp framePtr);

T_exp F_staticLinkExp_amd64(T_exp framePtr);

T_exp F_upperStaticLinkExp_amd64(T_exp staticLink);

T_exp F_staticLink2FP_amd64(T_exp staticLink);

T_exp F_externalCall_amd64(string s, T_expList args);
