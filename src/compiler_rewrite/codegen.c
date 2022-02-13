#include "amd64/codegenamd64.h"
#include "x86/codegenx86.h"
#include "codegen.h"
#include "tree.h"
#include "frame.h"
#include "base.h"

AS_instrList F_codegen(F_frame f, T_stmList stmList) {
    switch (targetArch) {
        case x86:
            return F_codegen_x86(f, stmList);
        case AMD64:
            return F_codegen_amd64(f, stmList);
        default:
            return NULL;
    }
}