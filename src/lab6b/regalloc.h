/* function prototype from regalloc.c */
#define RA_K 6

struct RA_result {Temp_map coloring; AS_instrList il;};
struct RA_result RA_regAlloc(F_frame f, AS_instrList il, bool verbose);
