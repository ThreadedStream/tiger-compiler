project(tiger C)
cmake_minimum_required(VERSION 3.19)

#find_package(FLEX)
#find_package(BISON)
#
#BISON_TARGET(tiger_parser tiger.y ${CMAKE_CURRENT_SOURCE_DIR}/tiger_parser.c)
#FLEX_TARGET(tiger_scanner tiger.lex ${CMAKE_CURRENT_SOURCE_DIR}/tiger_lex.c)
#ADD_FLEX_BISON_DEPENDENCY(tiger_scanner tiger_parser)


add_executable(tiger
        lex.yy.c
        y.tab.c
        parse.c
        absyn.h
        absyn.c
        assem.h
        assem.c
        canon.h
        canon.c
#        codegenx86.h
#        codegenx86.c
        codegenx86_64.h
        codegenx86_64.c
        color.h
        color.c
        env.h
        env.c
        errormsg.h
        errormsg.c
        escape.h
        escape.c
        flowgraph.h
        flowgraph.c
        frame.h
        graph.h
        graph.c
        liveness.h
        liveness.c
        main.c
        parse.h
        prabsyn.h
        prabsyn.c
        printtree.h
        printtree.c
        regalloc.h
        regalloc.c
        semant.h
        semant.c
        symbol.h
        symbol.c
        table.h
        table.c
        temp.h
        temp.c
        translate.h
        translate.c
        tree.h
        tree.c
        types.h
        types.c
        util.h
        util.c
        x86-64frame.c
)
