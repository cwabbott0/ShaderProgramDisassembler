project(Disassembler)

set(SRCS Main.cpp BifrostDisasm.cpp t6xx/disassemble.c t6xx/hfloat.c)
add_executable(disasm ${SRCS})
