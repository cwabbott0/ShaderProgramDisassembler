#include "Disasm.h"
#include <stdio.h>
#include <inttypes.h>
#include <sstream>
#include <iomanip>
#include <vector>
#include <string>
#include <string.h>
#include <assert.h>

// return bits (high, lo]
static uint64_t bits(uint32_t word, unsigned lo, unsigned high)
{
	if (high == 32)
		return word >> lo;
	return (word & ((1 << high) - 1)) >> lo;
}

struct Regs {
	uint64_t uniformConst : 8;
	uint64_t reg2 : 6;
	uint64_t reg3 : 6;
	uint64_t reg0 : 5;
	uint64_t reg1 : 6;
	uint64_t ctrl : 4;
};

enum RegWriteUnit {
	RegWriteNone = 0, // don't write
	RegWrite2, // write using reg2
	RegWrite3, // write using reg3
};

// this represents the decoded version of the ctrl register field.
struct RegCtrl {
	bool readReg0;
	bool readReg1;
	bool readReg3;
	RegWriteUnit FMAWriteUnit;
	RegWriteUnit ADDWriteUnit;
	bool clauseStart;
};

static RegCtrl DecodeRegCtrl(Regs regs)
{
	RegCtrl decoded = {};
	unsigned ctrl;
	if (regs.ctrl == 0) {
		ctrl = regs.reg1 >> 2;
		decoded.readReg0 = !(regs.reg1 & 0x2);
		if ((regs.reg1 & 0x1))
			printf("# unknown regctrl bit set\n");
		decoded.readReg1 = false;
	} else {
		ctrl = regs.ctrl;
		decoded.readReg0 = decoded.readReg1 = true;
	}
	switch (ctrl) {
		case 1:
			decoded.FMAWriteUnit = RegWrite2;
			break;
		case 3:
			decoded.FMAWriteUnit = RegWrite2;
			decoded.readReg3 = true;
			break;
		case 4:
			decoded.readReg3 = true;
			break;
		case 5:
			decoded.ADDWriteUnit = RegWrite2;
			break;
		case 6:
			decoded.ADDWriteUnit = RegWrite2;
			decoded.readReg3 = true;
			break;
		case 8:
			decoded.clauseStart = true;
			break;
		case 9:
			decoded.FMAWriteUnit = RegWrite2;
			decoded.clauseStart = true;
			break;
		case 11:
			break;
		case 12:
			decoded.readReg3 = true;
			decoded.clauseStart = true;
			break;
		case 13:
			decoded.ADDWriteUnit = RegWrite2;
			decoded.clauseStart = true;
		case 15:
			decoded.FMAWriteUnit = RegWrite2;
			decoded.ADDWriteUnit = RegWrite3;
			break;
		default:
			printf("# unknown reg ctrl %d\n", ctrl);
	}

	return decoded;
}

// Pass in the ADDWriteUnit or FMAWriteUnit, and this returns which register
// the ADD/FMA units are writing to
static unsigned GetRegToWrite(RegWriteUnit unit, Regs regs)
{
	switch (unit) {
		case RegWrite2:
			return regs.reg2;
		case RegWrite3:
			return regs.reg3;
		case RegWriteNone:
			assert(0);
	}
}

static void DumpRegs(Regs srcs)
{
	printf("reg0: R%d\n", srcs.reg0);
	printf("reg1: R%d\n", srcs.reg1);
	printf("reg2: R%d\n", srcs.reg2);
	printf("reg3: R%d\n", srcs.reg3);
	printf("ctrl: %d\n", srcs.ctrl);
	if (srcs.uniformConst) {
		if (srcs.uniformConst & 0x80) {
			printf("uniform: U%d\n", (srcs.uniformConst & 0x7f) * 2);
		}
	}
}

static void DumpConstImm(uint32_t imm)
{
	union {
		float f;
		uint32_t i;
	} fi;
	fi.i = imm;
	printf("%f (0x%08x)", fi.f, imm);
}

static void DumpSrc(unsigned src, Regs srcs, uint64_t *consts, bool isFMA)
{
	switch (src) {
		case 0: printf("R%d", srcs.reg0); break;
		case 1: printf("R%d", srcs.reg1); break;
		case 2: printf("R%d", srcs.reg3); break;
		case 3:
				if (isFMA)
					printf("0");
				else
					printf("T"); // i.e. the output of FMA this cycle
				break;
		case 4: {
			if (srcs.uniformConst & 0x80) {
				printf("U%d", (srcs.uniformConst & 0x7f) * 2);
			} else {
				unsigned low_bits = srcs.uniformConst & 0xf;
				uint32_t imm;
				bool valid = true;
				switch (srcs.uniformConst >> 4) {
					case 4: imm = ((uint32_t) consts[0]) | low_bits; break;
					case 5: imm = ((uint32_t) consts[1]) | low_bits; break;
					case 6: imm = ((uint32_t) consts[2]) | low_bits; break;
					case 7: imm = ((uint32_t) consts[3]) | low_bits; break;
					case 2: imm = ((uint32_t) consts[4]) | low_bits; break;
					default: valid = false; break;
				}
				if (valid)
					DumpConstImm(imm);
				else
					printf("unkConstSrc");
			}
			break;
		}
		case 5: {
			if (srcs.uniformConst & 0x80) {
				printf("U%d", (srcs.uniformConst & 0x7f) * 2 + 1);
			} else {
				uint32_t imm;
				bool valid = true;
				switch (srcs.uniformConst >> 4) {
					case 4: imm = (uint32_t)(consts[0] >> 32); break;
					case 5: imm = (uint32_t)(consts[1] >> 32); break;
					case 6: imm = (uint32_t)(consts[2] >> 32); break;
					case 7: imm = (uint32_t)(consts[3] >> 32); break;
					case 2: imm = (uint32_t)(consts[4] >> 32); break;
					default: valid = false; break;
				}
				if (valid)
					DumpConstImm(imm);
				else
					printf("unkConstSrc");
			}
			break;
		}
		case 6: printf("T0"); break;
		case 7: printf("T1"); break;
	}
}

static void DumpOutputMod(unsigned mod)
{
	switch (mod) {
		case 0:
			break;
		case 1:
			printf(".sat0"); break; // max(out, 0)
		case 2:
			printf(".unk2"); break;
		case 3:
			printf(".sat"); break; // clamp(out, 0, 1)
		default:
			break;
	}
}

struct FMA {
	uint64_t src0 : 3;
	uint64_t op : 20;
};

enum FMASrcType {
	FMAOneSrc,
	FMATwoSrc,
	FMATwoSrcFmod,
	FMAFcmp,
	FMAThreeSrc,
	FMAThreeSrcFmod,
	FMAFourSrc,
};

struct FMAOpInfo {
	unsigned op;
	char name[20];
	FMASrcType srcType;
};

static const FMAOpInfo FMAOpInfos[] = {
	{ 0x00000, "FMA",  FMAThreeSrcFmod },
	{ 0x40000, "FMAX", FMATwoSrcFmod },
	{ 0x44000, "FMIN", FMATwoSrcFmod },
	{ 0x48000, "FCMP.GL", FMAFcmp },
	{ 0x4c000, "FCMP.D3D", FMAFcmp },
	{ 0x4ff98, "ADD", FMATwoSrc },
	{ 0x4ffd8, "SUB", FMATwoSrc },
	{ 0x4fff0, "SUBB", FMATwoSrc },
	{ 0x50180, "FRCP_PT5", FMATwoSrc },
	{ 0x528c0, "FRCP_PT3", FMAThreeSrc },
	{ 0x58000, "FADD", FMATwoSrcFmod },
	{ 0x5c000, "CSEL.FEQ", FMAFourSrc },
	{ 0x5c200, "CSEL.FGT", FMAFourSrc },
	{ 0x5c400, "CSEL.FGE", FMAFourSrc },
	{ 0x5c600, "CSEL.IEQ", FMAFourSrc },
	{ 0x5c800, "CSEL.IGT", FMAFourSrc },
	{ 0x5ca00, "CSEL.IGE", FMAFourSrc },
	{ 0x5cc00, "CSEL.UGT", FMAFourSrc },
	{ 0x5ce00, "CSEL.UGE", FMAFourSrc },
	{ 0x5de40, "ICMP.GL.GT", FMATwoSrc }, // src0 > src1 ? 1 : 0
	{ 0x5de48, "ICMP.GL.GE", FMATwoSrc },
	{ 0x5de50, "UCMP.GL.GT", FMATwoSrc },
	{ 0x5de58, "UCMP.GL.GE", FMATwoSrc },
	{ 0x5de60, "ICMP.GL.EQ", FMATwoSrc },
	{ 0x5dec0, "ICMP.D3D.GT", FMATwoSrc }, // src0 > src1 ? ~0 : 0
	{ 0x5dec8, "ICMP.D3D.GE", FMATwoSrc },
	{ 0x5ded0, "UCMP.D3D.GT", FMATwoSrc },
	{ 0x5ded8, "UCMP.D3D.GE", FMATwoSrc },
	{ 0x5dee0, "ICMP.D3D.EQ", FMATwoSrc },
	{ 0x60200, "RSHIFT_NAND", FMAThreeSrc },
	{ 0x60e00, "RSHIFT_OR", FMAThreeSrc },
	{ 0x61200, "RSHIFT_AND", FMAThreeSrc },
	{ 0x61e00, "RSHIFT_NOR", FMAThreeSrc }, // ~((src0 << src2) | src1)
	{ 0x62200, "LSHIFT_NAND", FMAThreeSrc },
	{ 0x62e00, "LSHIFT_OR",  FMAThreeSrc }, // (src0 << src2) | src1
	{ 0x63200, "LSHIFT_AND", FMAThreeSrc }, // (src0 << src2) & src1
	{ 0x63e00, "LSHIFT_NOR", FMAThreeSrc },
	{ 0x64200, "RSHIFT_XOR", FMAThreeSrc },
	{ 0x64600, "RSHIFT_XNOR", FMAThreeSrc }, // ~((src0 >> src2) ^ src1)
	{ 0x64a00, "LSHIFT_XOR", FMAThreeSrc },
	{ 0x64e00, "LSHIFT_XNOR", FMAThreeSrc }, // ~((src0 >> src2) ^ src1)
	{ 0x65200, "LSHIFT_ADD", FMAThreeSrc },
	{ 0x65600, "LSHIFT_SUB", FMAThreeSrc }, // (src0 << src2) - src1
	{ 0x65a00, "LSHIFT_RSUB", FMAThreeSrc }, // src1 - (src0 << src2)
	{ 0x65e00, "RSHIFT_ADD", FMAThreeSrc },
	{ 0x66200, "RSHIFT_SUB", FMAThreeSrc },
	{ 0x66600, "RSHIFT_RSUB", FMAThreeSrc },
	{ 0x66a00, "ARSHIFT_ADD", FMAThreeSrc },
	{ 0x66e00, "ARSHIFT_SUB", FMAThreeSrc },
	{ 0x67200, "ARSHIFT_RSUB", FMAThreeSrc },
	{ 0xcfc10, "ADDC", FMATwoSrc },
	{ 0xe0136, "F2I", FMAOneSrc },
	{ 0xe0137, "F2U", FMAOneSrc },
	{ 0xe0178, "I2F", FMAOneSrc },
	{ 0xe0179, "U2F", FMAOneSrc },
	{ 0xe032d, "MOV",  FMAOneSrc },
	{ 0xe0365, "FRCP_PT1", FMAOneSrc },
	{ 0xe038d, "FRCP_PT4", FMAOneSrc },
	{ 0xe0b80, "IMAX", FMATwoSrc },
	{ 0xe0bc0, "UMAX", FMATwoSrc },
	{ 0xe0c00, "IMIN", FMATwoSrc },
	{ 0xe0c40, "UMIN", FMATwoSrc },
	{ 0xe0f40, "CSEL", FMAThreeSrc }, // src2 != 0 ? src1 : src0
	{ 0xe7800, "IMAD", FMAThreeSrc },
	{ 0xe78db, "POPCNT", FMAOneSrc },
};

#define ARRAY_SIZE(x) (sizeof(x) / sizeof(x[0]))

static FMAOpInfo findFMAOpInfo(unsigned op)
{
	for (int i = 0; i < ARRAY_SIZE(FMAOpInfos); i++) {
		unsigned opCmp;
		switch (FMAOpInfos[i].srcType) {
			case FMAOneSrc:
				opCmp = op;
				break;
			case FMATwoSrc:
				opCmp = op & ~0x7;
				break;
			case FMAFcmp:
				opCmp = op & ~0x1fff;
				break;
			case FMAThreeSrc:
				opCmp = op & ~0x3f;
				break;
			case FMATwoSrcFmod:
				opCmp = op & ~0x3fff;
				break;
			case FMAThreeSrcFmod:
				opCmp = op & ~0x3ffff;
				break;
			case FMAFourSrc:
				opCmp = op & ~0x1ff;
				break;
		}
		if (FMAOpInfos[i].op == opCmp)
			return FMAOpInfos[i];
	}

	FMAOpInfo info;
	snprintf(info.name, sizeof(info.name), "op%04x", op);
	info.op = op;
	info.srcType = FMAThreeSrc;
	return info;
}

static void DumpFCMP(unsigned op)
{
		switch (op) {
			case 0:
				printf(".OEQ");
				break;
			case 1:
				printf(".OGT");
				break;
			case 2:
				printf(".OGE");
				break;
			case 3:
				printf(".UNE");
				break;
			case 4:
				printf(".OLT");
				break;
			case 5:
				printf(".OLE");
				break;
			default:
				printf(".unk%d", op);
				break;
		}
}

static void DumpFMA(uint64_t word, Regs regs, Regs nextRegs, uint64_t *consts)
{
	printf("# FMA: %016" PRIx64 "\n", word);
	FMA FMA;
	memcpy((char *) &FMA, (char *) &word, sizeof(FMA));
	FMAOpInfo info = findFMAOpInfo(FMA.op);

	printf("%s", info.name);
	if (info.srcType == FMATwoSrcFmod ||
		info.srcType == FMAThreeSrcFmod) {
		// output modifiers
		DumpOutputMod(bits(FMA.op, 12, 14));
	} else if (info.srcType == FMAFcmp) {
		DumpFCMP(bits(FMA.op, 10, 13));
	}

	printf(" ");

	RegCtrl nextCtrl = DecodeRegCtrl(nextRegs);
	if (nextCtrl.FMAWriteUnit != RegWriteNone) {
		printf("{R%d, T0}, ", GetRegToWrite(nextCtrl.FMAWriteUnit, nextRegs));
	} else {
		printf("T0, ");
	}

	switch (info.srcType) {
		case FMAOneSrc:
			DumpSrc(FMA.src0, regs, consts, true);
			break;
		case FMATwoSrc:
			DumpSrc(FMA.src0, regs, consts, true);
			printf(", ");
			DumpSrc(FMA.op & 0x7, regs, consts, true);
			break;
		case FMATwoSrcFmod:
			if (FMA.op & 0x10)
				printf("-");
			DumpSrc(FMA.src0, regs, consts, true);
			printf(", ");
			if (FMA.op & 0x20)
				printf("-");
			DumpSrc(FMA.op & 0x7, regs, consts, true);
			break;
		case FMAFcmp:
			if (FMA.op & 0x200)
				printf("abs(");
			DumpSrc(FMA.src0, regs, consts, true);
			if (FMA.op & 0x200)
				printf(")");
			printf(", ");
			if (FMA.op & 0x20)
				printf("-");
			if (FMA.op & 0x8)
				printf("abs(");
			DumpSrc(FMA.op & 0x7, regs, consts, true);
			if (FMA.op & 0x8)
				printf(")");
			break;
		case FMAThreeSrc:
			DumpSrc(FMA.src0, regs, consts, true);
			printf(", ");
			DumpSrc(FMA.op & 0x7, regs, consts, true);
			printf(", ");
			DumpSrc((FMA.op >> 3) & 0x7, regs, consts, true);
			break;
		case FMAThreeSrcFmod:
			if (FMA.op & (1 << 14))
				printf("-");
			DumpSrc(FMA.src0, regs, consts, true);
			printf(", ");
			DumpSrc(FMA.op & 0x7, regs, consts, true);
			printf(", ");
			if (FMA.op & (1 << 15))
				printf("-");
			DumpSrc((FMA.op >> 3) & 0x7, regs, consts, true);
			break;
		case FMAFourSrc:
			DumpSrc(FMA.src0, regs, consts, true);
			printf(", ");
			DumpSrc(FMA.op & 0x7, regs, consts, true);
			printf(", ");
			DumpSrc((FMA.op >> 3) & 0x7, regs, consts, true);
			printf(", ");
			DumpSrc((FMA.op >> 6) & 0x7, regs, consts, true);
			break;
	}
	printf("\n");
}

struct ADD {
	uint64_t src0 : 3;
	uint64_t op : 17;
};

enum ADDSrcType {
	ADDOneSrc,
	ADDTwoSrc,
	ADDTwoSrcFmod,
	ADDFcmp,
};

struct ADDOpInfo {
	unsigned op;
	char name[20];
	ADDSrcType srcType;
};

static const ADDOpInfo ADDOpInfos[] = {
	{ 0x00000, "FMAX", ADDTwoSrcFmod },
	{ 0x02000, "FMIN", ADDTwoSrcFmod },
	{ 0x04000, "FADD", ADDTwoSrcFmod },
	{ 0x06000, "FCMP.GL", ADDFcmp },
	{ 0x07000, "FCMP.D3D", ADDFcmp },
	{ 0x07936, "F2I", ADDOneSrc },
	{ 0x07937, "F2U", ADDOneSrc },
	{ 0x07978, "I2F", ADDOneSrc },
	{ 0x07979, "U2F", ADDOneSrc },
	{ 0x07b28, "MOV",  ADDTwoSrc }, // the two sources are always the same?
	//{ 0x0c000, "SKIP", ADDOneSrc }, // skip decoding this instruction
	{ 0x0ce00, "FRCP_PT2", ADDOneSrc },
	{ 0x0f640, "ICMP.GL.GT", ADDTwoSrc }, // src0 > src1 ? 1 : 0
	{ 0x0f648, "ICMP.GL.GE", ADDTwoSrc },
	{ 0x0f650, "UCMP.GL.GT", ADDTwoSrc },
	{ 0x0f658, "UCMP.GL.GE", ADDTwoSrc },
	{ 0x0f660, "ICMP.GL.EQ", ADDTwoSrc },
	{ 0x0f6c0, "ICMP.D3D.GT", ADDTwoSrc }, // src0 > src1 ? ~0 : 0
	{ 0x0f6c8, "ICMP.D3D.GE", ADDTwoSrc },
	{ 0x0f6d0, "UCMP.D3D.GT", ADDTwoSrc },
	{ 0x0f6d8, "UCMP.D3D.GE", ADDTwoSrc },
	{ 0x0f6e0, "ICMP.D3D.EQ", ADDTwoSrc },
	//{ 0x10000, "CONST", ADDOneSrc }, // this is actually a constant
	{ 0x178c0, "ADD",  ADDTwoSrc },
	{ 0x17ac0, "SUB",  ADDTwoSrc },
	{ 0x17c10, "ADDC", ADDTwoSrc }, // adds src0 to the bottom bit of src1
	{ 0x1dd18, "OR",  ADDTwoSrc },
	{ 0x1dd60, "LSHIFT", ADDTwoSrc },
	{ 0x1dd20, "AND",  ADDTwoSrc },
	{ 0x1dd50, "XOR",  ADDTwoSrc },
	{ 0x1dd84, "RSHIFT", ADDTwoSrc },
	{ 0x1dda4, "ARSHIFT", ADDTwoSrc },
};

static ADDOpInfo findADDOpInfo(unsigned op)
{
	for (int i = 0; i < ARRAY_SIZE(ADDOpInfos); i++) {
		unsigned opCmp;
		switch (ADDOpInfos[i].srcType) {
			case ADDOneSrc:
				opCmp = op;
				break;
			case ADDTwoSrc:
				opCmp = op & ~0x7;
				break;
			case ADDTwoSrcFmod:
				opCmp = op & ~0x3ff;
				break;
			case ADDFcmp:
				opCmp = op & ~0x7ff;
				break;
		}
		if (ADDOpInfos[i].op == opCmp)
			return ADDOpInfos[i];
	}

	ADDOpInfo info;
	snprintf(info.name, sizeof(info.name), "op%04x", op);
	info.op = op;
	info.srcType = ADDTwoSrc;
	return info;
}

static void DumpADD(uint64_t word, Regs regs, Regs nextRegs, uint64_t *consts)
{
	printf("# ADD: %016" PRIx64 "\n", word);
	ADD ADD;
	memcpy((char *) &ADD, (char *) &word, sizeof(ADD));
	ADDOpInfo info = findADDOpInfo(ADD.op);

	printf("%s", info.name);
	if (info.srcType == ADDTwoSrcFmod) {
		// output modifiers
		DumpOutputMod(bits(ADD.op, 8, 10));
	} else if (info.srcType == ADDFcmp) {
		DumpFCMP(bits(ADD.op, 3, 6));
	}
	printf(" ");

	RegCtrl nextCtrl = DecodeRegCtrl(nextRegs);
	if (nextCtrl.ADDWriteUnit != RegWriteNone) {
		printf("{R%d, T1}, ", GetRegToWrite(nextCtrl.ADDWriteUnit, nextRegs));
	} else {
		printf("T1, ");
	}

	switch (info.srcType) {
		case ADDOneSrc:
			DumpSrc(ADD.src0, regs, consts, false);
			break;
		case ADDTwoSrc:
			DumpSrc(ADD.src0, regs, consts, false);
			printf(", ");
			DumpSrc(ADD.op & 0x7, regs, consts, false);
			break;
		case ADDTwoSrcFmod:
			if (ADD.op & 0x10)
				printf("-");
			DumpSrc(ADD.src0, regs, consts, false);
			printf(", ");
			if (ADD.op & 0x20)
				printf("-");
			DumpSrc(ADD.op & 0x7, regs, consts, false);
			break;
		case ADDFcmp:
			if (ADD.op & 0x400) {
				printf("-");
			}
			if (ADD.op & 0x100) {
				printf("abs(");
			}
			DumpSrc(ADD.src0, regs, consts, false);
			if (ADD.op & 0x100) {
				printf(")");
			}
			printf(", ");
			if (ADD.op & 0x200) {
				printf("abs(");
			}
			DumpSrc(ADD.op & 0x7, regs, consts, false);
			if (ADD.op & 0x200) {
				printf(")");
			}
	}
	printf("\n");
}

// each of these structs represents an instruction that's dispatched in one
// cycle. Note that these instructions are packed in funny ways within the
// clause, hence the need for a separate struct.
struct AluInstr {
	uint64_t regBits;
	uint64_t FMABits;
	uint64_t ADDBits;
};

void DumpInstr(const AluInstr &instr, Regs nextRegs, uint64_t *consts)
{
	printf("# regs: %016" PRIx32 "\n", instr.regBits);
	Regs regs;
	memcpy((char *) &regs, (char *) &instr.regBits, sizeof(regs));
	DumpRegs(regs);
	DumpFMA(instr.FMABits, regs, nextRegs, consts);
	DumpADD(instr.ADDBits, regs, nextRegs, consts);
}

void AddInstr(std::vector<AluInstr> &instrs, AluInstr &instr)
{
	instrs.push_back(instr);
	instr = {};
}

void DumpClause(uint32_t *words, uint32_t size)
{
	AluInstr mainInstr = {};
	AluInstr extraInstr = {};
	bool extraInstrStarted = false;
	std::vector<AluInstr> instrs;
	uint64_t consts[8] = {};
	unsigned num_consts = 0;
	uint64_t header = 0;
	for (unsigned i = 0; i < size; i++, words += 4) {
		printf("# ");
		for (int i = 0; i < 4; i++)
			printf("%08x ", words[3 - i]); // low bit on the right
		printf("\n");
		unsigned tag = bits(words[0], 3, 8);

		bool mainInstrConstant = false;
		if (tag & 0x10) {
			mainInstr.ADDBits |= (tag & 0x7) << 17;
			assert(extraInstrStarted);
			extraInstr.ADDBits = bits(words[3], 0, 17) | (bits(words[0], 0, 3) << 17);
			extraInstr.FMABits |= bits(words[2], 19, 32) << 10;
			AddInstr(instrs, extraInstr);
			extraInstrStarted = false;
			consts[num_consts] = bits(words[3], 17, 32) << 4;
		} else {
			mainInstr.ADDBits |= bits(words[0], 0, 3) << 17;
			switch (tag & 0x7) {
				case 0x0:
					if (extraInstrStarted) {
						// TODO if this comes after a 0x4 quadword, then 0 seems to
						// mean that the main instruction is a constant instead,
						// but it's not clear what the other cases mean.
						unsigned tag2 = bits(words[3], 26, 29);
						mainInstrConstant = (tag2 == 0);
						extraInstr.ADDBits = bits(words[3], 0, 17) | bits(words[3], 29, 32) << 17;
						extraInstr.FMABits |= bits(words[2], 19, 32) << 10;
						AddInstr(instrs, extraInstr);
						extraInstrStarted = false;
					}
					break;
				case 0x1:
					// used for one-quadword clause
				case 0x5:
					// used for beginning of clause
					header = bits(words[2], 19, 32) | ((uint64_t) words[3] << (32 - 19));
					break;
				case 0x2:
				case 0x3:
					consts[num_consts++] |= (bits(words[2], 19, 32) | ((uint64_t) words[3] << 13)) << 19;
					break;
				case 0x4:
					extraInstr.FMABits |= bits(words[3], 22, 32);
					extraInstr.regBits = bits(words[2], 19, 32) | (bits(words[3], 0, 22) << (32 - 19));
					extraInstrStarted = true;
					break;
				case 0x6:
				case 0x7:
					consts[num_consts + 1] = bits(words[2], 4, 32) << 4 | (uint64_t) words[3] << 32;
					mainInstrConstant = true;
					break;
				default:
					break;
			}
		}

		if (mainInstrConstant) {
			consts[num_consts] = (bits(words[0], 8, 32) << 4) | (uint64_t) words[1] << 28 | bits(words[2], 0, 4) << 60;
			if ((tag & 0x7) == 0) {
				num_consts++;
			} else {
				num_consts += 2;
			}
		} else {
			// 20 bits
			mainInstr.ADDBits |= bits(words[2], 2, 32 - 13);
			// 23 bits
			mainInstr.FMABits = bits(words[1], 11, 32) | bits(words[2], 0, 2) << (32 - 11);
			// 35 bits
			mainInstr.regBits = ((uint64_t) bits(words[1], 0, 11)) << 24 | (uint64_t) bits(words[0], 8, 32);
			AddInstr(instrs, mainInstr);
		}
	}

	printf("# header: %012" PRIx64 "\n", header);

	for (auto instr = instrs.begin(), end = instrs.end(); instr != end;
		 ++instr) {
		Regs nextRegs;
		if (instr + 1 == end) {
			memcpy((char *) &nextRegs, (char *) &instrs[0].regBits,
					sizeof(nextRegs));
		} else {
			memcpy((char *) &nextRegs, (char *) &instr[1].regBits,
					sizeof(nextRegs));
		}

		DumpInstr(*instr, nextRegs, consts);
	}

	for (int i = 0; i < num_consts; i++) {
		printf("# const%d: %08x\n", 2 * i, consts[i] & 0xffffffff);
		printf("# const%d: %08x\n", 2 * i + 1, consts[i] >> 32);
	}
}

void DumpInstructions(unsigned indent, uint8_t* instBlob, uint32_t size)
{
	uint32_t *words = (uint32_t *) instBlob;
	uint32_t *wordsEnd = words + (size / 4);
	while (words != wordsEnd)
	{
		// search for the beginning of the next clause, or the padding after
		// the program ends
		uint32_t *bundleEnd = words + 4;
		while (true) {
			if (bundleEnd == wordsEnd)
				break;
			uint32_t zero[4] = {0};
			if (memcmp(bundleEnd, zero, 4 * sizeof(uint32_t)) == 0)
				break;
			uint8_t tag = bundleEnd[0] & 0xf8;
			// clauses seem to always start with this:
			if (tag == 0x28)
				break;
			if (tag == 0x48)
				break;
			bundleEnd += 4;
		}
		printf("{\n");
		DumpClause(words, (bundleEnd - words) / 4);
		printf("}\n");
		words = bundleEnd;
	}
}

