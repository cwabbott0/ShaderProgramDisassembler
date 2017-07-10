#include "Disasm.h"
#include <stdio.h>
#include <inttypes.h>
#include <sstream>
#include <iomanip>
#include <string>
#include <string.h>

// return bits (high, lo]
static uint64_t bits(uint32_t word, unsigned lo, unsigned high)
{
	if (high == 32)
		return word >> lo;
	return (word & ((1 << high) - 1)) >> lo;
}

struct Srcs {
	uint64_t uniformConst : 8;
	uint64_t reg2 : 6;
	uint64_t reg3 : 6;
	uint64_t reg0 : 5;
	uint64_t reg1 : 5;
	uint64_t unk1 : 5;
};

static void DumpSrcs(Srcs srcs)
{
	printf("reg0: R%d\n", srcs.reg0);
	printf("reg1: R%d\n", srcs.reg1);
	printf("reg2: R%d\n", srcs.reg2);
	printf("reg3: R%d\n", srcs.reg3);
	printf("unk1: R%d\n", srcs.unk1);
	if (srcs.uniformConst) {
		if (srcs.uniformConst & 0x80) {
			printf("uniform: U%d\n", (srcs.uniformConst & 0x7f) * 2);
		}
	}
}

static void DumpSrc(unsigned src, Srcs srcs, bool isFMA)
{
	switch (src) {
		case 0: printf("R%d", srcs.reg0); break;
		case 1: printf("R%d", srcs.reg1); break;
		// TODO reg2 or reg3? FMA sets them the same...
		case 2: printf("R%d", srcs.reg2); break;
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
				// TODO parse constants in clause
				switch (srcs.uniformConst >> 4) {
					case 4: printf("const0"); break;
					case 5: printf("const2"); break;
					case 6: printf("const4"); break;
					default: printf("unkConstSrc"); break;
				}
			}
			break;
		}
		case 5: {
			if (srcs.uniformConst & 0x80) {
				printf("U%d", (srcs.uniformConst & 0x7f) * 2 + 1);
			} else {
				switch (srcs.uniformConst >> 4) {
					case 4: printf("const1"); break;
					case 5: printf("const3"); break;
					case 6: printf("const5"); break;
					default: printf("unkConstSrc"); break;
				}
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
	uint64_t src1 : 3;
	uint64_t op : 17;
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
	{ 0x0000, "FMA",  FMAThreeSrcFmod },
	{ 0x8000, "FMAX", FMATwoSrcFmod },
	{ 0x8800, "FMIN", FMATwoSrcFmod },
	{ 0x9000, "FCMP.GL", FMAFcmp },
	{ 0x9800, "FCMP.D3D", FMAFcmp },
	{ 0x9ff3, "ADD", FMATwoSrc },
	{ 0x9ffb, "SUB", FMATwoSrc },
	{ 0xb000, "FADD", FMATwoSrcFmod },
	{ 0xb800, "CSEL.FEQ", FMAFourSrc },
	{ 0xb840, "CSEL.FGT", FMAFourSrc },
	{ 0xb880, "CSEL.FGE", FMAFourSrc },
	{ 0xb8c0, "CSEL.IEQ", FMAFourSrc },
	{ 0xb900, "CSEL.IGT", FMAFourSrc },
	{ 0xb940, "CSEL.IGE", FMAFourSrc },
	{ 0xb980, "CSEL.UGT", FMAFourSrc },
	{ 0xb9c0, "CSEL.UGE", FMAFourSrc },
	{ 0xbbc8, "ICMP.GL.GT", FMATwoSrc }, // src0 > src1 ? 1 : 0
	{ 0xbbc9, "ICMP.GL.GE", FMATwoSrc },
	{ 0xbbca, "UCMP.GL.GT", FMATwoSrc },
	{ 0xbbcb, "UCMP.GL.GE", FMATwoSrc },
	{ 0xbbcc, "ICMP.GL.EQ", FMATwoSrc },
	{ 0xbbd8, "ICMP.D3D.GT", FMATwoSrc }, // src0 > src1 ? ~0 : 0
	{ 0xbbd9, "ICMP.D3D.GE", FMATwoSrc },
	{ 0xbbda, "UCMP.D3D.GT", FMATwoSrc },
	{ 0xbbdb, "UCMP.D3D.GE", FMATwoSrc },
	{ 0xbbdc, "ICMP.D3D.EQ", FMATwoSrc },
	{ 0xc040, "RSHIFT_NAND", FMAThreeSrc },
	{ 0xc1c0, "RSHIFT_OR", FMAThreeSrc },
	{ 0xc240, "RSHIFT_AND", FMAThreeSrc },
	{ 0xc3c0, "RSHIFT_NOR", FMAThreeSrc }, // ~((src0 << src2) | src1)
	{ 0xc440, "LSHIFT_NAND", FMAThreeSrc },
	{ 0xc5c0, "LSHIFT_OR",  FMAThreeSrc }, // (src0 << src2) | src1
	{ 0xc640, "LSHIFT_AND", FMAThreeSrc }, // (src0 << src2) & src1
	{ 0xc7c0, "LSHIFT_NOR", FMAThreeSrc },
	{ 0xc840, "RSHIFT_XOR", FMAThreeSrc },
	{ 0xc8c0, "RSHIFT_XNOR", FMAThreeSrc }, // ~((src0 >> src2) ^ src1)
	{ 0xc940, "LSHIFT_XOR", FMAThreeSrc },
	{ 0xc9c0, "LSHIFT_XNOR", FMAThreeSrc }, // ~((src0 >> src2) ^ src1)
	{ 0xca40, "LSHIFT_ADD", FMAThreeSrc },
	{ 0xcac0, "LSHIFT_SUB", FMAThreeSrc }, // (src0 << src2) - src1
	{ 0xcb40, "LSHIFT_RSUB", FMAThreeSrc }, // src1 - (src0 << src2)
	{ 0xcbc0, "RSHIFT_ADD", FMAThreeSrc },
	{ 0xcc40, "RSHIFT_SUB", FMAThreeSrc },
	{ 0xccc0, "RSHIFT_RSUB", FMAThreeSrc },
	{ 0xcd40, "ARSHIFT_ADD", FMAThreeSrc },
	{ 0xcdc0, "ARSHIFT_SUB", FMAThreeSrc },
	{ 0xce40, "ARSHIFT_RSUB", FMAThreeSrc },
	{ 0x1c065, "MOV",  FMAOneSrc },
	{ 0x1c170, "IMAX", FMATwoSrc },
	{ 0x1c178, "UMAX", FMATwoSrc },
	{ 0x1c180, "IMIN", FMATwoSrc },
	{ 0x1c188, "UMIN", FMATwoSrc },
	{ 0x1c1e8, "CSEL", FMAThreeSrc }, // src2 != 0 ? src1 : src0
	{ 0x1cf00, "IMAD", FMAThreeSrc },
	{ 0x1cf1b, "POPCNT", FMAOneSrc },
};

#define ARRAY_SIZE(x) (sizeof(x) / sizeof(x[0]))

static FMAOpInfo findFMAOpInfo(unsigned op)
{
	for (int i = 0; i < ARRAY_SIZE(FMAOpInfos); i++) {
		unsigned opCmp;
		switch (FMAOpInfos[i].srcType) {
			case FMAOneSrc:
			case FMATwoSrc:
				opCmp = op;
				break;
			case FMAFcmp:
				opCmp = op & ~0x3ff;
				break;
			case FMAThreeSrc:
				opCmp = op & ~0b111;
				break;
			case FMATwoSrcFmod:
				opCmp = op & ~0x7ff;
				break;
			case FMAThreeSrcFmod:
				opCmp = op & ~0x7fff;
				break;
			case FMAFourSrc:
				opCmp = op & ~0b111111;
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

static void DumpFMA(uint64_t word, Srcs srcs)
{
	printf("# FMA: %016" PRIx64 "\n", word);
	FMA FMA;
	memcpy((char *) &FMA, (char *) &word, sizeof(FMA));
	FMAOpInfo info = findFMAOpInfo(FMA.op);

	printf("%s", info.name);
	if (info.srcType == FMATwoSrcFmod ||
		info.srcType == FMAThreeSrcFmod) {
		// output modifiers
		DumpOutputMod(bits(FMA.op, 9, 11));
	} else if (info.srcType == FMAFcmp) {
		DumpFCMP(bits(FMA.op, 7, 10));
	}

	printf(" ");
	printf("T0, ");
	// TODO figure out dest
	switch (info.srcType) {
		case FMAOneSrc:
			DumpSrc(FMA.src0, srcs, true);
			break;
		case FMATwoSrc:
			DumpSrc(FMA.src0, srcs, true);
			printf(", ");
			DumpSrc(FMA.src1, srcs, true);
			break;
		case FMATwoSrcFmod:
			if (FMA.op & 0x2)
				printf("-");
			DumpSrc(FMA.src0, srcs, true);
			printf(", ");
			if (FMA.op & 0x4)
				printf("-");
			DumpSrc(FMA.src1, srcs, true);
			break;
		case FMAFcmp:
			if (FMA.op & 0x40)
				printf("abs(");
			DumpSrc(FMA.src0, srcs, true);
			if (FMA.op & 0x40)
				printf(")");
			printf(", ");
			if (FMA.op & 0x4)
				printf("-");
			if (FMA.op & 0x1)
				printf("abs(");
			DumpSrc(FMA.src1, srcs, true);
			if (FMA.op & 0x1)
				printf(")");
			break;
		case FMAThreeSrc:
			DumpSrc(FMA.src0, srcs, true);
			printf(", ");
			DumpSrc(FMA.src1, srcs, true);
			printf(", ");
			DumpSrc(FMA.op & 0x7, srcs, true);
			break;
		case FMAThreeSrcFmod:
			if (FMA.op & (1 << 11))
				printf("-");
			DumpSrc(FMA.src0, srcs, true);
			printf(", ");
			DumpSrc(FMA.src1, srcs, true);
			printf(", ");
			if (FMA.op & (1 << 12))
				printf("-");
			DumpSrc(FMA.op & 0x7, srcs, true);
			break;
		case FMAFourSrc:
			DumpSrc(FMA.src0, srcs, true);
			printf(", ");
			DumpSrc(FMA.src1, srcs, true);
			printf(", ");
			DumpSrc(FMA.op & 0x7, srcs, true);
			printf(", ");
			DumpSrc((FMA.op >> 3) & 0x7, srcs, true);
			break;
	}
	printf("\n");
}

struct ADD {
	uint64_t src0 : 3;
	uint64_t src1 : 3;
	uint64_t op : 14;
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
	{ 0x0000, "FMAX", ADDTwoSrcFmod },
	{ 0x0400, "FMIN", ADDTwoSrcFmod },
	{ 0x0800, "FADD", ADDTwoSrcFmod },
	{ 0x0c00, "FCMP.GL", ADDFcmp },
	{ 0x0e00, "FCMP.D3D", ADDFcmp },
	{ 0x0f65, "MOV",  ADDOneSrc },
	{ 0x1ec8, "ICMP.GL.GT", ADDTwoSrc }, // src0 > src1 ? 1 : 0
	{ 0x1ec9, "ICMP.GL.GE", ADDTwoSrc },
	{ 0x1eca, "UCMP.GL.GT", ADDTwoSrc },
	{ 0x1ecb, "UCMP.GL.GE", ADDTwoSrc },
	{ 0x1ecc, "ICMP.GL.EQ", ADDTwoSrc },
	{ 0x1ed8, "ICMP.D3D.GT", ADDTwoSrc }, // src0 > src1 ? ~0 : 0
	{ 0x1ed9, "ICMP.D3D.GE", ADDTwoSrc },
	{ 0x1eda, "UCMP.D3D.GT", ADDTwoSrc },
	{ 0x1edb, "UCMP.D3D.GE", ADDTwoSrc },
	{ 0x1edc, "ICMP.D3D.EQ", ADDTwoSrc },
	{ 0x2f18, "ADD",  ADDTwoSrc },
	{ 0x2f58, "SUB",  ADDTwoSrc },
	{ 0x3ba3, "OR",  ADDTwoSrc },
	{ 0x3bac, "LSHIFT", ADDTwoSrc },
	{ 0x3ba4, "AND",  ADDTwoSrc },
	{ 0x3baa, "XOR",  ADDTwoSrc },
};

static ADDOpInfo findADDOpInfo(unsigned op)
{
	for (int i = 0; i < ARRAY_SIZE(ADDOpInfos); i++) {
		unsigned opCmp;
		switch (ADDOpInfos[i].srcType) {
			case ADDOneSrc:
			case ADDTwoSrc:
				opCmp = op;
				break;
			case ADDTwoSrcFmod:
				opCmp = op & ~0x7f;
				break;
			case ADDFcmp:
				opCmp = op & ~0xff;
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

static void DumpADD(uint64_t word, Srcs srcs)
{
	printf("# ADD: %016" PRIx64 "\n", word);
	ADD ADD;
	memcpy((char *) &ADD, (char *) &word, sizeof(ADD));
	ADDOpInfo info = findADDOpInfo(ADD.op);

	printf("%s", info.name);
	if (info.srcType == ADDTwoSrcFmod) {
		// output modifiers
		DumpOutputMod(bits(ADD.op, 5, 7));
	} else if (info.srcType == ADDFcmp) {
		switch (bits(ADD.op, 0, 3)) {
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
		}
	}
	printf(" ");
	printf("T1, ");
	switch (info.srcType) {
		case ADDOneSrc:
			DumpSrc(ADD.src0, srcs, false);
			break;
		case ADDTwoSrc:
			DumpSrc(ADD.src0, srcs, false);
			printf(", ");
			DumpSrc(ADD.src1, srcs, false);
			break;
		case ADDTwoSrcFmod:
			if (ADD.op & 0x2)
				printf("-");
			DumpSrc(ADD.src0, srcs, false);
			printf(", ");
			if (ADD.op & 0x4)
				printf("-");
			DumpSrc(ADD.src1, srcs, false);
			break;
		case ADDFcmp:
			if (ADD.op & 0x80) {
				printf("-");
			}
			if (ADD.op & 0x20) {
				printf("abs(");
			}
			DumpSrc(ADD.src0, srcs, false);
			if (ADD.op & 0x20) {
				printf(")");
			}
			printf(", ");
			if (ADD.op & 0x40) {
				printf("abs(");
			}
			DumpSrc(ADD.src1, srcs, false);
			if (ADD.op & 0x40) {
				printf(")");
			}
	}
	printf("\n");
}

// each of these structs represents an instruction that's dispatched in one
// cycle. Note that these instructions are packed in funny ways within the
// clause, hence the need for a separate struct.
struct AluInstr {
	uint64_t srcBits;
	uint64_t FMABits;
	uint64_t ADDBits;
};

void DumpInstr(AluInstr &instr)
{
	printf("# srcs: %016" PRIx32 "\n", instr.srcBits);
	Srcs srcs;
	memcpy((char *) &srcs, (char *) &instr.srcBits, sizeof(srcs));
	DumpSrcs(srcs);
	instr.srcBits = 0;
	DumpFMA(instr.FMABits, srcs);
	instr.FMABits = 0;
	DumpADD(instr.ADDBits, srcs);
	instr.ADDBits = 0;
}

void DumpClause(uint32_t *words, uint32_t size)
{
	AluInstr instrs[2] = { {0, 0, 0}, {0, 0, 0} };
	uint64_t consts[3] = { 0, 0, 0 };
	unsigned num_consts = 0;
	for (unsigned i = 0; i < size; i++, words += 4) {
		printf("# ");
		for (int i = 0; i < 4; i++)
			printf("%08x ", words[3 - i]); // low bit on the right
		printf("\n");
		unsigned tag = bits(words[0], 3, 8);

		if (tag & 0x10) {
			instrs[0].ADDBits |= bits(words[1], 3, 6) << 17;
			instrs[1].ADDBits = bits(words[3], 0, 17) | (bits(words[0], 0, 3) << 17);
			instrs[1].FMABits |= bits(words[2], 19, 32) << 10;
			DumpInstr(instrs[1]);
		} else {
			instrs[0].ADDBits |= bits(words[0], 0, 3) << 17;
		}

		switch (tag) {
			case 0x4:
			case 0xC:
				instrs[1].FMABits |= bits(words[3], 22, 32);
				instrs[1].srcBits = bits(words[2], 19, 32) | (bits(words[3], 0, 22) << (32 - 19));
				break;
			case 0x0:
			case 0x8:
				instrs[1].ADDBits = bits(words[3], 0, 17) | bits(words[3], 29, 32) << 17;
				instrs[1].FMABits |= bits(words[2], 19, 32) << 10;
				DumpInstr(instrs[1]);
				break;
			case 0xe:
				consts[num_consts + 1] = bits(words[2], 4, 32) << 4 | (uint64_t) words[3] << 32;
				break;
			default:
				break;
		}

		switch (tag) {
			case 0xe:
				consts[num_consts] = (bits(words[0], 8, 32) << 4) | (uint64_t) words[1] << 28 | bits(words[2], 0, 4) << 60;
				num_consts += 2;
				break;
			case 0x8:
				//consts[num_consts++] = (bits(words[0], 8, 32) << 4) | (uint64_t) words[1] << 28 | bits(words[2], 0, 4) << 60;
				//break;
			default:
				// 20 bits
				instrs[0].ADDBits |= bits(words[2], 2, 32 - 13);
				// 23 bits
				instrs[0].FMABits = bits(words[1], 11, 32) | bits(words[2], 0, 2) << (32 - 11);
				// 35 bits
				instrs[0].srcBits = ((uint64_t) bits(words[1], 0, 11)) << 24 | (uint64_t) bits(words[0], 8, 32);
				DumpInstr(instrs[0]);
				break;
		}
	}

	for (int i = 0; i < num_consts; i++) {
		printf("const%d: %08x\n", 2 * i, consts[i] & 0xffffffff);
		printf("const%d: %08x\n", 2 * i + 1, consts[i] >> 32);
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

