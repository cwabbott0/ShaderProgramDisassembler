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
	uint64_t src2 : 3;
	uint64_t op : 14;
};

enum FMASrcType {
	FMAOneSrc,
	FMATwoSrc,
	FMATwoSrcFmod,
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
	{ 0x1000, "FMAX", FMATwoSrcFmod },
	{ 0x1100, "FMIN", FMATwoSrcFmod },
	{ 0x13fe, "ADD", FMATwoSrc },
	{ 0x13ff, "SUB", FMATwoSrc },
	{ 0x1600, "FADD", FMATwoSrcFmod },
	{ 0x1700, "CSEL.FEQ", FMAFourSrc },
	{ 0x1708, "CSEL.FGT", FMAFourSrc },
	{ 0x1710, "CSEL.FGE", FMAFourSrc },
	{ 0x1718, "CSEL.IEQ", FMAFourSrc },
	{ 0x1720, "CSEL.IGT", FMAFourSrc },
	{ 0x1728, "CSEL.IGE", FMAFourSrc },
	{ 0x1730, "CSEL.UGT", FMAFourSrc },
	{ 0x1738, "CSEL.UGE", FMAFourSrc },
	{ 0x1808, "RSHIFT_NAND", FMAThreeSrc },
	{ 0x1838, "RSHIFT_OR", FMAThreeSrc },
	{ 0x1848, "RSHIFT_AND", FMAThreeSrc },
	{ 0x1878, "RSHIFT_NOR", FMAThreeSrc }, // ~((src0 << src2) | src1)
	{ 0x1888, "LSHIFT_NAND", FMAThreeSrc },
	{ 0x18b8, "LSHIFT_OR",  FMAThreeSrc }, // (src0 << src2) | src1
	{ 0x18c8, "LSHIFT_AND", FMAThreeSrc }, // (src0 << src2) & src1
	{ 0x18f8, "LSHIFT_NOR", FMAThreeSrc },
	{ 0x1908, "RSHIFT_XOR", FMAThreeSrc },
	{ 0x1918, "RSHIFT_XNOR", FMAThreeSrc }, // ~((src0 >> src2) ^ src1)
	{ 0x1928, "LSHIFT_XOR", FMAThreeSrc },
	{ 0x1938, "LSHIFT_XNOR", FMAThreeSrc }, // ~((src0 >> src2) ^ src1)
	{ 0x1948, "LSHIFT_ADD", FMAThreeSrc },
	{ 0x1958, "LSHIFT_SUB", FMAThreeSrc }, // (src0 << src2) - src1
	{ 0x1968, "LSHIFT_RSUB", FMAThreeSrc }, // src1 - (src0 << src2)
	{ 0x1978, "RSHIFT_ADD", FMAThreeSrc },
	{ 0x1988, "RSHIFT_SUB", FMAThreeSrc },
	{ 0x1998, "RSHIFT_RSUB", FMAThreeSrc },
	{ 0x19a8, "ARSHIFT_ADD", FMAThreeSrc },
	{ 0x19b8, "ARSHIFT_SUB", FMAThreeSrc },
	{ 0x19c8, "ARSHIFT_RSUB", FMAThreeSrc },
	{ 0x380c, "MOV",  FMAOneSrc },
	{ 0x382e, "IMAX", FMATwoSrc },
	{ 0x382f, "UMAX", FMATwoSrc },
	{ 0x3830, "IMIN", FMATwoSrc },
	{ 0x3831, "UMIN", FMATwoSrc },
	{ 0x383d, "CSEL", FMAThreeSrc }, // src2 != 0 ? src1 : src0
	{ 0x39e0, "IMAD", FMAThreeSrc },
	{ 0x39e3, "POPCNT", FMAOneSrc },
};

#define ARRAY_SIZE(x) (sizeof(x) / sizeof(x[0]))

static FMAOpInfo findFMAOpInfo(unsigned op)
{
	for (int i = 0; i < ARRAY_SIZE(FMAOpInfos); i++) {
		unsigned opCmp;
		switch (FMAOpInfos[i].srcType) {
			case FMAOneSrc:
			case FMATwoSrc:
			case FMAThreeSrc:
				opCmp = op;
				break;
			case FMATwoSrcFmod:
				opCmp = op & ~0xff;
				break;
			case FMAThreeSrcFmod:
				opCmp = op & ~0xfff;
				break;
			case FMAFourSrc:
				opCmp = op & ~0x7;
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
		DumpOutputMod(bits(FMA.op, 6, 8));
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
			if (FMA.src2 & 0x2)
				printf("-");
			DumpSrc(FMA.src0, srcs, true);
			printf(", ");
			if (FMA.src2 & 0x4)
				printf("-");
			DumpSrc(FMA.src1, srcs, true);
			break;
		case FMAThreeSrc:
			DumpSrc(FMA.src0, srcs, true);
			printf(", ");
			DumpSrc(FMA.src1, srcs, true);
			printf(", ");
			DumpSrc(FMA.src2, srcs, true);
			break;
		case FMAThreeSrcFmod:
			if (FMA.op & (1 << 8))
				printf("-");
			DumpSrc(FMA.src0, srcs, true);
			printf(", ");
			DumpSrc(FMA.src1, srcs, true);
			printf(", ");
			if (FMA.op & (1 << 9))
				printf("-");
			DumpSrc(FMA.src2, srcs, true);
			break;
		case FMAFourSrc:
			DumpSrc(FMA.src0, srcs, true);
			printf(", ");
			DumpSrc(FMA.src1, srcs, true);
			printf(", ");
			DumpSrc(FMA.src2, srcs, true);
			printf(", ");
			DumpSrc(FMA.op & 0x7, srcs, true);
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
};

struct ADDOpInfo {
	unsigned op;
	char name[10];
	ADDSrcType srcType;
};

static const ADDOpInfo ADDOpInfos[] = {
	{ 0x0000, "FMAX", ADDTwoSrcFmod },
	{ 0x0400, "FMIN", ADDTwoSrcFmod },
	{ 0x0800, "FADD", ADDTwoSrcFmod },
	{ 0x0f65, "MOV",  ADDOneSrc },
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

