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

static void DumpSrc(unsigned src, Srcs srcs, bool isPart0)
{
	switch (src) {
		case 0: printf("R%d", srcs.reg0); break;
		case 1: printf("R%d", srcs.reg1); break;
		// TODO reg2 or reg3? FMA sets them the same...
		case 2: printf("R%d", srcs.reg2); break;
		case 3:
				if (isPart0)
					printf("0");
				else
					printf("T"); // i.e. the output of part0 this cycle
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

struct Part0 {
	uint64_t src0 : 3;
	uint64_t src1 : 3;
	uint64_t src2 : 3;
	uint64_t op : 14;
};

enum Part0SrcType {
	Part0OneSrc,
	Part0TwoSrc,
	Part0TwoSrcFmod,
	Part0ThreeSrc,
	Part0ThreeSrcFmod,
	Part0FourSrc,
};

struct Part0OpInfo {
	unsigned op;
	char name[20];
	Part0SrcType srcType;
};

static const Part0OpInfo part0OpInfos[] = {
	{ 0x0000, "FMA",  Part0ThreeSrcFmod },
	{ 0x1000, "FMAX", Part0TwoSrcFmod },
	{ 0x1100, "FMIN", Part0TwoSrcFmod },
	{ 0x13fe, "ADD", Part0TwoSrc },
	{ 0x13ff, "SUB", Part0TwoSrc },
	{ 0x1600, "FADD", Part0TwoSrcFmod },
	{ 0x1700, "CSEL.FEQ", Part0FourSrc },
	{ 0x1708, "CSEL.FGT", Part0FourSrc },
	{ 0x1710, "CSEL.FGE", Part0FourSrc },
	{ 0x1718, "CSEL.IEQ", Part0FourSrc },
	{ 0x1720, "CSEL.IGT", Part0FourSrc },
	{ 0x1728, "CSEL.IGE", Part0FourSrc },
	{ 0x1730, "CSEL.UGT", Part0FourSrc },
	{ 0x1738, "CSEL.UGE", Part0FourSrc },
	{ 0x1808, "RSHIFT_NAND", Part0ThreeSrc },
	{ 0x1838, "RSHIFT_OR", Part0ThreeSrc },
	{ 0x1848, "RSHIFT_AND", Part0ThreeSrc },
	{ 0x1878, "RSHIFT_NOR", Part0ThreeSrc }, // ~((src0 << src2) | src1)
	{ 0x1888, "LSHIFT_NAND", Part0ThreeSrc },
	{ 0x18b8, "LSHIFT_OR",  Part0ThreeSrc }, // (src0 << src2) | src1
	{ 0x18c8, "LSHIFT_AND", Part0ThreeSrc }, // (src0 << src2) & src1
	{ 0x18f8, "LSHIFT_NOR", Part0ThreeSrc },
	{ 0x1908, "RSHIFT_XOR", Part0ThreeSrc },
	{ 0x1918, "RSHIFT_XNOR", Part0ThreeSrc }, // ~((src0 >> src2) ^ src1)
	{ 0x1928, "LSHIFT_XOR", Part0ThreeSrc },
	{ 0x1938, "LSHIFT_XNOR", Part0ThreeSrc }, // ~((src0 >> src2) ^ src1)
	{ 0x1948, "LSHIFT_ADD", Part0ThreeSrc },
	{ 0x1958, "LSHIFT_SUB", Part0ThreeSrc }, // (src0 << src2) - src1
	{ 0x1968, "LSHIFT_RSUB", Part0ThreeSrc }, // src1 - (src0 << src2)
	{ 0x1978, "RSHIFT_ADD", Part0ThreeSrc },
	{ 0x1988, "RSHIFT_SUB", Part0ThreeSrc },
	{ 0x1998, "RSHIFT_RSUB", Part0ThreeSrc },
	{ 0x19a8, "ARSHIFT_ADD", Part0ThreeSrc },
	{ 0x19b8, "ARSHIFT_SUB", Part0ThreeSrc },
	{ 0x19c8, "ARSHIFT_RSUB", Part0ThreeSrc },
	{ 0x380c, "MOV",  Part0OneSrc },
	{ 0x382e, "IMAX", Part0TwoSrc },
	{ 0x382f, "UMAX", Part0TwoSrc },
	{ 0x3830, "IMIN", Part0TwoSrc },
	{ 0x3831, "UMIN", Part0TwoSrc },
	{ 0x383d, "CSEL", Part0ThreeSrc }, // src2 != 0 ? src1 : src0
	{ 0x39e0, "IMAD", Part0ThreeSrc },
	{ 0x39e3, "POPCNT", Part0OneSrc },
};

#define ARRAY_SIZE(x) (sizeof(x) / sizeof(x[0]))

static Part0OpInfo findPart0OpInfo(unsigned op)
{
	for (int i = 0; i < ARRAY_SIZE(part0OpInfos); i++) {
		unsigned opCmp;
		switch (part0OpInfos[i].srcType) {
			case Part0OneSrc:
			case Part0TwoSrc:
			case Part0ThreeSrc:
				opCmp = op;
				break;
			case Part0TwoSrcFmod:
				opCmp = op & ~0xff;
				break;
			case Part0ThreeSrcFmod:
				opCmp = op & ~0xfff;
				break;
			case Part0FourSrc:
				opCmp = op & ~0x7;
				break;
		}
		if (part0OpInfos[i].op == opCmp)
			return part0OpInfos[i];
	}

	Part0OpInfo info;
	snprintf(info.name, sizeof(info.name), "op%04x", op);
	info.op = op;
	info.srcType = Part0ThreeSrc;
	return info;
}

static void DumpPart0(uint64_t word, Srcs srcs)
{
	printf("# part0: %016" PRIx64 "\n", word);
	Part0 part0;
	memcpy((char *) &part0, (char *) &word, sizeof(part0));
	Part0OpInfo info = findPart0OpInfo(part0.op);

	printf("%s", info.name);
	if (info.srcType == Part0TwoSrcFmod ||
		info.srcType == Part0ThreeSrcFmod) {
		// output modifiers
		DumpOutputMod(bits(part0.op, 6, 8));
	}
	printf(" ");
	printf("T0, ");
	// TODO figure out dest
	switch (info.srcType) {
		case Part0OneSrc:
			DumpSrc(part0.src0, srcs, true);
			break;
		case Part0TwoSrc:
			DumpSrc(part0.src0, srcs, true);
			printf(", ");
			DumpSrc(part0.src1, srcs, true);
			break;
		case Part0TwoSrcFmod:
			if (part0.src2 & 0x2)
				printf("-");
			DumpSrc(part0.src0, srcs, true);
			printf(", ");
			if (part0.src2 & 0x4)
				printf("-");
			DumpSrc(part0.src1, srcs, true);
			break;
		case Part0ThreeSrc:
			DumpSrc(part0.src0, srcs, true);
			printf(", ");
			DumpSrc(part0.src1, srcs, true);
			printf(", ");
			DumpSrc(part0.src2, srcs, true);
			break;
		case Part0ThreeSrcFmod:
			if (part0.op & (1 << 8))
				printf("-");
			DumpSrc(part0.src0, srcs, true);
			printf(", ");
			DumpSrc(part0.src1, srcs, true);
			printf(", ");
			if (part0.op & (1 << 9))
				printf("-");
			DumpSrc(part0.src2, srcs, true);
			break;
		case Part0FourSrc:
			DumpSrc(part0.src0, srcs, true);
			printf(", ");
			DumpSrc(part0.src1, srcs, true);
			printf(", ");
			DumpSrc(part0.src2, srcs, true);
			printf(", ");
			DumpSrc(part0.op & 0x7, srcs, true);
			break;
	}
	printf("\n");
}

struct Part1 {
	uint64_t src0 : 3;
	uint64_t src1 : 3;
	uint64_t op : 14;
};

enum Part1SrcType {
	Part1OneSrc,
	Part1TwoSrc,
	Part1TwoSrcFmod,
};

struct Part1OpInfo {
	unsigned op;
	char name[10];
	Part1SrcType srcType;
};

static const Part1OpInfo part1OpInfos[] = {
	{ 0x0000, "FMAX", Part1TwoSrcFmod },
	{ 0x0400, "FMIN", Part1TwoSrcFmod },
	{ 0x0800, "FADD", Part1TwoSrcFmod },
	{ 0x0f65, "MOV",  Part1OneSrc },
	{ 0x2f18, "ADD",  Part1TwoSrc },
	{ 0x2f58, "SUB",  Part1TwoSrc },
	{ 0x3ba3, "OR",  Part1TwoSrc },
	{ 0x3bac, "LSHIFT", Part1TwoSrc },
	{ 0x3ba4, "AND",  Part1TwoSrc },
	{ 0x3baa, "XOR",  Part1TwoSrc },
};

static Part1OpInfo findPart1OpInfo(unsigned op)
{
	for (int i = 0; i < ARRAY_SIZE(part1OpInfos); i++) {
		unsigned opCmp;
		switch (part1OpInfos[i].srcType) {
			case Part1OneSrc:
			case Part1TwoSrc:
				opCmp = op;
				break;
			case Part1TwoSrcFmod:
				opCmp = op & ~0x7f;
				break;
		}
		if (part1OpInfos[i].op == opCmp)
			return part1OpInfos[i];
	}

	Part1OpInfo info;
	snprintf(info.name, sizeof(info.name), "op%04x", op);
	info.op = op;
	info.srcType = Part1TwoSrc;
	return info;
}

static void DumpPart1(uint64_t word, Srcs srcs)
{
	printf("# part1: %016" PRIx64 "\n", word);
	Part1 part1;
	memcpy((char *) &part1, (char *) &word, sizeof(part1));
	Part1OpInfo info = findPart1OpInfo(part1.op);

	printf("%s", info.name);
	if (info.srcType == Part1TwoSrcFmod) {
		// output modifiers
		DumpOutputMod(bits(part1.op, 5, 7));
	}
	printf(" ");
	printf("T1, ");
	switch (info.srcType) {
		case Part1OneSrc:
			DumpSrc(part1.src0, srcs, false);
			break;
		case Part1TwoSrc:
			DumpSrc(part1.src0, srcs, false);
			printf(", ");
			DumpSrc(part1.src1, srcs, false);
			break;
		case Part1TwoSrcFmod:
			if (part1.op & 0x2)
				printf("-");
			DumpSrc(part1.src0, srcs, false);
			printf(", ");
			if (part1.op & 0x4)
				printf("-");
			DumpSrc(part1.src1, srcs, false);
			break;
	}
	printf("\n");
}

// each of these structs represents an instruction that's dispatched in one
// cycle. Note that these instructions are packed in funny ways within the
// clause, hence the need for a separate struct.
struct AluInstr {
	uint64_t srcBits;
	uint64_t part0Bits;
	uint64_t part1Bits;
};

void DumpInstr(AluInstr &instr)
{
	printf("# srcs: %016" PRIx32 "\n", instr.srcBits);
	Srcs srcs;
	memcpy((char *) &srcs, (char *) &instr.srcBits, sizeof(srcs));
	DumpSrcs(srcs);
	instr.srcBits = 0;
	DumpPart0(instr.part0Bits, srcs);
	instr.part0Bits = 0;
	DumpPart1(instr.part1Bits, srcs);
	instr.part1Bits = 0;
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
			instrs[0].part1Bits |= bits(words[1], 3, 6) << 17;
			instrs[1].part1Bits = bits(words[3], 0, 17) | (bits(words[0], 0, 3) << 17);
			instrs[1].part0Bits |= bits(words[2], 19, 32) << 10;
			DumpInstr(instrs[1]);
		} else {
			instrs[0].part1Bits |= bits(words[0], 0, 3) << 17;
		}

		switch (tag) {
			case 0x4:
			case 0xC:
				instrs[1].part0Bits |= bits(words[3], 22, 32);
				instrs[1].srcBits = bits(words[2], 19, 32) | (bits(words[3], 0, 22) << (32 - 19));
				break;
			case 0x0:
			case 0x8:
				instrs[1].part1Bits = bits(words[3], 0, 17) | bits(words[3], 29, 32) << 17;
				instrs[1].part0Bits |= bits(words[2], 19, 32) << 10;
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
				instrs[0].part1Bits |= bits(words[2], 2, 32 - 13);
				// 23 bits
				instrs[0].part0Bits = bits(words[1], 11, 32) | bits(words[2], 0, 2) << (32 - 11);
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

