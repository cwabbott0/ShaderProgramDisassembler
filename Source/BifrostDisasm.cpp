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
			break;
		case 15:
			decoded.FMAWriteUnit = RegWrite3;
			decoded.ADDWriteUnit = RegWrite2;
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
	RegCtrl ctrl = DecodeRegCtrl(srcs);
	printf("# ");
	if (ctrl.readReg0) {
		unsigned reg0 = srcs.reg0;
		if (srcs.ctrl == 0)
			reg0 |= (srcs.reg1 & 1) << 5;
		printf("port 0: R%d ", reg0);
	}

	if (ctrl.readReg1)
		printf("port 1: R%d ", srcs.reg1);

	if (ctrl.FMAWriteUnit == RegWrite2)
		printf("port 2: R%d (write FMA) ", srcs.reg2);
	else if (ctrl.ADDWriteUnit == RegWrite2)
		printf("port 2: R%d (write ADD) ", srcs.reg2);

	if (ctrl.FMAWriteUnit == RegWrite3)
		printf("port 3: R%d (write FMA) ", srcs.reg3);
	else if (ctrl.ADDWriteUnit == RegWrite3)
		printf("port 3: R%d (write ADD) ", srcs.reg3);
	else if (ctrl.readReg3)
		printf("port 3: R%d (read) ", srcs.reg3);

	if (srcs.uniformConst) {
		if (srcs.uniformConst & 0x80) {
			printf("uniform: U%d", (srcs.uniformConst & 0x7f) * 2);
		}
	}

	printf("\n");
}

static void DumpConstImm(uint32_t imm)
{
	union {
		float f;
		uint32_t i;
	} fi;
	fi.i = imm;
	printf("0x%08x /* %f */", imm, fi.f);
}

static uint64_t GetConst(uint64_t *consts, Regs srcs)
{
	unsigned low_bits = srcs.uniformConst & 0xf;
	uint64_t imm;
	switch (srcs.uniformConst >> 4) {
		case 4: imm = consts[0]; break;
		case 5: imm = consts[1]; break;
		case 6: imm = consts[2]; break;
		case 7: imm = consts[3]; break;
		case 2: imm = consts[4]; break;
		case 3: imm = consts[5]; break;
		default: assert(0); break;
	}
	return imm | low_bits;
}

static void DumpSrc(unsigned src, Regs srcs, uint64_t *consts, bool isFMA)
{
	switch (src) {
		case 0: {
			unsigned reg = srcs.reg0;
			if (srcs.ctrl == 0)
				reg |= (srcs.reg1 & 1) << 5;
			printf("R%d", reg);
			break;
		}
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
					case 3: imm = ((uint32_t) consts[5]) | low_bits; break;
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
					case 3: imm = (uint32_t)(consts[5] >> 32); break;
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
			printf(".clamp_0_inf"); break; // max(out, 0)
		case 2:
			printf(".clamp_m1_1"); break; // clamp(out, -1, 1)
		case 3:
			printf(".clamp_0_1"); break; // clamp(out, 0, 1)
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
	FMATwoSrcFmod16,
	FMAFcmp,
	FMAFcmp16,
	FMAThreeSrc,
	FMAThreeSrcFmod,
	FMAThreeSrcFmod16,
	FMAFourSrc,
};

struct FMAOpInfo {
	unsigned op;
	char name[20];
	FMASrcType srcType;
};

static const FMAOpInfo FMAOpInfos[] = {
	{ 0x00000, "FMA.f32",  FMAThreeSrcFmod },
	{ 0x40000, "MAX.f32", FMATwoSrcFmod },
	{ 0x44000, "MIN.f32", FMATwoSrcFmod },
	{ 0x48000, "FCMP.GL", FMAFcmp },
	{ 0x4c000, "FCMP.D3D", FMAFcmp },
	{ 0x4ff98, "ADD.i32", FMATwoSrc },
	{ 0x4ffd8, "SUB.i32", FMATwoSrc },
	{ 0x4fff0, "SUBB.i32", FMATwoSrc },
	// compute FMA of first three sources, then set exponent to the fourth
	// source (as an integer).
	{ 0x50000, "FMA_RSCALE", FMAFourSrc },
	// Seems to compute src2 - src0 * src1... why don't they just use FMA?
	{ 0x528c0, "FRCP_PT3", FMAThreeSrc },
	// compute FMA of first three sources, then add the fourth argument to the
	// scale (modify scale)
	{ 0x54000, "FMA_MSCALE", FMAFourSrc },
	{ 0x58000, "ADD.f32", FMATwoSrcFmod },
	{ 0x5c000, "CSEL.FEQ.f32", FMAFourSrc },
	{ 0x5c200, "CSEL.FGT.f32", FMAFourSrc },
	{ 0x5c400, "CSEL.FGE.f32", FMAFourSrc },
	{ 0x5c600, "CSEL.IEQ.f32", FMAFourSrc },
	{ 0x5c800, "CSEL.IGT.i32", FMAFourSrc },
	{ 0x5ca00, "CSEL.IGE.i32", FMAFourSrc },
	{ 0x5cc00, "CSEL.UGT.i32", FMAFourSrc },
	{ 0x5ce00, "CSEL.UGE.i32", FMAFourSrc },
	{ 0x5d8d0, "ICMP.D3D.GT.v2i16", FMATwoSrc },
	{ 0x5d9d0, "UCMP.D3D.GT.v2i16", FMATwoSrc },
	{ 0x5dad0, "ICMP.D3D.GE.v2i16", FMATwoSrc },
	{ 0x5dbd0, "UCMP.D3D.GE.v2i16", FMATwoSrc },
	{ 0x5dcd0, "ICMP.D3D.EQ.v2i16", FMATwoSrc },
	{ 0x5de40, "ICMP.GL.GT.i32", FMATwoSrc }, // src0 > src1 ? 1 : 0
	{ 0x5de48, "ICMP.GL.GE.i32", FMATwoSrc },
	{ 0x5de50, "UCMP.GL.GT.i32", FMATwoSrc },
	{ 0x5de58, "UCMP.GL.GE.i32", FMATwoSrc },
	{ 0x5de60, "ICMP.GL.EQ.i32", FMATwoSrc },
	{ 0x5dec0, "ICMP.D3D.GT.i32", FMATwoSrc }, // src0 > src1 ? ~0 : 0
	{ 0x5dec8, "ICMP.D3D.GE.i32", FMATwoSrc },
	{ 0x5ded0, "UCMP.D3D.GT.i32", FMATwoSrc },
	{ 0x5ded8, "UCMP.D3D.GE.i32", FMATwoSrc },
	{ 0x5dee0, "ICMP.D3D.EQ.i32", FMATwoSrc },
	{ 0x60200, "RSHIFT_NAND.i32", FMAThreeSrc },
	{ 0x603c0, "RSHIFT_NAND.v2i16", FMAThreeSrc },
	{ 0x60e00, "RSHIFT_OR.i32", FMAThreeSrc },
	{ 0x60fc0, "RSHIFT_OR.v2i16", FMAThreeSrc },
	{ 0x61200, "RSHIFT_AND.i32", FMAThreeSrc },
	{ 0x613c0, "RSHIFT_AND.v2i16", FMAThreeSrc },
	{ 0x61e00, "RSHIFT_NOR.i32", FMAThreeSrc }, // ~((src0 << src2) | src1)
	{ 0x61fc0, "RSHIFT_NOR.v2i16", FMAThreeSrc }, // ~((src0 << src2) | src1)
	{ 0x62200, "LSHIFT_NAND.i32", FMAThreeSrc },
	{ 0x623c0, "LSHIFT_NAND.v2i16", FMAThreeSrc },
	{ 0x62e00, "LSHIFT_OR.i32",  FMAThreeSrc }, // (src0 << src2) | src1
	{ 0x62fc0, "LSHIFT_OR.v2i16",  FMAThreeSrc }, // (src0 << src2) | src1
	{ 0x63200, "LSHIFT_AND.i32", FMAThreeSrc }, // (src0 << src2) & src1
	{ 0x633c0, "LSHIFT_AND.v2i16", FMAThreeSrc },
	{ 0x63e00, "LSHIFT_NOR.i32", FMAThreeSrc },
	{ 0x63fc0, "LSHIFT_NOR.v2i16", FMAThreeSrc },
	{ 0x64200, "RSHIFT_XOR.i32", FMAThreeSrc },
	{ 0x643c0, "RSHIFT_XOR.v2i16", FMAThreeSrc },
	{ 0x64600, "RSHIFT_XNOR.i32", FMAThreeSrc }, // ~((src0 >> src2) ^ src1)
	{ 0x647c0, "RSHIFT_XNOR.v2i16", FMAThreeSrc }, // ~((src0 >> src2) ^ src1)
	{ 0x64a00, "LSHIFT_XOR.i32", FMAThreeSrc },
	{ 0x64bc0, "LSHIFT_XOR.v2i16", FMAThreeSrc },
	{ 0x64e00, "LSHIFT_XNOR.i32", FMAThreeSrc }, // ~((src0 >> src2) ^ src1)
	{ 0x64fc0, "LSHIFT_XNOR.v2i16", FMAThreeSrc }, // ~((src0 >> src2) ^ src1)
	{ 0x65200, "LSHIFT_ADD.i32", FMAThreeSrc },
	{ 0x65600, "LSHIFT_SUB.i32", FMAThreeSrc }, // (src0 << src2) - src1
	{ 0x65a00, "LSHIFT_RSUB.i32", FMAThreeSrc }, // src1 - (src0 << src2)
	{ 0x65e00, "RSHIFT_ADD.i32", FMAThreeSrc },
	{ 0x66200, "RSHIFT_SUB.i32", FMAThreeSrc },
	{ 0x66600, "RSHIFT_RSUB.i32", FMAThreeSrc },
	{ 0x66a00, "ARSHIFT_ADD.i32", FMAThreeSrc },
	{ 0x66e00, "ARSHIFT_SUB.i32", FMAThreeSrc },
	{ 0x67200, "ARSHIFT_RSUB.i32", FMAThreeSrc },
	{ 0x80000, "FMA.v2f16",  FMAThreeSrcFmod16 },
	{ 0xc0000, "MAX.v2f16", FMATwoSrcFmod16 },
	{ 0xc4000, "MIN.v2f16", FMATwoSrcFmod16 },
	{ 0xcc000, "FCMP.D3D", FMAFcmp16 },
	{ 0xcf900, "ADD.v2i16", FMATwoSrc },
	{ 0xcfc10, "ADDC.i32", FMATwoSrc },
	{ 0xd8000, "ADD.v2f16", FMATwoSrcFmod16 },
	{ 0xdc000, "CSEL.FEQ.v2f16", FMAFourSrc },
	{ 0xdc200, "CSEL.FGT.v2f16", FMAFourSrc },
	{ 0xdc400, "CSEL.FGE.v2f16", FMAFourSrc },
	{ 0xdc600, "CSEL.IEQ.v2f16", FMAFourSrc },
	{ 0xdc800, "CSEL.IGT.v2i16", FMAFourSrc },
	{ 0xdca00, "CSEL.IGE.v2i16", FMAFourSrc },
	{ 0xdcc00, "CSEL.UGT.v2i16", FMAFourSrc },
	{ 0xdce00, "CSEL.UGE.v2i16", FMAFourSrc },
	{ 0xdd000, "F32_TO_F16", FMATwoSrc },
	{ 0xe0056, "F16_TO_I16", FMAOneSrc },
	{ 0xe0057, "F16_TO_U16", FMAOneSrc },
	{ 0xe00d0, "I16_TO_F16", FMAOneSrc },
	{ 0xe00d1, "U16_TO_F16", FMAOneSrc },
	{ 0xe0136, "F32_TO_I32", FMAOneSrc },
	{ 0xe0137, "F32_TO_U32", FMAOneSrc },
	{ 0xe0178, "I32_TO_F32", FMAOneSrc },
	{ 0xe0179, "U32_TO_F32", FMAOneSrc },
	{ 0xe0198, "I16_TO_I32.X", FMAOneSrc },
	{ 0xe0199, "U16_TO_U32.X", FMAOneSrc },
	{ 0xe019a, "I16_TO_I32.Y", FMAOneSrc },
	{ 0xe019b, "U16_TO_U32.Y", FMAOneSrc },
	{ 0xe019c, "I16_TO_F32.X", FMAOneSrc },
	{ 0xe019d, "U16_TO_F32.X", FMAOneSrc },
	{ 0xe019e, "I16_TO_F32.Y", FMAOneSrc },
	{ 0xe019f, "U16_TO_F32.Y", FMAOneSrc },
	{ 0xe01a2, "F16_TO_F32.X", FMAOneSrc },
	{ 0xe01a3, "F16_TO_F32.Y", FMAOneSrc },
	{ 0xe032c, "NOP",  FMAOneSrc },
	{ 0xe032d, "MOV",  FMAOneSrc },
	// From the ARM patent US20160364209A1:
	// "Decompose v (the input) into numbers x1 and s such that v = x1 * 2^s,
	// and x1 is a floating point value in a predetermined range where the
	// value 1 is within the range and not at one extremity of the range (e.g.
	// choose a range where 1 is towards middle of range)."
	// 
	// This computes x1.
	{ 0xe0345, "LOG_FREXPM", FMAOneSrc },
	{ 0xe0365, "FRCP_TABLE", FMAOneSrc },
	// Compute required exponent for reciprocal (negate it, accounting for the offset.)
	{ 0xe038d, "FRCP_EXP", FMAOneSrc },
	{ 0xe03c5, "LOG_FREXPE", FMAOneSrc },
	{ 0xe0b80, "IMAX3", FMAThreeSrc },
	{ 0xe0bc0, "UMAX3", FMAThreeSrc },
	{ 0xe0c00, "IMIN3", FMAThreeSrc },
	{ 0xe0c40, "UMIN3", FMAThreeSrc },
	{ 0xe0f40, "CSEL", FMAThreeSrc }, // src2 != 0 ? src1 : src0
	{ 0xe1845, "CEIL", FMAOneSrc },
	{ 0xe1885, "FLOOR", FMAOneSrc },
	// This acts like a normal 32-bit add, except that it sets a flag on
	// overflow that gets listened to by load/store instructions in the ADD
	// part of the instruction, and added appropriately to the upper 32 bits of
	// the address. It lets you efficiently add a 32-bit offset to a 64-bit
	// pointer when loading/storing.
	{ 0xe1c80, "ADD_ADDR", FMATwoSrc },
	// Similar to the above, but used for normal additions (paired with
	// ADD_HIGH32 in the ADD slot to do 64-bit addition).
	{ 0xe1cc0, "ADD_LOW32", FMATwoSrc },
	{ 0xe1e00, "SEL.XX.i16", FMATwoSrc },
	{ 0xe1e10, "SEL.XY.i16", FMATwoSrc },
	{ 0xe1e08, "SEL.YX.i16", FMATwoSrc },
	{ 0xe1e18, "SEL.YY.i16", FMATwoSrc },
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
			case FMATwoSrcFmod16:
				opCmp = op & ~0x3fff;
				break;
			case FMAThreeSrcFmod:
			case FMAThreeSrcFmod16:
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

static void Dump16Swizzle(unsigned swiz)
{
	if (swiz == 2)
		return;
	printf(".%c%c", "xy"[swiz & 1], "xy"[(swiz >> 1) & 1]);
}

static void DumpFMAExpandSrc0(unsigned ctrl)
{
		switch (ctrl) {
			case 3:
			case 4:
			case 6:
				printf(".x");
				break;
			case 5:
			case 7:
				printf(".y");
				break;
			case 0:
			case 1:
			case 2:
				break;
			default:
				printf(".unk");
				break;
		}
}

static void DumpFMAExpandSrc1(unsigned ctrl)
{
		switch (ctrl) {
			case 1:
			case 3:
				printf(".x");
				break;
			case 2:
			case 4:
			case 5:
				printf(".y");
				break;
			case 0:
			case 6:
			case 7:
				break;
			default:
				printf(".unk");
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
		info.srcType == FMAThreeSrcFmod ||
		info.srcType == FMATwoSrcFmod16 ||
		info.srcType == FMAThreeSrcFmod16) {
		// output modifiers
		DumpOutputMod(bits(FMA.op, 12, 14));
	} else if (info.srcType == FMAFcmp || info.srcType == FMAFcmp16) {
		DumpFCMP(bits(FMA.op, 10, 13));
		if (info.srcType == FMAFcmp)
			printf(".f32");
		else
			printf(".v2f16");
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
			if (FMA.op & 0x200)
				printf("abs(");
			DumpSrc(FMA.src0, regs, consts, true);
			DumpFMAExpandSrc0((FMA.op >> 6) & 0x7);
			if (FMA.op & 0x200)
				printf(")");
			printf(", ");
			if (FMA.op & 0x20)
				printf("-");
			if (FMA.op & 0x8)
				printf("abs(");
			DumpSrc(FMA.op & 0x7, regs, consts, true);
			DumpFMAExpandSrc1((FMA.op >> 6) & 0x7);
			if (FMA.op & 0x8)
				printf(")");
			break;
		case FMATwoSrcFmod16: {
			bool abs1 = FMA.op & 0x8;
			bool abs2 = (FMA.op & 0x7) < FMA.src0;
			if (FMA.op & 0x10)
				printf("-");
			if (abs1 || abs2)
				printf("abs(");
			DumpSrc(FMA.src0, regs, consts, true);
			Dump16Swizzle((FMA.op >> 6) & 0x3);
			if (abs1 || abs2)
				printf(")");
			printf(", ");
			if (FMA.op & 0x20)
				printf("-");
			if (abs1 && abs2)
				printf("abs(");
			DumpSrc(FMA.op & 0x7, regs, consts, true);
			Dump16Swizzle((FMA.op >> 8) & 0x3);
			if (abs1 && abs2)
				printf(")");
			break;
		}
		case FMAFcmp:
			if (FMA.op & 0x200)
				printf("abs(");
			DumpSrc(FMA.src0, regs, consts, true);
			DumpFMAExpandSrc0((FMA.op >> 6) & 0x7);
			if (FMA.op & 0x200)
				printf(")");
			printf(", ");
			if (FMA.op & 0x20)
				printf("-");
			if (FMA.op & 0x8)
				printf("abs(");
			DumpSrc(FMA.op & 0x7, regs, consts, true);
			DumpFMAExpandSrc1((FMA.op >> 6) & 0x7);
			if (FMA.op & 0x8)
				printf(")");
			break;
		case FMAFcmp16:
			DumpSrc(FMA.src0, regs, consts, true);
			// Note: this is kinda a guess, I haven't seen the blob set this to
			// anything other than the identity, but it matches FMATwoSrcFmod16
			Dump16Swizzle((FMA.op >> 6) & 0x3);
			printf(", ");
			DumpSrc(FMA.op & 0x7, regs, consts, true);
			Dump16Swizzle((FMA.op >> 8) & 0x3);
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
			if (FMA.op & (1 << 9))
				printf("abs(");
			DumpSrc(FMA.src0, regs, consts, true);
			DumpFMAExpandSrc0((FMA.op >> 6) & 0x7);
			if (FMA.op & (1 << 9))
				printf(")");
			printf(", ");
			if (FMA.op & (1 << 16))
				printf("abs(");
			DumpSrc(FMA.op & 0x7, regs, consts, true);
			DumpFMAExpandSrc1((FMA.op >> 6) & 0x7);
			if (FMA.op & (1 << 16))
				printf(")");
			printf(", ");
			if (FMA.op & (1 << 15))
				printf("-");
			if (FMA.op & (1 << 17))
				printf("abs(");
			DumpSrc((FMA.op >> 3) & 0x7, regs, consts, true);
			if (FMA.op & (1 << 17))
				printf(")");
			break;
		case FMAThreeSrcFmod16:
			if (FMA.op & (1 << 14))
				printf("-");
			DumpSrc(FMA.src0, regs, consts, true);
			Dump16Swizzle((FMA.op >> 6) & 0x3);
			printf(", ");
			DumpSrc(FMA.op & 0x7, regs, consts, true);
			Dump16Swizzle((FMA.op >> 8) & 0x3);
			printf(", ");
			if (FMA.op & (1 << 15))
				printf("-");
			DumpSrc((FMA.op >> 3) & 0x7, regs, consts, true);
			Dump16Swizzle((FMA.op >> 16) & 0x3);
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
	ADDTwoSrcFmod16,
	ADDTwoSrcFmod16Commutative,
	ADDThreeSrc,
	ADDFcmp,
	ADDTexCompact, // texture instruction with embedded sampler
	ADDTex, // texture instruction with sampler/etc. in uniform port
	ADDVaryingInterp,
	ADDBlending,
};

struct ADDOpInfo {
	unsigned op;
	char name[20];
	ADDSrcType srcType;
	bool hasDataReg;
};

static const ADDOpInfo ADDOpInfos[] = {
	{ 0x00000, "MAX.f32", ADDTwoSrcFmod },
	{ 0x02000, "MIN.f32", ADDTwoSrcFmod },
	{ 0x04000, "ADD.f32", ADDTwoSrcFmod },
	{ 0x06000, "FCMP.GL", ADDFcmp },
	{ 0x07000, "FCMP.D3D", ADDFcmp },
	{ 0x07856, "F16_TO_I16", ADDOneSrc },
	{ 0x07857, "F16_TO_U16", ADDOneSrc },
	{ 0x078d0, "I16_TO_F16", ADDOneSrc },
	{ 0x078d1, "U16_TO_F16", ADDOneSrc },
	{ 0x07936, "F32_TO_I32", ADDOneSrc },
	{ 0x07937, "F32_TO_U32", ADDOneSrc },
	{ 0x07978, "I32_TO_F32", ADDOneSrc },
	{ 0x07979, "U32_TO_F32", ADDOneSrc },
	{ 0x07998, "I16_TO_I32.X", ADDOneSrc },
	{ 0x07999, "U16_TO_U32.X", ADDOneSrc },
	{ 0x0799a, "I16_TO_I32.Y", ADDOneSrc },
	{ 0x0799b, "U16_TO_U32.Y", ADDOneSrc },
	{ 0x0799c, "I16_TO_F32.X", ADDOneSrc },
	{ 0x0799d, "U16_TO_F32.X", ADDOneSrc },
	{ 0x0799e, "I16_TO_F32.Y", ADDOneSrc },
	{ 0x0799f, "U16_TO_F32.Y", ADDOneSrc },
	// take the low 16 bits, and expand it to a 32-bit float
	{ 0x079a2, "F16_TO_F32.X", ADDOneSrc },
	// take the high 16 bits, ...
	{ 0x079a3, "F16_TO_F32.Y", ADDOneSrc },
	{ 0x07b2c, "NOP",  ADDOneSrc },
	{ 0x07b2d, "MOV",  ADDOneSrc },
	{ 0x07b8d, "FRCP_EXP", ADDOneSrc },
	// From the ARM patent US20160364209A1:
	// "Decompose v (the input) into numbers x1 and s such that v = x1 * 2^s,
	// and x1 is a floating point value in a predetermined range where the
	// value 1 is within the range and not at one extremity of the range (e.g.
	// choose a range where 1 is towards middle of range)."
	// 
	// This computes s.
	{ 0x07bc5, "FLOG_FREXPE", ADDOneSrc },
	{ 0x07d45, "CEIL", ADDOneSrc },
	{ 0x07d85, "FLOOR", ADDOneSrc },
	{ 0x07f18, "ADD_HIGH32", ADDTwoSrc },
	{ 0x0a000, "LD_VAR.32", ADDVaryingInterp, true },
	{ 0x0b000, "TEX", ADDTexCompact, true },
	{ 0x0c188, "LOAD.i32", ADDTwoSrc, true },
	{ 0x0c1c8, "LOAD.v2i32", ADDTwoSrc, true },
	{ 0x0c208, "LOAD.v4i32", ADDTwoSrc, true },
	{ 0x0c248, "STORE.v4i32", ADDTwoSrc, true },
	{ 0x0c588, "STORE.i32", ADDTwoSrc, true },
	{ 0x0c5c8, "STORE.v2i32", ADDTwoSrc, true },
	{ 0x0c648, "LOAD.u16", ADDTwoSrc, true }, // zero-extends
	{ 0x0ca88, "LOAD.v3i32", ADDTwoSrc, true },
	{ 0x0cb88, "STORE.v3i32", ADDTwoSrc, true },
	// Produce appropriate scale
	{ 0x0ce00, "FRCP_SCALE", ADDOneSrc },
	// Used in the argument reduction for log.
	// See the ARM patent for more information.
	{ 0x0ce60, "FRCP_APPROX", ADDOneSrc },
	{ 0x0cf50, "SIN_TABLE", ADDOneSrc },
	{ 0x0cf51, "COS_TABLE", ADDOneSrc },
	{ 0x0cf60, "FLOG2_TABLE", ADDOneSrc },
	{ 0x0ea60, "SEL.XX.i16", ADDTwoSrc },
	{ 0x0ea70, "SEL.XY.i16", ADDTwoSrc },
	{ 0x0ea68, "SEL.YX.i16", ADDTwoSrc },
	{ 0x0ea78, "SEL.YY.i16", ADDTwoSrc },
	{ 0x0ec00, "F32_TO_F16", ADDTwoSrc },
	{ 0x0cf64, "FLOGE_TABLE", ADDOneSrc },
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
	{ 0x10000, "MAX.v2f16", ADDTwoSrcFmod16Commutative },
	{ 0x12000, "MIN.v2f16", ADDTwoSrcFmod16Commutative },
	{ 0x14000, "ADD.v2f16", ADDTwoSrcFmod16 },
	{ 0x178c0, "ADD.i32",  ADDTwoSrc },
	{ 0x17900, "ADD.v2i16", ADDTwoSrc },
	{ 0x17ac0, "SUB.i32",  ADDTwoSrc },
	{ 0x17c10, "ADDC.i32", ADDTwoSrc }, // adds src0 to the bottom bit of src1
	// Implements alpha-to-coverage, as well as possibly the late depth and
	// stencil tests. The first source is the existing sample mask in R60
	// (possibly modified by gl_SampleMask), and the second source is the alpha
	// value.  The sample mask is written right away based on the
	// alpha-to-coverage result using the normal register write mechanism,
	// since that doesn't need to read from any memory, and then written again
	// later based on the result of the stencil and depth tests using the
	// special register.
	{ 0x191e8, "ATEST.f32", ADDTwoSrc, true },
	{ 0x191f0, "ATEST.X.f16", ADDTwoSrc, true },
	{ 0x191f8, "ATEST.Y.f16", ADDTwoSrc, true },
	// This takes the sample coverage mask (computed by ATEST above) as a
	// regular argument, in addition to the vec4 color in the special register.
	{ 0x1952c, "BLEND", ADDBlending, true },
	{ 0x1a000, "LD_VAR.16", ADDVaryingInterp, true },
	{ 0x1ae60, "TEX", ADDTex, true },
	{ 0x1c000, "RSHIFT_NAND.i32", ADDThreeSrc },
	{ 0x1c300, "RSHIFT_OR.i32", ADDThreeSrc },
	{ 0x1c400, "RSHIFT_AND.i32", ADDThreeSrc },
	{ 0x1c700, "RSHIFT_NOR.i32", ADDThreeSrc },
	{ 0x1c800, "LSHIFT_NAND.i32", ADDThreeSrc },
	{ 0x1cb00, "LSHIFT_OR.i32", ADDThreeSrc },
	{ 0x1cc00, "LSHIFT_AND.i32", ADDThreeSrc },
	{ 0x1cf00, "LSHIFT_NOR.i32", ADDThreeSrc },
	{ 0x1d000, "RSHIFT_XOR.i32", ADDThreeSrc },
	{ 0x1d100, "RSHIFT_XNOR.i32", ADDThreeSrc },
	{ 0x1d200, "LSHIFT_XOR.i32", ADDThreeSrc },
	{ 0x1d300, "LSHIFT_XNOR.i32", ADDThreeSrc },
	{ 0x1d400, "LSHIFT_ADD.i32", ADDThreeSrc },
	{ 0x1d500, "LSHIFT_SUB.i32", ADDThreeSrc },
	{ 0x1d500, "LSHIFT_RSUB.i32", ADDThreeSrc },
	{ 0x1d700, "RSHIFT_ADD.i32", ADDThreeSrc },
	{ 0x1d800, "RSHIFT_SUB.i32", ADDThreeSrc },
	{ 0x1d900, "RSHIFT_RSUB.i32", ADDThreeSrc },
	{ 0x1da00, "ARSHIFT_ADD.i32", ADDThreeSrc },
	{ 0x1db00, "ARSHIFT_SUB.i32", ADDThreeSrc },
	{ 0x1dc00, "ARSHIFT_RSUB.i32", ADDThreeSrc },
	{ 0x1dd18, "OR.i32",  ADDTwoSrc },
	{ 0x1dd20, "AND.i32",  ADDTwoSrc },
	{ 0x1dd60, "LSHIFT.i32", ADDTwoSrc },
	{ 0x1dd50, "XOR.i32",  ADDTwoSrc },
	{ 0x1dd80, "RSHIFT.i32", ADDTwoSrc },
	{ 0x1dda0, "ARSHIFT.i32", ADDTwoSrc },
};

static ADDOpInfo findADDOpInfo(unsigned op)
{
	for (int i = 0; i < ARRAY_SIZE(ADDOpInfos); i++) {
		unsigned opCmp;
		switch (ADDOpInfos[i].srcType) {
			case ADDOneSrc:
			case ADDBlending:
				opCmp = op;
				break;
			case ADDTwoSrc:
				opCmp = op & ~0x7;
				break;
			case ADDThreeSrc:
				opCmp = op & ~0x3f;
				break;
			case ADDTex:
				opCmp = op & ~0xf;
				break;
			case ADDTwoSrcFmod:
			case ADDTwoSrcFmod16:
				opCmp = op & ~0x1fff;
				break;
			case ADDTwoSrcFmod16Commutative:
				opCmp = op & ~0xfff;
				break;
			case ADDFcmp:
				opCmp = op & ~0x7ff;
				break;
			case ADDTexCompact:
				opCmp = op & ~0x3ff;
				break;
			case ADDVaryingInterp:
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
	info.hasDataReg = true;
	return info;
}

struct TexCtrl {
	unsigned samplerIndex : 4; // also used to signal indirects
	unsigned texIndex : 7;
	bool noMergeIndex : 1; // whether to merge (direct) sampler & texture indices
	bool filter : 1; // use the usual filtering pipeline (0 for texelFetch & textureGather)
	unsigned unk0 : 2;
	bool texelOffset : 1; // *Offset()
	bool isShadow : 1;
	bool isArray : 1;
	unsigned texType : 2; // 2D, 3D, Cube, Buffer
	bool computeLOD : 1; // 0 for *Lod()
	bool notSupplyLOD : 1; // 0 for *Lod() or when a bias is applied
	bool calcGradients : 1; // 0 for *Grad()
	unsigned unk1 : 1;
	unsigned resultType : 4; // integer, unsigned, float TODO: why is this 4 bits?
	unsigned unk2 : 4;
};

struct DualTexCtrl {
	unsigned samplerIndex0 : 2;
	unsigned unk0 : 2;
	unsigned texIndex0 : 2;
	unsigned samplerIndex1 : 2;
	unsigned texIndex1 : 2;
	unsigned unk1 : 22;
};

static void DumpADD(uint64_t word, Regs regs, Regs nextRegs, uint64_t *consts, unsigned dataReg)
{
	printf("# ADD: %016" PRIx64 "\n", word);
	ADD ADD;
	memcpy((char *) &ADD, (char *) &word, sizeof(ADD));
	ADDOpInfo info = findADDOpInfo(ADD.op);

	printf("%s", info.name);

	// float16 seems like it doesn't support output modifiers
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
		case ADDBlending:
			// Note: in this case, regs.uniformConst == location | 0x8
			// This probably means we can't load uniforms or immediates in the
			// same instruction. This re-uses the encoding that normally means
			// "disabled", where the low 4 bits are ignored. Perhaps the extra
			// 0x8 or'd in indicates this is happening.
			printf("location:%d, ", regs.uniformConst & 0x7);
			// fallthrough
		case ADDOneSrc:
			DumpSrc(ADD.src0, regs, consts, false);
			break;
		case ADDTex:
		case ADDTexCompact: {
			int texIndex;
			int samplerIndex;
			bool dualTex = false;
			if (info.srcType == ADDTexCompact) {
				texIndex = (ADD.op >> 3) & 0x7;
				samplerIndex = (ADD.op >> 7) & 0x7;
				bool unknown = (ADD.op & 0x40);
				// TODO: figure out if the unknown bit is ever 0
				if (!unknown)
					printf("unknown ");
			} else {
				uint64_t constVal = GetConst(consts, regs);
				uint32_t controlBits = (ADD.op & 0x8) ? (constVal >> 32) : constVal;
				TexCtrl ctrl;
				memcpy((char *) &ctrl, (char *) &controlBits, sizeof(ctrl));

				// TODO: figure out what actually triggers dual-tex
				if (ctrl.resultType == 9) {
					DualTexCtrl dualCtrl;
					memcpy((char *) &dualCtrl, (char *) &controlBits, sizeof(ctrl));
					printf("(dualtex) tex0:%d samp0:%d tex1:%d samp1:%d ",
							dualCtrl.texIndex0, dualCtrl.samplerIndex0,
							dualCtrl.texIndex1, dualCtrl.samplerIndex1);
					if (dualCtrl.unk0 != 3)
						printf("unk:%d ", dualCtrl.unk0);
					dualTex = true;
				} else {
					if (ctrl.noMergeIndex) {
						texIndex = ctrl.texIndex;
						samplerIndex = ctrl.samplerIndex;
					} else {
						texIndex = samplerIndex = ctrl.texIndex;
						unsigned unk = ctrl.samplerIndex >> 2;
						if (unk != 3)
							printf("unk:%d ", unk);
						if (ctrl.samplerIndex & 1)
							texIndex = -1;
						if (ctrl.samplerIndex & 2)
							samplerIndex = -1;
					}

					if (ctrl.unk0 != 3)
						printf("unk0:%d ", ctrl.unk0);
					if (ctrl.unk1)
						printf("unk1 ");
					if (ctrl.unk2 != 0xf)
						printf("unk2:%x ", ctrl.unk2);

					switch (ctrl.resultType) {
						case 0x4:
							printf("f32 "); break;
						case 0xe:
							printf("i32 "); break;
						case 0xf:
							printf("u32 "); break;
						default:
							printf("unktype(%x) ", ctrl.resultType);
					}

					switch (ctrl.texType) {
						case 0:
							printf("cube "); break;
						case 1:
							printf("buffer "); break;
						case 2:
							printf("2D "); break;
						case 3:
							printf("3D "); break;
					}

					if (ctrl.isShadow)
						printf("shadow ");
					if (ctrl.isArray)
						printf("array ");

					if (!ctrl.filter) {
						if (ctrl.calcGradients) {
							int comp = (controlBits >> 20) & 0x3;
							printf("txg comp:%d ", comp);
						} else {
							printf("txf ");
						}
					} else {
						if (!ctrl.notSupplyLOD) {
							if (ctrl.computeLOD)
								printf("lod_bias ");
							else
								printf("lod ");
						}

						if (!ctrl.calcGradients)
							printf("grad ");
					}

					if (ctrl.texelOffset)
						printf("offset ");
				}
			}

			if (!dualTex) {
				if (texIndex == -1)
					printf("tex:indirect ");
				else
					printf("tex:%d ", texIndex);

				if (samplerIndex == -1)
					printf("samp:indirect ");
				else
					printf("samp:%d ", samplerIndex);
			}
			// fallthrough
		}
		case ADDVaryingInterp: {
			if (ADD.op & 0x200)
				printf("reuse ");
			if (ADD.op & 0x400)
				printf("flat ");
			switch ((ADD.op >> 7) & 0x3) {
				case 0: printf("per_frag "); break;
				case 1: printf("centroid "); break;
				case 2: break;
				case 3: printf("explicit "); break;
				default: break;
			}
			printf("vec%d ", ((ADD.op >> 5) & 0x3) + 1);
			unsigned addr = ADD.op & 0x1f;
			if (addr < 0b10100) {
				// direct addr
				printf("addr:%d", addr);
			} else if (addr < 0b11000) {
				// unknown
				printf("addr:%d(unknown)", addr);
			} else {
				printf("addr:");
				DumpSrc(ADD.op & 0x7, regs, consts, false);
			}
			printf(", ");
			DumpSrc(ADD.src0, regs, consts, false);
			break;
		}	
		case ADDTwoSrc:
			DumpSrc(ADD.src0, regs, consts, false);
			printf(", ");
			DumpSrc(ADD.op & 0x7, regs, consts, false);
			break;
		case ADDThreeSrc:
			DumpSrc(ADD.src0, regs, consts, false);
			printf(", ");
			DumpSrc(ADD.op & 0x7, regs, consts, false);
			printf(", ");
			DumpSrc((ADD.op >> 3) & 0x7, regs, consts, false);
			break;
		case ADDTwoSrcFmod:
			if (ADD.op & 0x10)
				printf("-");
			if (ADD.op & 0x1000)
				printf("abs(");
			DumpSrc(ADD.src0, regs, consts, false);
			switch ((ADD.op >> 6) & 0x3) {
				case 3:
					printf(".x");
					break;
				default:
					break;
			}
			if (ADD.op & 0x1000)
				printf(")");
			printf(", ");
			if (ADD.op & 0x20)
				printf("-");
			if (ADD.op & 0x8)
				printf("abs(");
			DumpSrc(ADD.op & 0x7, regs, consts, false);
			switch ((ADD.op >> 6) & 0x3) {
				case 1:
				case 3:
					printf(".x");
					break;
				case 2:
					printf(".y");
					break;
				case 0:
					break;
				default:
					printf(".unk");
					break;
			}
			if (ADD.op & 0x8)
				printf(")");
			break;
		case ADDTwoSrcFmod16:
			if (ADD.op & 0x10)
				printf("-");
			if (ADD.op & 0x1000)
				printf("abs(");
			DumpSrc(ADD.src0, regs, consts, false);
			if (ADD.op & 0x1000)
				printf(")");
			Dump16Swizzle((ADD.op >> 6) & 0x3);
			printf(", ");
			if (ADD.op & 0x20)
				printf("-");
			if (ADD.op & 0x8)
				printf("abs(");
			DumpSrc(ADD.op & 0x7, regs, consts, false);
			Dump16Swizzle((ADD.op >> 8) & 0x3);
			if (ADD.op & 0x8)
				printf(")");
			break;
		case ADDTwoSrcFmod16Commutative: {
			bool abs1 = ADD.op & 0x8;
			bool abs2 = (ADD.op & 0x7) < ADD.src0;
			if (ADD.op & 0x10)
				printf("-");
			if (abs1 || abs2)
				printf("abs(");
			DumpSrc(ADD.src0, regs, consts, false);
			Dump16Swizzle((ADD.op >> 6) & 0x3);
			if (abs1 || abs2)
				printf(")");
			printf(", ");
			if (ADD.op & 0x20)
				printf("-");
			if (abs1 && abs2)
				printf("abs(");
			DumpSrc(ADD.op & 0x7, regs, consts, false);
			Dump16Swizzle((ADD.op >> 8) & 0x3);
			if (abs1 && abs2)
				printf(")");
			break;
		}
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
	if (info.hasDataReg) {
		printf(", R%d", dataReg);
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

void DumpInstr(const AluInstr &instr, Regs nextRegs, uint64_t *consts, unsigned dataReg)
{
	printf("# regs: %016" PRIx32 "\n", instr.regBits);
	Regs regs;
	memcpy((char *) &regs, (char *) &instr.regBits, sizeof(regs));
	DumpRegs(regs);
	DumpFMA(instr.FMABits, regs, nextRegs, consts);
	DumpADD(instr.ADDBits, regs, nextRegs, consts, dataReg);
}

struct Header {
	uint64_t unk0 : 11;
	// true if the execution mask of the next clause is the same as the mask of
	// the current clause.
	uint64_t backToBack : 1;
	uint64_t noEndOfShader : 1;
	uint64_t unk1 : 2;
	// Set to true for fragment shaders, to implement this bit of spec text
	// from section 7.1.5 of the GLSL ES spec:
	//
	// "Stores to image and buffer variables performed by helper invocations
	// have no effect on the underlying image or buffer memory."
	//
	// Helper invocations are threads (invocations) corresponding to pixels in
	// a quad that aren't actually part of the triangle, but are included to
	// make derivatives work correctly. They're usually turned on, but they
	// need to be masked off for GLSL-level stores. This bit seems to be the
	// only bit that's actually different between fragment shaders and other
	// shaders, so this is probably what it's doing.
	uint64_t elideWrites : 1;
	// If backToBack is off:
	// - true for conditional branches and fallthrough
	// - false for unconditional branches
	// The blob seems to always set it to true if back-to-back is on.
	uint64_t branchCond : 1;
	uint64_t unk2 : 1;
	uint64_t dataReg : 6;
	uint64_t scoreboardDeps : 8;
	uint64_t scoreboardIndex : 3;
	uint64_t clauseType : 4;
	uint64_t unk3 : 1; // part of clauseType?
	uint64_t nextClauseType : 4;
	uint64_t unk4 : 1; // part of nextClauseType?
};

void DumpHeader(Header header)
{
	if (header.clauseType != 0) {
		printf("id(%d) ", header.scoreboardIndex);
	}
	if (header.scoreboardDeps != 0) {
		printf("next-wait(");
		bool first = true;
		for (unsigned i = 0; i < 8; i++) {
			if (header.scoreboardDeps & (1 << i)) {
				if (!first) {
					printf(", ");
				}
				printf("%d", i);
				first = false;
			}
		}
		printf(") ");
	}

	if (!header.noEndOfShader)
		printf("eos ");

	if (!header.backToBack) {
		printf("nbb ");
		if (header.branchCond)
			printf("branch-cond ");
		else
			printf("branch-uncond ");
	}

	if (header.elideWrites)
		printf("we ");

	if (header.unk0)
		printf("unk0");
	if  (header.unk1)
		printf("unk1");
	if (header.unk2)
		printf("unk2");
	if (header.unk3)
		printf("unk3");
	if (header.unk4)
		printf("unk4");

	printf("\n");

	printf("# clause type %d, next clause type %d\n",
		   header.clauseType, header.nextClauseType);
}

void DumpClause(uint32_t *words, unsigned *size)
{
	// State for a decoded clause
	AluInstr instrs[8] = {};
	uint64_t consts[6] = {};
	unsigned numInstrs = 0;
	unsigned numConsts = 0;
	uint64_t headerBits = 0;

	unsigned i;
	for (i = 0; ; i++, words += 4) {
		printf("# ");
		for (int j = 0; j < 4; j++)
			printf("%08x ", words[3 - j]); // low bit on the right
		printf("\n");
		unsigned tag = bits(words[0], 0, 8);

		// speculatively decode some things that are common between many formats, so we can share some code
		AluInstr mainInstr = {};
		// 20 bits
		mainInstr.ADDBits = bits(words[2], 2, 32 - 13);
		// 23 bits
		mainInstr.FMABits = bits(words[1], 11, 32) | bits(words[2], 0, 2) << (32 - 11);
		// 35 bits
		mainInstr.regBits = ((uint64_t) bits(words[1], 0, 11)) << 24 | (uint64_t) bits(words[0], 8, 32);

		uint64_t const0 = bits(words[0], 8, 32) << 4 | (uint64_t) words[1] << 28 | bits(words[2], 0, 4) << 60;
		uint64_t const1 = bits(words[2], 4, 32) << 4 | (uint64_t) words[3] << 32;

		bool stop = tag & 0x40;

		if (tag & 0x80) {
			unsigned idx = stop ? 5 : 2;
			mainInstr.ADDBits |= ((tag >> 3) & 0x7) << 17;
			instrs[idx + 1] = mainInstr;
			instrs[idx].ADDBits = bits(words[3], 0, 17) | ((tag & 0x7) << 17);
			instrs[idx].FMABits |= bits(words[2], 19, 32) << 10;
			consts[0] = bits(words[3], 17, 32) << 4;
		} else {
			bool done = false;
			switch ((tag >> 3) & 0x7) {
				case 0x0:
					switch (tag & 0x7) {
						case 0x3:
							mainInstr.ADDBits |= bits(words[3], 29, 32) << 17;
							instrs[1] = mainInstr;
							numInstrs = 2;
							done = stop;
							break;
						case 0x4:
							instrs[2].ADDBits = bits(words[3], 0, 17) | bits(words[3], 29, 32) << 17;
							instrs[2].FMABits |= bits(words[2], 19, 32) << 10;
							consts[0] = const0;
							numInstrs = 3;
							numConsts = 1;
							done = stop;
							break;
						case 0x1:
						case 0x5:
							instrs[2].ADDBits = bits(words[3], 0, 17) | bits(words[3], 29, 32) << 17;
							instrs[2].FMABits |= bits(words[2], 19, 32) << 10;
							mainInstr.ADDBits |= bits(words[3], 26, 29) << 17;
							instrs[3] = mainInstr;
							if ((tag & 0x7) == 0x5) {
								numInstrs = 4;
								done = stop;
							}
							break;
						case 0x6:
							instrs[5].ADDBits = bits(words[3], 0, 17) | bits(words[3], 29, 32) << 17;
							instrs[5].FMABits |= bits(words[2], 19, 32) << 10;
							consts[0] = const0;
							numInstrs = 6;
							numConsts = 1;
							done = stop;
							break;
						case 0x7:
							instrs[5].ADDBits = bits(words[3], 0, 17) | bits(words[3], 29, 32) << 17;
							instrs[5].FMABits |= bits(words[2], 19, 32) << 10;
							mainInstr.ADDBits |= bits(words[3], 26, 29) << 17;
							instrs[6] = mainInstr;
							numInstrs = 7;
							done = stop;
							break;
						default:
							printf("unknown tag bits 0x%02x\n", tag);
					}
					break;
				case 0x1:
					headerBits = bits(words[2], 19, 32) | ((uint64_t) words[3] << (32 - 19));
					mainInstr.ADDBits |= (tag & 0x7) << 17;
					instrs[0] = mainInstr;
					numInstrs = 1;
					done = stop;
					// only constants can come after this
					break;
				case 0x5:
					headerBits = bits(words[2], 19, 32) | ((uint64_t) words[3] << (32 - 19));
					mainInstr.ADDBits |= (tag & 0x7) << 17;
					instrs[0] = mainInstr;
					break;
				case 0x2:
				case 0x3: {
					unsigned idx = ((tag >> 3) & 0x7) == 2 ? 4 : 7;
					mainInstr.ADDBits |= (tag & 0x7) << 17;
					instrs[idx] = mainInstr;
					consts[0] |= (bits(words[2], 19, 32) | ((uint64_t) words[3] << 13)) << 19;
					numConsts = 1;
					numInstrs = idx + 1;
					done = stop;
					break;
				}
				case 0x4: {
					unsigned idx = stop ? 4 : 1;
					mainInstr.ADDBits |= (tag & 0x7) << 17;
					instrs[idx] = mainInstr;
					instrs[idx + 1].FMABits |= bits(words[3], 22, 32);
					instrs[idx + 1].regBits = bits(words[2], 19, 32) | (bits(words[3], 0, 22) << (32 - 19));
					break;
				}
				case 0x6:
				case 0x7: {
					unsigned pos = tag & 0xf;
					// note that `pos' encodes both the total number of
					// instructions and the position in the constant stream,
					// presumably because decoded constants and instructions
					// share a buffer in the decoder, but we only care about
					// the position in the constant stream; the total number of
					// instructions is redundant.
					unsigned const_idx = 7;
					switch (pos) {
						case 0:
						case 1:
						case 2:
						case 6:
							const_idx = 0;
							break;
						case 3:
						case 4:
						case 7:
						case 9:
							const_idx = 1;
							break;
						case 5:
						case 0xa:
							const_idx = 2;
							break;
						case 8:
						case 0xb:
						case 0xc:
							const_idx = 3;
							break;
						case 0xd:
							const_idx = 4;
							break;
						default:
							printf("# unknown pos 0x%x\n", pos);
					}
					if (numConsts < const_idx + 2)
						numConsts = const_idx + 2;
					consts[const_idx] = const0;
					consts[const_idx + 1] = const1;
					done = stop;
					break;
				}
				default:
					break;
			}

			if (done)
				break;
		}
	}

	*size = i + 1;

	printf("# header: %012" PRIx64 "\n", headerBits);
	Header header;
	memcpy((char *) &header, (char *) &headerBits, sizeof(Header));
	DumpHeader(header);

	printf("{\n");
	for (i = 0; i < numInstrs; i++) {
		Regs nextRegs;
		if (i + 1 == numInstrs) {
			memcpy((char *) &nextRegs, (char *) &instrs[0].regBits,
					sizeof(nextRegs));
		} else {
			memcpy((char *) &nextRegs, (char *) &instrs[i + 1].regBits,
					sizeof(nextRegs));
		}

		DumpInstr(instrs[i], nextRegs, consts, header.dataReg);
	}
	printf("}\n");

	for (int i = 0; i < numConsts; i++) {
		printf("# const%d: %08x\n", 2 * i, consts[i] & 0xffffffff);
		printf("# const%d: %08x\n", 2 * i + 1, consts[i] >> 32);
	}
}

void DisassembleBifrost(uint8_t* instBlob, uint32_t size)
{
	uint32_t *words = (uint32_t *) instBlob;
	uint32_t *wordsEnd = words + (size / 4);
	while (words != wordsEnd)
	{
		// we don't know what the program-end bit is quite yet, so for now just
		// assume that an all-0 quadword is padding
		uint32_t zero[4] = {};
		if (memcmp(words, zero, 4 * sizeof(uint32_t)) == 0)
			break;
		unsigned size;
		DumpClause(words, &size);
		words += size * 4;
	}
}

