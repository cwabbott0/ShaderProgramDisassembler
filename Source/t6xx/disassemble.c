/* Author(s):
 *   Connor Abbott
 *
 * Copyright (c) 2013 Connor Abbott (connor@abbott.cx)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#include "lima_t6xx.h"
#include "../Disasm.h"
#include "hfloat.h"
#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <inttypes.h>
#include <string.h>

lima_t6xx_word_type_e lima_t6xx_word_type[16] = {
	lima_t6xx_word_type_unknown,    /* 0x0 */
	lima_t6xx_word_type_unknown,    /* 0x1 */
	lima_t6xx_word_type_unknown,    /* 0x2 */
	lima_t6xx_word_type_texture,    /* 0x3 */
	lima_t6xx_word_type_unknown,    /* 0x4 */
	lima_t6xx_word_type_load_store, /* 0x5 */
	lima_t6xx_word_type_unknown,    /* 0x6 */
	lima_t6xx_word_type_unknown,    /* 0x7 */
	lima_t6xx_word_type_alu,        /* 0x8 */
	lima_t6xx_word_type_alu,        /* 0x9 */
	lima_t6xx_word_type_alu,        /* 0xA */
	lima_t6xx_word_type_alu,        /* 0xB */
	lima_t6xx_word_type_unknown,    /* 0xC */
	lima_t6xx_word_type_alu,        /* 0xD */
	lima_t6xx_word_type_alu,        /* 0xE */
	lima_t6xx_word_type_unknown,    /* 0xF */
};

unsigned lima_t6xx_word_size[16] = {
	0, /* 0x0 */
	0, /* 0x1 */
	0, /* 0x2 */
	1, /* 0x3 */
	0, /* 0x4 */
	1, /* 0x5 */
	0, /* 0x6 */
	0, /* 0x7 */
	1, /* 0x8 */
	2, /* 0x9 */
	3, /* 0xA */
	4, /* 0xB */
	0, /* 0xC */
	2, /* 0xD */
	3, /* 0xE */
	0, /* 0xF */
};

static char* alu_opcode_names[256] = {
	[lima_t6xx_alu_op_fadd]       = "fadd",
	[lima_t6xx_alu_op_fmul]       = "fmul",
	[lima_t6xx_alu_op_fmin]       = "fmin",
	[lima_t6xx_alu_op_fmax]       = "fmax",
	[lima_t6xx_alu_op_fmov]       = "fmov",
	[lima_t6xx_alu_op_ffloor]     = "ffloor",
	[lima_t6xx_alu_op_fceil]      = "fceil",
	[lima_t6xx_alu_op_fdot3]      = "fdot3",
	[lima_t6xx_alu_op_fdot3r]     = "fdot3r",
	[lima_t6xx_alu_op_fdot4]      = "fdot4",
	[lima_t6xx_alu_op_freduce]    = "freduce",
	[lima_t6xx_alu_op_iadd]       = "iadd",
	[lima_t6xx_alu_op_isub]       = "isub",
	[lima_t6xx_alu_op_imul]       = "imul",
	[lima_t6xx_alu_op_imov]       = "imov",
	[lima_t6xx_alu_op_feq]        = "feq",
	[lima_t6xx_alu_op_fne]        = "fne",
	[lima_t6xx_alu_op_flt]        = "flt",
	[lima_t6xx_alu_op_fle]        = "fle",
	[lima_t6xx_alu_op_f2i]        = "f2i",
	[lima_t6xx_alu_op_ieq]        = "ieq",
	[lima_t6xx_alu_op_ine]        = "ine",
	[lima_t6xx_alu_op_ilt]        = "ilt",
	[lima_t6xx_alu_op_ile]        = "ile",
	[lima_t6xx_alu_op_ball]       = "ball",
	[lima_t6xx_alu_op_bany]       = "bany",
	[lima_t6xx_alu_op_i2f]        = "i2f",
	[lima_t6xx_alu_op_csel]       = "csel",
	[lima_t6xx_alu_op_fatan_pt2]  = "fatan_pt2",
	[lima_t6xx_alu_op_frcp]       = "frcp",
	[lima_t6xx_alu_op_frsqrt]     = "frsqrt",
	[lima_t6xx_alu_op_fsqrt]      = "fsqrt",
	[lima_t6xx_alu_op_fexp2]      = "fexp2",
	[lima_t6xx_alu_op_flog2]      = "flog2",
	[lima_t6xx_alu_op_fsin]       = "fsin",
	[lima_t6xx_alu_op_fcos]       = "fcos",
	[lima_t6xx_alu_op_fatan2_pt1] = "fatan2_pt1"
};

static char* load_store_opcode_names[256] = {
	[lima_t6xx_op_load_attr_16] = "ld_attr_16",
	[lima_t6xx_op_load_attr_32] = "ld_attr_32",
	[lima_t6xx_op_load_vary_16] = "ld_vary_16",
	[lima_t6xx_op_load_vary_32] = "ld_vary_32",
	[lima_t6xx_op_load_uniform_16] = "ld_uniform_16",
	[lima_t6xx_op_load_uniform_32] = "ld_uniform_32",
	[lima_t6xx_op_store_vary_16] = "st_vary_16",
	[lima_t6xx_op_store_vary_32] = "st_vary_32"
};

static void print_alu_opcode(lima_t6xx_alu_op_e op)
{
	if (alu_opcode_names[op])
		printf("%s", alu_opcode_names[op]);
	else
		printf("op_%02X", op);
}

static void print_ld_st_opcode(lima_t6xx_load_store_op_e op)
{
	if (load_store_opcode_names[op])
		printf("%s", load_store_opcode_names[op]);
	else
		printf("op_%02X", op);
}

static void print_reg(unsigned reg, bool half)
{
	if (half)
		printf("h");
	printf("r%u", reg);
}

static char* outmod_names[4] = {
	"",
	" pos",
	"",
	" sat"
};

static void print_outmod(lima_t6xx_outmod_e outmod)
{
	printf("%s", outmod_names[outmod]);
}

static void print_quad_word(uint32_t* words)
{
	unsigned i;
	for (i = 0; i < 4; i++)
		printf("0x%08X ", words[i]);
	printf("\n");
}

static void print_16bit_word(uint16_t word)
{
	printf("%04X", word);
}

static void print_16bit_words(uint16_t* words, unsigned num_words)
{
	unsigned i;
	for (i = 0; i < num_words; i++)
	{
		print_16bit_word(words[num_words - i - 1]);
	}
}

static void print_alu_field(uint16_t* words, uint16_t reg_word,
							unsigned num_words)
{
	print_16bit_words(words, num_words);
	printf(" (");
	print_16bit_word(reg_word);
	printf(")\n");
}

static void print_vector_src(unsigned src_binary, bool out_high,
							 bool out_half, unsigned reg)
{
	lima_t6xx_vector_alu_src_t* src = (lima_t6xx_vector_alu_src_t*)&src_binary;
	
	if (src->negate)
		printf("-");
	if (src->abs)
		printf("abs(");
	
	//register
	
	if (out_half)
	{
		unsigned half_reg;
		if (out_high)
		{
			if (src->rep_low)
				half_reg = reg * 2;
			else
				half_reg = reg * 2 + 1;
		}
		else
		{
			if (src->rep_high)
				half_reg = reg * 2 + 1;
			else
				half_reg = reg * 2;
		}
		print_reg(half_reg, true);
	}
	else
	{
		if (src->half)
			print_reg(reg * 2 + src->rep_low, true);
		else
			print_reg(reg, false);
	}
	
	//swizzle
	
	if (src->swizzle != 0xE4) //default swizzle
	{
		unsigned i;
		static const char c[4] = "xyzw";
		
		printf(".");
		
		for (i = 0; i < 4; i++)
			printf("%c", c[(src->swizzle >> (i * 2)) & 3]);
	}
	
	if (src->abs)
		printf(")");
}

static uint16_t decode_vector_imm(unsigned src2_reg, unsigned imm)
{
	uint16_t ret;
	ret = src2_reg << 11;
	ret |= (imm & 0x7) << 8;
	ret |= (imm >> 3) & 0xFF;
	return ret;
}

static void print_vector_field(uint16_t* words, uint16_t reg_word)
{
	lima_t6xx_reg_info_t* reg_info = (lima_t6xx_reg_info_t*)&reg_word;
	lima_t6xx_vector_alu_t* alu_field = (lima_t6xx_vector_alu_t*) words;
	
	if (alu_field->reg_mode != lima_t6xx_reg_mode_half &&
		alu_field->reg_mode != lima_t6xx_reg_mode_full)
	{
		printf("unknown reg mode %u\n", alu_field->reg_mode);
	}
	
	print_alu_opcode(alu_field->op);
	print_outmod(alu_field->outmod);
	printf(" ");
	
	bool half, out_half, out_high;
	unsigned mask;
	
	half = (alu_field->reg_mode == lima_t6xx_reg_mode_half);
	
	if (half)
	{
		if (alu_field->mask & 0xF)
		{
			out_high = false;
			assert(!(alu_field->mask & 0xF0));
			mask = alu_field->mask;
		}
		else
		{
			out_high = true;
			mask = alu_field->mask >> 4;
		}
	}
	else
	{
		mask = alu_field->mask & 1;
		mask |= (alu_field->mask & 4) >> 1;
		mask |= (alu_field->mask & 16) >> 2;
		mask |= (alu_field->mask & 64) >> 3;
	}
	
	out_half = half;
	
	if (alu_field->dest_override != lima_t6xx_dest_override_none)
	{
		assert(!out_half);
		out_half = true;
		if (alu_field->dest_override == lima_t6xx_dest_override_lower)
			out_high = false;
		else if (alu_field->dest_override == lima_t6xx_dest_override_upper)
			out_high = true;
		else
			assert(0);
	}
	
	if (out_half)
	{
		if (out_high)
			print_reg(2 * reg_info->out_reg + 1, true);
		else
			print_reg(2 * reg_info->out_reg, true);
	}
	else
		print_reg(reg_info->out_reg, false);
	
	if (mask != 0xF)
	{
		unsigned i;
		static const char c[4] = "xyzw";
		
		printf(".");
		for (i = 0; i < 4; i++)
			if (mask & (1 << i))
				printf("%c", c[i]);
	}
	
	printf(", ");
	
	print_vector_src(alu_field->src1, out_high, half, reg_info->src1_reg);
	
	printf(", ");
	
	if (reg_info->src2_imm)
	{
		uint16_t imm = decode_vector_imm(reg_info->src2_reg,
										 alu_field->src2 >> 2);
		printf("%g", ogt_hfloat_to_float((ogt_hfloat_t)imm));
	}
	else
	{
		print_vector_src(alu_field->src2, out_high, half,
						 reg_info->src2_reg);
	}
	
	printf("\n");
}

static void print_scalar_src(unsigned src_binary, unsigned reg)
{
	lima_t6xx_scalar_alu_src_t* src = (lima_t6xx_scalar_alu_src_t*)&src_binary;
	
	if (src->negate)
		printf("-");
	if (src->abs)
		printf("abs(");
	
	if (src->full)
		print_reg(reg, false);
	else
		print_reg(reg * 2 + (src->component >> 2), true);
	
	static const char c[4] = "xyzw";\
	printf(".%c", c[src->full ? src->component >> 1 : src->component & 3]);
	
	if (src->abs)
		printf(")");
	
}

static uint16_t decode_scalar_imm(unsigned src2_reg, unsigned imm)
{
	uint16_t ret;
	ret = src2_reg << 11;
	ret |= (imm & 3) << 9;
	ret |= (imm & 4) << 6;
	ret |= (imm & 0x38) << 2;
	ret |= imm >> 6;
	return ret;
}

static void print_scalar_field(uint16_t* words, uint16_t reg_word)
{
	lima_t6xx_reg_info_t* reg_info = (lima_t6xx_reg_info_t*)&reg_word;
	lima_t6xx_scalar_alu_t* alu_field = (lima_t6xx_scalar_alu_t*) words;
	
	if (alu_field->unknown)
	{
		printf("scalar ALU unknown bit set\n");
	}
	
	print_alu_opcode(alu_field->op);
	print_outmod(alu_field->outmod);
	printf(" ");
	
	if (alu_field->output_full)
		print_reg(reg_info->out_reg, false);
	else
		print_reg(reg_info->out_reg * 2 + (alu_field->output_component >> 2),
				  true);
	
	static const char c[4] = "xyzw";
	printf(".%c, ",
		   c[alu_field->output_full ? alu_field->output_component >> 1 :
			 alu_field->output_component & 3]);
	
	print_scalar_src(alu_field->src1, reg_info->src1_reg);
	
	printf(", ");
	
	if (reg_info->src2_imm)
	{
		uint16_t imm = decode_scalar_imm(reg_info->src2_reg,
										 alu_field->src2);
		printf("%g", ogt_hfloat_to_float((ogt_hfloat_t)imm));
	}
	else
		print_scalar_src(alu_field->src2, reg_info->src2_reg);
	
	printf("\n");
}

static void print_compact_branch_writeout_field(uint16_t word)
{
	lima_t6xx_jmp_writeout_op_e op = word & 0x7;

	switch (op)
	{
		case lima_t6xx_jmp_writeout_op_branch_uncond:
		{
			lima_t6xx_branch_uncond_t br_uncond;
			memcpy((char*) &br_uncond, (char*) &word, sizeof(br_uncond));
			printf("br_uncond ");
			if (br_uncond.unknown != 1)
				printf("unknown:%d, ", br_uncond.unknown);
			if (br_uncond.offset > 0)
				printf("+");
			printf("%d\n", br_uncond.offset);
			break;
		}
		case lima_t6xx_jmp_writeout_op_branch_cond:
		{
			lima_t6xx_branch_cond_t br_cond;
			memcpy((char*) &br_cond, (char*) &word, sizeof(br_cond));
			printf("br_cond.");
			switch (br_cond.cond)
			{
				case 0: printf("unk0"); break;
				case 1: printf("false"); break;
				case 2: printf("true"); break;
				case 3: printf("unk3"); break;
				default: break;
			}
			printf(" ");
			if (br_cond.offset > 0)
				printf("+");
			printf("%d\n", br_cond.offset);
			break;
		}
		default:
		{
			printf("op_%d\n", word & 0x7);
		}
	}
}

static unsigned num_alu_fields_enabled(uint32_t control_word)
{
	unsigned ret = 0;
	
	if ((control_word >> 17) & 1)
		ret++;
	
	if ((control_word >> 19) & 1)
		ret++;
	
	if ((control_word >> 21) & 1)
		ret++;
	
	if ((control_word >> 23) & 1)
		ret++;
	
	if ((control_word >> 25) & 1)
		ret++;
	
	return ret;
}

static float float_bitcast(uint32_t integer)
{
	union {
		uint32_t i;
		float f;
	} v;
	
	v.i = integer;
	return v.f;
}

static void print_alu_word(uint32_t* words, unsigned num_quad_words)
{
	unsigned i;
	for (i = 0; i < num_quad_words; i++)
		print_quad_word(&words[4*i]);
	
	uint32_t control_word = words[0];
	uint16_t* beginning_ptr = (uint16_t*)(words + 1);
	unsigned num_fields = num_alu_fields_enabled(control_word);
	uint16_t* word_ptr = beginning_ptr + num_fields;
	unsigned num_words = 2 + num_fields;
	
	if ((control_word >> 16) & 1)
	{
		printf("unknown bit 16 enabled\n");
	}
	
	if ((control_word >> 17) & 1)
	{
		printf("vmul field: ");
		print_alu_field(word_ptr, *beginning_ptr, 3);
		print_vector_field(word_ptr, *beginning_ptr);
		beginning_ptr += 1;
		word_ptr += 3;
		num_words += 3;
	}
	
	if ((control_word >> 18) & 1)
	{
		printf("unknown bit 18 enabled\n");
	}
	
	if ((control_word >> 19) & 1)
	{
		printf("sadd field: ");
		print_alu_field(word_ptr, *beginning_ptr, 2);
		print_scalar_field(word_ptr, *beginning_ptr);
		beginning_ptr += 1;
		word_ptr += 2;
		num_words += 2;
	}
	
	if ((control_word >> 20) & 1)
	{
		printf("unknown bit 20 enabled\n");
	}
	
	if ((control_word >> 21) & 1)
	{
		printf("vadd field: ");
		print_alu_field(word_ptr, *beginning_ptr, 3);
		print_vector_field(word_ptr, *beginning_ptr);
		beginning_ptr += 1;
		word_ptr += 3;
		num_words += 3;
	}
	
	if ((control_word >> 22) & 1)
	{
		printf("unknown bit 22 enabled\n");
	}
	
	if ((control_word >> 23) & 1)
	{
		printf("smul field: ");
		print_alu_field(word_ptr, *beginning_ptr, 2);
		print_scalar_field(word_ptr, *beginning_ptr);
		beginning_ptr += 1;
		word_ptr += 2;
		num_words += 2;
	}
	
	if ((control_word >> 24) & 1)
	{
		printf("unknown bit 24 enabled\n");
	}
	
	if ((control_word >> 25) & 1)
	{
		printf("lut field:  ");
		print_alu_field(word_ptr, *beginning_ptr, 3);
		print_vector_field(word_ptr, *beginning_ptr);
		beginning_ptr += 1;
		word_ptr += 3;
		num_words += 3;
	}
	
	if ((control_word >> 26) & 1)
	{
		printf("compact output and branch field: ");
		print_16bit_word(*word_ptr);
		printf("\n");
		print_compact_branch_writeout_field(*word_ptr);
		word_ptr += 1;
		num_words += 1;
	}
	
	if ((control_word >> 27) & 1)
	{
		printf("output and branch field: ");
		print_16bit_words(word_ptr, 3);
		printf("\n");
		word_ptr += 3;
		num_words += 3;
	}
	
	if (num_quad_words > (num_words + 7) / 8)
	{
		assert(num_quad_words == (num_words + 15) / 8);
		//Assume that the extra quadword is constants
		uint32_t* consts = words + (4 * num_quad_words - 4);
		printf("constants: (%g, %g, %g, %g)\n",
			   float_bitcast(consts[0]),
			   float_bitcast(consts[1]),
			   float_bitcast(consts[2]),
			   float_bitcast(consts[3]));
	}
	
	printf("\n");
}

static void print_load_store_instr(uint64_t data)
{
	unsigned i;
	
	lima_t6xx_load_store_word_t* word = (lima_t6xx_load_store_word_t *) &data;
	printf("unknown: %07X\n", word->unknown);
	print_ld_st_opcode(word->op);
	printf(" r%d", word->reg);
	
	if (word->mask != 0xF)
	{
		printf(".");
		for (i = 0; i < 4; i++)
			if (word->mask & (1 << i))
				printf("%c", "xyzw"[i]);
	}
	
	printf(" %d", word->address);
	if (word->swizzle != 0xE4)
	{
		printf(".");
		for (i = 0; i < 4; i++)
			printf("%c", "xyzw"[(word->swizzle >> (2 * i)) & 3]);
	}
	
	printf("\n");
}

static void print_load_store_word(uint32_t* word)
{
	lima_t6xx_load_store_t* load_store = (lima_t6xx_load_store_t *) word;
	
	if (load_store->word1 != 3)
	{
		print_load_store_instr(load_store->word1);
	}
	if (load_store->word2 != 3)
	{
		print_load_store_instr(load_store->word2);
	}
}

void DisassembleMidgard(uint8_t* code, uint32_t size)
{
	uint32_t* words = (uint32_t*) code;
	unsigned num_words = size / 4;
	
	unsigned i = 0;
	while (i < num_words)
	{
		printf("offset = %d\n", i / 4);
		unsigned num_quad_words = lima_t6xx_word_size[words[i] & 0xF];
		switch (lima_t6xx_word_type[words[i] & 0xF])
		{
			case lima_t6xx_word_type_texture:
				printf("Texture word:\n");
				print_quad_word(&words[i]); //TODO
				printf("\n");
				break;
				
			case lima_t6xx_word_type_load_store:
				printf("Load/Store word:\n");
				print_load_store_word(&words[i]);
				printf("\n");
				break;
				
			case lima_t6xx_word_type_alu:
				printf("ALU word:\n");
				print_alu_word(&words[i], num_quad_words);
				break;
				
			default:
				printf("Unknown word type %u:\n", words[i] & 0xF);
				num_quad_words = 1;
				print_quad_word(&words[i]);
				printf("\n");
				break;
		}
		
		i += 4 * num_quad_words;
	}
}
