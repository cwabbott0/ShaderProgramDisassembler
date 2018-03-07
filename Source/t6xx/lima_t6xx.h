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

/* lima_t6xx.h - common structures for T600 and T650 variants */

#ifndef __lima_t6xx_h__
#define __lima_t6xx_h__

#include <stdint.h>
#include <stdbool.h>

typedef enum
{
	lima_t6xx_word_type_alu,
	lima_t6xx_word_type_load_store,
	lima_t6xx_word_type_texture,
	lima_t6xx_word_type_unknown
} lima_t6xx_word_type_e;

extern lima_t6xx_word_type_e lima_t6xx_word_type[];

 //size in quadwords, i.e. multiples of 128 bits
extern unsigned lima_t6xx_word_size[];

typedef enum
{
	lima_t6xx_alu_vmul,
	lima_t6xx_alu_sadd,
	lima_t6xx_alu_smul,
	lima_t6xx_alu_vadd,
	lima_t6xx_alu_lut
} lima_t6xx_alu_e;

/*
 * ALU words
 */

typedef enum
{
	lima_t6xx_alu_op_fadd       = 0x10,
	lima_t6xx_alu_op_fmul       = 0x14,
	lima_t6xx_alu_op_fmin       = 0x28,
	lima_t6xx_alu_op_fmax       = 0x2C,
	lima_t6xx_alu_op_fmov       = 0x30,
	lima_t6xx_alu_op_ffloor     = 0x36,
	lima_t6xx_alu_op_fceil      = 0x37,
	lima_t6xx_alu_op_fdot3      = 0x3C,
	lima_t6xx_alu_op_fdot3r     = 0x3D,
	lima_t6xx_alu_op_fdot4      = 0x3E,
	lima_t6xx_alu_op_freduce    = 0x3F,
	lima_t6xx_alu_op_iadd       = 0x40,
	lima_t6xx_alu_op_isub       = 0x46,
	lima_t6xx_alu_op_imul       = 0x58,
	lima_t6xx_alu_op_imov       = 0x7B,
	lima_t6xx_alu_op_feq        = 0x80,
	lima_t6xx_alu_op_fne        = 0x81,
	lima_t6xx_alu_op_flt        = 0x82,
	lima_t6xx_alu_op_fle        = 0x83,
	lima_t6xx_alu_op_f2i        = 0x99,
	lima_t6xx_alu_op_ieq        = 0xA0,
	lima_t6xx_alu_op_ine        = 0xA1,
	lima_t6xx_alu_op_ilt        = 0xA4,
	lima_t6xx_alu_op_ile        = 0xA5,
	lima_t6xx_alu_op_ball       = 0xA9,
	lima_t6xx_alu_op_bany       = 0xB1,
	lima_t6xx_alu_op_i2f        = 0xB8,
	lima_t6xx_alu_op_csel       = 0xC5,
	lima_t6xx_alu_op_fatan_pt2  = 0xE8,
	lima_t6xx_alu_op_frcp       = 0xF0,
	lima_t6xx_alu_op_frsqrt     = 0xF2,
	lima_t6xx_alu_op_fsqrt      = 0xF3,
	lima_t6xx_alu_op_fexp2      = 0xF4,
	lima_t6xx_alu_op_flog2      = 0xF5,
	lima_t6xx_alu_op_fsin       = 0xF6,
	lima_t6xx_alu_op_fcos       = 0xF7,
	lima_t6xx_alu_op_fatan2_pt1 = 0xF9,
} lima_t6xx_alu_op_e;

typedef enum
{
	lima_t6xx_outmod_none = 0,
	lima_t6xx_outmod_pos  = 1,
	lima_t6xx_outmod_int  = 2,
	lima_t6xx_outmod_sat  = 3
} lima_t6xx_outmod_e;

typedef enum
{
	lima_t6xx_reg_mode_half = 1,
	lima_t6xx_reg_mode_full = 2
} lima_t6xx_reg_mode_e;

typedef enum
{
	lima_t6xx_dest_override_lower = 0,
	lima_t6xx_dest_override_upper = 1,
	lima_t6xx_dest_override_none = 2
} lima_t6xx_dest_override_e;

typedef struct
__attribute__((__packed__))
{
	bool abs         : 1;
	bool negate      : 1;
	
	/* replicate lower half if dest = half, or low/high half selection if
	 * dest = full
	 */
	bool rep_low     : 1;
	bool rep_high    : 1; /* unused if dest = full */
	bool half        : 1; /* only matters if dest = full */
	unsigned swizzle : 8;
} lima_t6xx_vector_alu_src_t;

typedef struct
__attribute__((__packed__))
{
	lima_t6xx_alu_op_e op               :  8;
	lima_t6xx_reg_mode_e reg_mode   :  2;
	unsigned src1 : 13;
	unsigned src2 : 13;
	lima_t6xx_dest_override_e dest_override : 2;
	lima_t6xx_outmod_e outmod               : 2;
	unsigned mask                           : 8;
} lima_t6xx_vector_alu_t;

typedef struct
__attribute__((__packed__))
{
	bool abs           : 1;
	bool negate        : 1;
	bool full          : 1; /* 0 = half, 1 = full */
	unsigned component : 3;
} lima_t6xx_scalar_alu_src_t;

typedef struct
__attribute__((__packed__))
{
	lima_t6xx_alu_op_e op         :  8;
	unsigned src1             :  6;
	unsigned src2             : 11;
	unsigned unknown          :  1;
	lima_t6xx_outmod_e outmod :  2;
	bool output_full          :  1;
	unsigned output_component :  3;
} lima_t6xx_scalar_alu_t;

typedef struct
__attribute__((__packed__))
{
	unsigned src1_reg : 5;
	unsigned src2_reg : 5;
	unsigned out_reg  : 5;
	bool src2_imm     : 1;
} lima_t6xx_reg_info_t;

typedef enum
{
	lima_t6xx_jmp_writeout_op_branch_uncond = 1,
	lima_t6xx_jmp_writeout_op_branch_cond = 2,
	lima_t6xx_jmp_writeout_op_writeout = 7,
} lima_t6xx_jmp_writeout_op_e;

typedef struct
__attribute__((__packed__))
{
	lima_t6xx_jmp_writeout_op_e op : 3; /* == branch_uncond */
	unsigned dest_tag : 4; /* tag of branch destination */
	unsigned unknown : 2;
	int offset : 7;
} lima_t6xx_branch_uncond_t;

typedef struct
__attribute__((__packed__))
{
	lima_t6xx_jmp_writeout_op_e op : 3; /* == branch_cond */
	unsigned dest_tag : 4; /* tag of branch destination */
	int offset : 7;
	unsigned cond : 2;
} lima_t6xx_branch_cond_t;

typedef struct
__attribute__((__packed__))
{
	lima_t6xx_jmp_writeout_op_e op : 3; /* == writeout */
	unsigned unknown : 13;
} lima_t6xx_writeout_t;

/*
 * Load/store words
 */

typedef enum
{
	lima_t6xx_op_ld_st_noop   = 0x03,
	lima_t6xx_op_load_attr_16 = 0x95,
	lima_t6xx_op_load_attr_32 = 0x94,
	lima_t6xx_op_load_vary_16 = 0x99,
	lima_t6xx_op_load_vary_32 = 0x98,
	lima_t6xx_op_load_uniform_16 = 0xAC,
	lima_t6xx_op_load_uniform_32 = 0xB0,
	lima_t6xx_op_store_vary_16 = 0xD5,
	lima_t6xx_op_store_vary_32 = 0xD4
} lima_t6xx_load_store_op_e;

typedef struct
__attribute__((__packed__))
{
	lima_t6xx_load_store_op_e op : 8;
	unsigned reg     : 5;
	unsigned mask    : 4;
	unsigned swizzle : 8;
	unsigned unknown : 26;
	unsigned address : 9;
} lima_t6xx_load_store_word_t;

typedef struct
__attribute__((__packed__))
{
	unsigned type      : 4;
	unsigned next_type : 4;
	uint64_t word1     : 60;
	uint64_t word2     : 60;
} lima_t6xx_load_store_t;


#endif
