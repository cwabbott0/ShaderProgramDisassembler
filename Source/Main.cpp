#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <string>
#include <string.h>
#include <vector>

#include "BlockInfo.h"
#include "Disasm.h"

bool ReadFile(std::vector<uint8_t> *fileData, std::string filename)
{
	FILE* fp = nullptr;
	size_t filesize = 0;

	fp = fopen(filename.c_str(), "rb");
	if (!fp)
		return false;

	fseek(fp, 0, SEEK_END);
	filesize = ftell(fp);
	rewind(fp);

	fileData->resize(filesize);

	fread(fileData->data(), 1, filesize, fp);

	fclose(fp);
	return true;

err:
	if (fp) {
		fclose(fp);
	}
	return false;
}

void PrintFile(std::vector<uint8_t> *fileData, size_t offset = 0, size_t realpos = 0)
{
	printf("file size is 0x%08llx\n", fileData->size());
	uint32_t *data = reinterpret_cast<uint32_t*>(fileData->data() + offset);
	size_t size = fileData->size() / 4;
	for (size_t i = 0; i < size; i++)
	{
		uint32_t word = data[i];
		auto getL = [word](int off)
		{
			return (uint8_t)(word >> off);
		};
		printf("%s0x%08x :0x%08x: %c%c%c%c\n",
			((realpos - offset) / 4 == i) ? "->" : "",
			offset + i * 4,
			word,
			getL(0), getL(8), getL(16), getL(24));
	}
}

void iprintf(unsigned indent, const char *format, ...) __attribute__((format(printf, 2, 3)));

void iprintf(unsigned indent, const char *format, ...)
{
	for (unsigned i = 0; i < indent; i++)
		printf("\t");
	va_list args;
	va_start(args, format);
	vprintf(format, args);
	va_end(args);
}

bool PrintBlocks(unsigned indent, uint8_t *data, size_t size);
bool PrintBlock(unsigned indent, uint8_t **data);

// TODO make this less hacky
unsigned hwVersion = 0;

// Attempt to parse a single block with maxSize words
bool ParseSingleBlock(unsigned indent, uint8_t* blockBlob, uint32_t cookie, uint32_t size)
{
	switch (cookie)
	{
	case COOKIE("MPB1"):
	{
		Block_MPB1* block = reinterpret_cast<Block_MPB1*>(blockBlob);
		iprintf(indent, "\tunk1 = 0x%08x\n", block->unk1);
		iprintf(indent, "\tunk2 = 0x%08x\n", block->unk2);
		assert(block->unk2 == 0);

		PrintBlocks(indent + 1, blockBlob + sizeof(Block_MPB1),
			    size - sizeof(Block_MPB1));
	}
	break;
	case COOKIE("MBS2"):
	{
		Block_MBS2* block = reinterpret_cast<Block_MBS2*>(blockBlob);
		iprintf(indent, "\tversion = 0x%08x\n", block->version);
		PrintBlocks(indent + 1, blockBlob + sizeof(Block_MBS2),
			    size - sizeof(Block_MBS2));
	}
	break;
	case COOKIE("VEHW"):
	{
		Block_VEHW* block = reinterpret_cast<Block_VEHW*>(blockBlob);
		hwVersion = block->hwVersion;
		iprintf(indent, "hwVersion = %d\n", block->hwVersion);
		//assert(block->unk2 == 0xb);
		//assert(block->unk3 == 0x0);
		//assert(block->unk4 == 0x0);
	}
	break;
	case COOKIE("VERT"):
	case COOKIE("FRAG"):
	case COOKIE("COMP"):
	case COOKIE("CVER"):
	case COOKIE("CGEO"):
		PrintBlocks(indent + 1, blockBlob, size);
		break;
	case COOKIE("CMMN"):
	{
		assert(*reinterpret_cast<uint32_t*>(blockBlob) == COOKIE("VELA"));
		PrintBlock(indent + 1, &blockBlob);

		// symbol tables?
		for (int i = 0; i < 6; i++) {
			assert(*reinterpret_cast<uint32_t*>(blockBlob) == COOKIE("SSYM"));
			PrintBlock(indent + 1, &blockBlob);
		}
		assert(*reinterpret_cast<uint32_t*>(blockBlob) == COOKIE("UBUF"));
		PrintBlock(indent + 1, &blockBlob);

		uint32_t numBinaries = *reinterpret_cast<uint32_t*>(blockBlob);
		iprintf(indent, "\tbinaries = %u\n", numBinaries);
		blockBlob += sizeof(uint32_t);

		for (unsigned i = 0; i < numBinaries; i++)
		{
			assert(*reinterpret_cast<uint32_t*>(blockBlob) == COOKIE("EBIN"));
			PrintBlock(indent + 1, &blockBlob);
		}
	}
	break;
	case COOKIE("VELA"):
	{
		Block_VELA* block = reinterpret_cast<Block_VELA*>(blockBlob);
		iprintf(indent, "\tunk2 = 0x%08x\n", block->unk2);
		//assert(block->unk2 == 0x8);
	}
	break;
	case COOKIE("SSYM"):
	{
		Block_SSYM* block = reinterpret_cast<Block_SSYM*>(blockBlob);
		iprintf(indent, "\tnumSymbols = 0x%08x\n", block->numSymbols);
		blockBlob += sizeof(uint32_t);
		for (unsigned i = 0; i < block->numSymbols; i++)
		{
			assert(*reinterpret_cast<uint32_t*>(blockBlob) == COOKIE("SYMB"));
			PrintBlock(indent + 1, &blockBlob);
		}

	}
	break;
	case COOKIE("SYMB"):
	{
		// First, the name
		assert(*reinterpret_cast<uint32_t*>(blockBlob) == COOKIE("STRI"));
		PrintBlock(indent + 1, &blockBlob);

		// Then, 12 bytes
		Block_SYMB_pt1 *pt1 = reinterpret_cast<Block_SYMB_pt1*>(blockBlob);
		iprintf(indent + 1, "unk1 = 0x%08x\n", pt1->unk1);
		iprintf(indent + 1, "unk2 = 0x%08x\n", pt1->unk2);
		iprintf(indent + 1, "unk3 = 0x%08x\n", pt1->unk3);
		blockBlob += sizeof(Block_SYMB_pt1);

		// Then, the type
		assert(*reinterpret_cast<uint32_t*>(blockBlob) == COOKIE("TYPE"));
		PrintBlock(indent + 1, &blockBlob);

		// Then, any relocs
		unsigned numRelocs = *reinterpret_cast<uint32_t*>(blockBlob);
		blockBlob += sizeof(uint32_t);
		for (unsigned i = 0; i < numRelocs; i++)
		{
			assert(*reinterpret_cast<uint32_t*>(blockBlob) == COOKIE("RLOC"));
			PrintBlock(indent + 1, &blockBlob);
		}
		
		// Finally, 8 more bytes
		Block_SYMB_pt2 *pt2 = reinterpret_cast<Block_SYMB_pt2*>(blockBlob);
		iprintf(indent + 1, "unk4 = 0x%08x\n", pt2->unk4);
		iprintf(indent + 1, "unk5 = 0x%08x\n", pt2->unk5);
		blockBlob += sizeof(Block_SYMB_pt2);
	}
	break;
	case COOKIE("STRI"):
	{
		// block.size includes the word aligned zero padding for the string
		char* name = reinterpret_cast<char*>(blockBlob);
		iprintf(indent + 1, "%s\n", name);
	}
	break;
	case COOKIE("TYPE"):
	{
		PrintBlock(indent + 1, &blockBlob);
	}
	break;
	case COOKIE("TPGE"):
	{
		Block_TPGE* block = reinterpret_cast<Block_TPGE*>(blockBlob);
		switch (block->type) {
			case 1: iprintf(indent, "\ttype = float\n"); break;
			case 2: iprintf(indent, "\ttype = int\n"); break;
			case 3: iprintf(indent, "\ttype = uint\n"); break;
			case 4: iprintf(indent, "\ttype = bool\n"); break;
			default: iprintf(indent, "\ttype = %d\n", block->type); break;
		}
		iprintf(indent, "\tcomponents = %d\n", block->components);
		switch (block->precision) {
			case 1: iprintf(indent, "\tprecision = highp\n"); break;
			case 2: iprintf(indent, "\tprecision = mediump\n"); break;
			case 3: iprintf(indent, "\tprecision = lowp\n"); break;
			default: iprintf(indent, "\tprecision = %d\n", block->precision); break;
		}
		switch (block->bitSize) {
			case 1: iprintf(indent, "\tbit size = 16-bit\n"); break;
			case 2: iprintf(indent, "\tbit size = 32-bit\n"); break;
			default: iprintf(indent, "\tbit size = %d\n", block->precision); break;
		}
		iprintf(indent, "\ttotal size = %d bytes\n", block->totalSize);
		iprintf(indent, "\tunk5 = 0x%08x\n", block->unk5);
	}
	break;
	case COOKIE("TPIB"):
	{
		Block_TPIB* block = reinterpret_cast<Block_TPIB*>(blockBlob);
		iprintf(indent, "\tunk2 = 0x%08x\n", block->unk2);
		iprintf(indent, "\tunk3 = 0x%08x\n", block->unk3);
		iprintf(indent, "\tunk4 = 0x%08x\n", block->unk4);

		// XXX: Sometimes different
		//assert(block->unk2 == 0x304);
		//assert(block->unk3 == 0x2);
		//assert(block->unk4 == 0x2);
	}
	break;
	case COOKIE("TPSE"):
		break;
	case COOKIE("TPAR"):
	{
		Block_TPAR* block = reinterpret_cast<Block_TPAR*>(blockBlob);
		iprintf(indent, "\tlen = %d\n", block->len);
		blockBlob += sizeof(Block_TPAR);
		PrintBlock(indent + 1, &blockBlob);
	}
	break;
	case COOKIE("UBUF"):
	{
		Block_UBUF* block = reinterpret_cast<Block_UBUF*>(blockBlob);
		iprintf(indent, "\tunk2 = 0x%08x\n", block->unk2);


		// XXX: sometimes different
		//assert(block->unk2 == 0x0);

		// XXX: unk2...?
	}
	break;
	case COOKIE("EBIN"):
	{
		uint8_t *end = blockBlob + size;
		Block_EBIN* block = reinterpret_cast<Block_EBIN*>(blockBlob);
		iprintf(indent, "\tunk2 = 0x%08x\n", block->unk2);
		iprintf(indent, "\tunk3 = 0x%08x\n", block->unk3);
		iprintf(indent, "\trelocs = %u\n", block->numRelocs);
		iprintf(indent, "\tunk5 = 0x%08x\n", block->unk5);

		// XXX: Sometimes different
		//assert(block->unk1 == 0xd4);
		//assert(block->unk2 == 0x0);
		assert(block->unk3 == ~0U);
		//assert(block->unk4 == 0x0);
		assert(block->unk5 == 0x0);
		//assert(block->unk6 == ~0U);

		blockBlob += sizeof(Block_EBIN);

		for (unsigned i = 0; i < block->numRelocs; i++)
		{
			assert(*reinterpret_cast<uint32_t*>(blockBlob) == COOKIE("RLOC"));
			PrintBlock(indent + 1, &blockBlob);
		}

		uint32_t unk6 = *reinterpret_cast<uint32_t*>(blockBlob);
		iprintf(indent, "\tunk6 = 0x%08x\n", unk6);
		blockBlob += sizeof(uint32_t);

		PrintBlocks(indent + 1, blockBlob, end - blockBlob);

	}
	break;
	case COOKIE("FSHA"):
	{
		Block_FSHA* block = reinterpret_cast<Block_FSHA*>(blockBlob);
		iprintf(indent, "\tunk2 = 0x%08x\n", block->unk2);
		iprintf(indent, "\tunk3 = 0x%08x\n", block->unk3);
		iprintf(indent, "\tunk4 = 0x%08x\n", block->unk4);
		iprintf(indent, "\tunk5 = 0x%08x\n", block->unk5);
		iprintf(indent, "\tunk6 = 0x%08x\n", block->unk6);
		iprintf(indent, "\tunk7 = 0x%08x\n", block->unk7);

		// XXX:Sometimes different
		// Probably a bitfield
		//assert(block->unk6 == 0x0);
		assert(block->unk7 == 0x0);
	}
	break;
	case COOKIE("BFRE"):
	{
		Block_BFRE* block = reinterpret_cast<Block_BFRE*>(blockBlob);
		iprintf(indent, "\tunk2 = 0x%08x\n", block->unk2);

		// XXX: Sometimes different
		// Probably a bitfield
		assert(block->unk2 == 0x0);
	}
	break;
	case COOKIE("SPDv"):
	{
		Block_SPDv* block = reinterpret_cast<Block_SPDv*>(blockBlob);
		iprintf(indent, "\tunk2 = 0x%08x\n", block->unk2);

		assert(block->unk2 == 0x0);
	}
	break;
	case COOKIE("SPDf"):
	{
		Block_SPDf* block = reinterpret_cast<Block_SPDf*>(blockBlob);
		iprintf(indent, "\tunk2 = 0x%08x\n", block->unk2);
		iprintf(indent, "\tunk3 = 0x%08x\n", block->unk3);

		assert(block->unk2 == 0x0080003e);
		assert(block->unk3 == 0x0);
	}
	break;
	case COOKIE("SPDc"):
	{
		Block_SPDc* block = reinterpret_cast<Block_SPDc*>(blockBlob);
		iprintf(indent, "\tunk2 = 0x%08x\n", block->unk2);

		assert(block->unk2 == 0x0);
	}
	break;

	case COOKIE("OBJC"):
	{
		switch (hwVersion) {
			case 1: // T600
			case 5: // T760
			case 7: // T880
			case 8: // T860
			case 10: // T830
				DisassembleMidgard(blockBlob, size);
				break;
			case 11: // G71
				DisassembleBifrost(blockBlob, size);
				break;
		}
	}
	break;
	case COOKIE("CCOM"):
	case COOKIE("CFRA"):
	{
		PrintBlocks(indent + 1, blockBlob, size);
	}
	break;
	case COOKIE("BATT"):
	{
		Block_BATT* block = reinterpret_cast<Block_BATT*>(blockBlob);
		iprintf(indent, "\tunk2 = 0x%08x\n", block->unk2);

		assert(block->unk2 == 0x2);
	}
	break;
	case COOKIE("KERN"):
		break;
	case COOKIE("KWGS"):
		break;
	case COOKIE("RLOC"):
	{
		Block_RLOC* block = reinterpret_cast<Block_RLOC*>(blockBlob);
		iprintf(indent, "\tlocation = %u\n", block->location);
		iprintf(indent, "\tunk3 = 0x%08x\n", block->unk3);

		//assert(block->unk2 == 0x0);
		//assert(block->unk3 == 0x0);
		//assert(block->unk4 == 0x0);
		//assert(block->unk5 == 0x8);
		// XXX: Sometimes different
	}
	break;
	case COOKIE("FOTV"):
	{
		// output variables
		uint32_t numVariables = *reinterpret_cast<uint32_t*>(blockBlob);
		blockBlob += sizeof(uint32_t);
		iprintf(indent, "\tvariables = %u\n", numVariables);

		for (unsigned i = 0; i < numVariables; i++)
		{
			assert(*reinterpret_cast<uint32_t*>(blockBlob) == COOKIE("OUTV"));
			PrintBlock(indent + 1, &blockBlob);
		}
	}
	case COOKIE("OUTV"):
		//TODO
		break;
	case COOKIE("AINF"):
		//TODO
		break;
	case COOKIE("VLKN"):
		// TODO vulkan stuff
		break;

	default:
	{
		auto getL = [cookie](int off)
		{
			return (uint8_t)(cookie >> off);
		};
		printf("Attempting 0x%08x: %c%c%c%c\n",
			cookie,
			getL(0), getL(8), getL(16), getL(24));
		return false;
	}
	break;
	};

	return true;
}

bool PrintBlock(unsigned indent, uint8_t **data)
{
	Header *hdr = reinterpret_cast<Header*>(*data);
	auto getL = [hdr](int off)
	{
		return (uint8_t)(hdr->cookie >> off);
	};
	iprintf(indent, "%c%c%c%c\n", getL(0), getL(8), getL(16), getL(24));
	iprintf(indent, "\tsize = 0x%08x\n", hdr->size);
	if (!ParseSingleBlock(indent, *data + sizeof(Header), hdr->cookie, hdr->size))
	{
		printf("Couldn't parse block! Leaving!\n");
		return false;
	}

	*data += hdr->size + sizeof(Header);
	return true;
}

bool PrintBlocks(unsigned indent, uint8_t *data, size_t size)
{
	for (uint8_t *tmp = data; tmp < data + size;)
	{
		if (!PrintBlock(indent, &tmp))
			return false;
	}

	return true;
}

int main(int argc, char** argv)
{
	if (argc < 2)
	{
		printf("Usage: %s <file.bin>\n", argv[0]);
		return 0;
	}

	std::vector<uint8_t> file;
	if (ReadFile(&file, argv[1]))
	{
		//PrintFile(&file);
		PrintBlocks(0, file.data(), file.size());
	}

	return 0;
}
