#include "Disasm.h"
#include <stdio.h>

void DumpInstructions(unsigned indent, uint8_t* instBlob, uint32_t size)
{
	uint8_t* instEnd = instBlob + size;
	while (instBlob != instEnd)
	{
		uint32_t *words = (uint32_t*) instBlob;
		uint8_t tag = words[0] & 0xff;
		if (tag & 0b00001000)
			printf("{\n");
		printf("# ");
		for (int i = 0; i < 4; i++)
			printf("%08x ", words[3 - i]); // low bit on the right
		printf("\n");
		if (tag & 0b01000000)
			printf("}\n");
		instBlob += 16;
	}
}

