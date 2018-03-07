#pragma once

#include <stdint.h>

void DisassembleBifrost(uint8_t* instBlob, uint32_t size);

#ifdef __cplusplus
extern "C" {
#endif

void DisassembleMidgard(uint8_t* instBlob, uint32_t size);

#ifdef __cplusplus
};
#endif
