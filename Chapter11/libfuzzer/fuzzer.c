#include <stdint.h>
#include <stdlib.h>

int count(const uint8_t *Data, size_t Size ) {
  int cnt = 0;
  if (Size)
    while (Data[cnt] >= '0' && Data[cnt] <= '9') ++cnt;
  return cnt;
}

int LLVMFuzzerTestOneInput(const uint8_t *Data, size_t Size) {
  count(Data, Size);
  return 0;
}
