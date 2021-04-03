#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[]) {
  char *p = malloc(12);
  memset(p, 0, 12);
  free(p);
  return (int)*p;
}
