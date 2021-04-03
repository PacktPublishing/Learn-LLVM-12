#include <unistd.h>

void func1() {
  usleep(10);
}

void func2(int n) {
  if (n % 2)
    func1();
  else
    usleep(100);
}

int main(int argc, char *argv[]) {
  for (int i = 0; i < 100; i++) { func1(); func2(i); }
  return 0;
}
