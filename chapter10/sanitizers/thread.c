#include <pthread.h>

int data = 0;

void *producer(void *x) {
  for (int i = 0; i < 10000; ++i) ++data ;
  return x;
}

void *consumer(void *x) {
  for (int i = 0; i < 10000; ++i) --data;
  return x;
}

int main() {
  pthread_t t1, t2;
  pthread_create(&t1, NULL, producer, NULL);
  pthread_create(&t2, NULL, consumer, NULL);
  pthread_join(t1, NULL);
  pthread_join(t2, NULL);
  return data;
}
