#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>


void *new_thread(void *args) {
  printf("New thread starting.  Running infinite loop\n");

  volatile int i = 0;

  while (1) {
    i = (((i * 11) / 7) << 1) >> 1;
  }
}

int main(int argc, char *argv[]) {
  int ret;
  int i;
  int num_threads;

  if (argc != 2) {
    printf("Usage: cpu_load <num_threads>\n");
    exit(-1);
  }

  num_threads = atoi(argv[1]);

  pthread_t *threads = malloc(sizeof(pthread_t) * num_threads);

  for (i = 0; i < num_threads; i++) {
    pthread_create(&threads[i], NULL, new_thread, NULL);
  }

  for (i = 0; i < num_threads; i++) {
    pthread_join(threads[i], NULL);
  }

  return 0;
}
