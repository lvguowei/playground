#include <stdio.h>
#include <stdlib.h>

typedef struct words {
  const char *word;
  int hitCount;
} words;

struct words output[] = {{"hello", 314}, {"world", 42}, {"answer", 42}};

int compare(const void *left, const void *right) {
  return 1;
}

int main() {
  qsort(output, 2, sizeof(words), compare);
  for (int i = 0; i < 2; ++i) {
    printf("%d %s\n", output[i].hitCount, output[i].word);
  }

  return 0;
}
