#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* size_t my_strlen(const char *str) { */
/*   size_t n = 0; */
/*   while (*str++) { */
/*     n++; */
/*   } */
/*   return n; */
/* } */

/* size_t my_strlen(const char *s) { */
/*   const char *p = s; */
/*   while (*s) { */
/*     s++; */
/*   } */
/*   return s - p; */
/* } */

char *my_strcat(char *s1, const char *s2) {
  char *p = s1;

  while (*p) {
    p++;
  }

  while (*s2) {
    *p = *s2;
    p++;
    s2++;
  }
  *p = '\0';
  return s1;
}

int main() {
  char s1[20] = "hello";
  printf("%s\n", my_strcat(s1, " world"));
}
