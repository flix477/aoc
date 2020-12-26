#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const int PREAMBLE_LENGTH = 25;

int check_number(unsigned long long x, unsigned long long* xs, size_t from) {
  size_t to = from + PREAMBLE_LENGTH;
  for (int i = from; i < to; ++i) {
    for (int j = i + 1; j < to; ++j) {
      if (xs[i] + xs[j] == x)
        return 1;
    }
  }

  return 0;
}

unsigned long long get_number(char* line, size_t len, FILE* fp) {
  fgets(line, len, fp);
  unsigned long long x = atoll(line);
  memset(line, 0, len);
  return x;
}

unsigned long long solve1(char* path) {
  FILE* fp = fopen(path, "r");
  if (fp == NULL)
    return 0;

  char line[32];
  size_t len = 32;
  memset(line, 0, len);

  unsigned long long xs[PREAMBLE_LENGTH];
  unsigned char queueEnd = 0;
  unsigned long long x;

  for (int i = 0; i < PREAMBLE_LENGTH; ++i)
    xs[i] = get_number(line, len, fp);

  x = get_number(line, len, fp);

  int result = check_number(x, xs, 0);
  if (!result)
    return x;

  while (!feof(fp)) {
    xs[queueEnd] = x;
    queueEnd = (queueEnd + 1) % PREAMBLE_LENGTH;
    x = get_number(line, len, fp);
    if (!check_number(x, xs, 0))
      return x;
  }

  return 0;
}

unsigned long long solve2(char* path) {
  FILE* fp = fopen(path, "r");
  if (fp == NULL)
    return 0;

  unsigned long long xs[1024];
  size_t count = 0;
  char line[32];
  size_t len = 32;
  memset(line, 0, len);

  char found = 0;
  unsigned long long invalid;

  while (!feof(fp)) {
    unsigned long long x = get_number(line, len, fp);
    if (x == 0) break;
    if (!found && count >= PREAMBLE_LENGTH && !check_number(x, xs, count - PREAMBLE_LENGTH)) {
      found = 1;
      invalid = x;
    }
    xs[count++] = x;
  }

  if (!found) return 0;

  unsigned long long sum = xs[0] + xs[1];
  size_t from = 0;
  size_t to = 2;
  
  while (sum != invalid && from < count && to < count + 1) {
    if (sum < invalid) {
      unsigned long long x = xs[to++];
      if (x == invalid) break;
      sum += x;
    } else if (sum > invalid)
      sum -= xs[from++];
  }

  if (sum != invalid) return 0;

  unsigned long long min = ULLONG_MAX;
  unsigned long long max = 0;

  for (int i = from; i < to; ++i) {
    unsigned long long x = xs[i];
    if (x < min) min = x;
    if (x > max) max = x;
  }

  return min + max;
}

int main() {
  printf("First invalid number: %lld\n", solve1("input.txt"));
  printf("Encryption weakness: %lld\n", solve2("input.txt"));
}
