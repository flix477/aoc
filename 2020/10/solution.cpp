#include <algorithm>
#include <fstream>
#include <map>
#include <string>
#include <vector>

using namespace std;

std::vector<int> get_joltages(std::string path) {
  ifstream file(path);
  std::string line;
  std::vector<int> joltages;

  while (getline(file, line))
    joltages.push_back(std::stoi(line));

  sort(joltages.begin(), joltages.end());

  return joltages;
}

int solve1() {
  const auto joltages = get_joltages("input.txt");
  std::map<int, int> differences_count = {{3, 1}};
  int last = 0;
  for (const auto &joltage: joltages) {
    differences_count[joltage - last]++;
    last = joltage;
  }

  return differences_count[1] * differences_count[3];
}

unsigned long long solve2_cached(
  const std::vector<int> &joltages,
  std::map<size_t, unsigned long long> &cache,
  size_t i
) {
  if (i == joltages.size() - 1)
    return 1;

  if (cache.find(i) != cache.end())
    return cache[i];

  int joltage = joltages[i];
  unsigned long long count = 0;
  for (size_t j = i + 1; j < joltages.size() && joltages[j] - joltage <= 3; ++j) {
    unsigned long long next_count = solve2_cached(joltages, cache, j);
    cache[j] = next_count;
    count += next_count;
  }

  return count;
}

unsigned long long solve2() {
  auto joltages = get_joltages("input.txt");
  std::map<size_t, unsigned long long> cache;
  joltages.insert(joltages.begin(), 0);
  return solve2_cached(joltages, cache, 0);
}

int main() {
  printf("1-jolt differences multiplied by 3-jolt differences: %d\n", solve1());
  printf("Distinct ways to arrange the adapters: %lld\n", solve2());
  return 0;
}
