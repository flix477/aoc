# https://adventofcode.com/2019/day/4

def solve(min, max):
  results = []
  next_value = str(min)
  while True:
    result = generate(next_value)
    if int(result) > max:
      return len(results)
    elif matches(result, min, max):
      results.append(result)
    next_value = str(int(result) + 1)

def generate(last_result):
  result = last_result
  last = 0
  for i, c in enumerate(result):
    value = int(c)
    if value < last:
      value = last
      result = result[:i] + str(last) + result[i+1:]
    last = value
  return result

def matches(password, min, max):
  return check_range(password, min, max) and \
         check_ascending(password)

def check_range(password, min, max):
  value = int(password)
  return value >= min and value <= max

def check_ascending(password):
  last = 0
  digits_count = dict()
  for c in password:
    value = int(c)
    if value > last:
      last = value
    elif value == last:
      digits_count[c] = digits_count[c] + 1 if c in digits_count else 2
    else:
      return False
  return check_duplicates(digits_count)

def check_duplicates(digits_count):
  for i in digits_count.values():
    if i == 2:
      return True
  return False

if __name__ == "__main__":
  print(solve(235741, 706948))
