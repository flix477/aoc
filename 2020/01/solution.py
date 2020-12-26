with open("input.txt", "r") as f:
    lines = [int(l.strip(), 10) for l in f]


def solve2(lines):
    lines.sort()

    i = 0
    j = len(lines) - 1

    while True:
        a = lines[i]
        b = lines[j]

        sum = a + b
        if sum > 2020:
            j -= 1
        elif sum < 2020:
            i += 1
        else:
            return a * b


# naive
def solve3(lines):
    for i in range(len(lines)):
        a = lines[i]
        for j in range(i, len(lines)):
            b = lines[j]
            for k in range(j, len(lines)):
                c = lines[k]
                if a + b + c == 2020:
                    return a * b * c


print(solve2(lines))
print(solve3(lines))
