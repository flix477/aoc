xs = [
    (i, x, 0)
    for i, x in enumerate([int(x) if x != "x" else None for x in "7,13,x,x,59,x,31,19".split(",")])
    if x
]

while True:
    xs = [(i, id, t + id) for i, id, t in xs]
    t = xs[0][2]
    if all(xt == t + xi for xi, _, xt in xs):
        break

print("completed")
print(xs)
