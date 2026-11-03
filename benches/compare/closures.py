def make_adder(n):
    return lambda x: x + n
adders = [make_adder(i) for i in range(1000)]
acc = 0
for i in range(2000):
    a = 0
    for f in adders:
        a += f(i)
    acc += a
print(acc)
