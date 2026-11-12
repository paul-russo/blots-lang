from functools import reduce
xs = range(1000000)
doubled = list(map(lambda x: x * 2 + 1, xs))
kept = list(filter(lambda x: x % 3 == 0, doubled))
print(reduce(lambda acc, x: acc + x, kept, 0))
