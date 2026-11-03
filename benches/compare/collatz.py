def steps(n):
    acc = 0
    while n != 1:
        n = n // 2 if n % 2 == 0 else 3 * n + 1
        acc += 1
    return acc
print(max(steps(n) for n in range(1, 100000)))
