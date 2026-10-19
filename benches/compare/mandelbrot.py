n = 300
total = 0
for k in range(n * n):
    cr = (k % n) / n * 3 - 2
    ci = (k // n) / n * 2 - 1
    zr = zi = 0.0
    i = 0
    while i < 50 and zr * zr + zi * zi <= 4:
        zr, zi = zr * zr - zi * zi + cr, 2 * zr * zi + ci
        i += 1
    total += i
print(total)
