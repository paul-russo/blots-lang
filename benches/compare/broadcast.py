n = 2000000
a = 0
b = 0
for i in range(n):
    a += (i * 2 + 1) / 3
    b += i * i % 7
print(a + b)
