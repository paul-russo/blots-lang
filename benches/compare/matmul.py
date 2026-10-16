n = 160
a = [[i + j for j in range(n)] for i in range(n)]
b = [[i - j for j in range(n)] for i in range(n)]
c = [[sum(a[i][k] * b[k][j] for k in range(n)) for j in range(n)] for i in range(n)]
print(sum(sum(row) for row in c))
