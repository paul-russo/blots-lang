xs = [(i * 7919 + 13) % 100003 for i in range(400000)]
xs.sort()
print(xs[0] + xs[200000] + xs[399999])
