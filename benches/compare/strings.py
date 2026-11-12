parts = ["item-" + str(i) for i in range(300000)]
total = 0
for s in parts:
    total += len(s)
print(total + len(",".join(parts)))
