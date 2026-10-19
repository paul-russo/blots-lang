local xs = {}
for i = 0, 399999 do xs[i + 1] = (i * 7919 + 13) % 100003 end
table.sort(xs)
print(xs[1] + xs[200001] + xs[400000])
