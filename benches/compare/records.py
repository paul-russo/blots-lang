points = [{"x": i, "y": i * 2, "z": i % 7} for i in range(400000)]
acc = 0
for p in points:
    acc += p["x"] + p["y"] * p["z"]
print(acc)
