def make(d):
    if d == 0:
        return {"l": None, "r": None}
    return {"l": make(d - 1), "r": make(d - 1)}
def check(t):
    if t["l"] is None:
        return 1
    return 1 + check(t["l"]) + check(t["r"])
print(sum(check(make(14)) for _ in range(20)))
