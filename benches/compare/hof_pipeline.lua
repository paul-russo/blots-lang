local function map(xs, f)
  local out = {}
  for i = 1, #xs do out[i] = f(xs[i]) end
  return out
end
local function filter(xs, f)
  local out, n = {}, 0
  for i = 1, #xs do
    local x = xs[i]
    if f(x) then n = n + 1; out[n] = x end
  end
  return out
end
local function reduce(xs, f, acc)
  for i = 1, #xs do acc = f(acc, xs[i]) end
  return acc
end
local xs = {}
for i = 0, 999999 do xs[i + 1] = i end
local doubled = map(xs, function(x) return x * 2 + 1 end)
local kept = filter(doubled, function(x) return x % 3 == 0 end)
print(reduce(kept, function(acc, x) return acc + x end, 0))
