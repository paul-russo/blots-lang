local function steps(n)
  local acc = 0
  while n ~= 1 do
    if n % 2 == 0 then n = n / 2 else n = 3 * n + 1 end
    acc = acc + 1
  end
  return acc
end
local best = 0
for n = 1, 99999 do
  local s = steps(n)
  if s > best then best = s end
end
print(best)
