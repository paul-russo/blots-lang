local n = 2000000
local a, b = 0, 0
for i = 0, n - 1 do
  a = a + (i * 2 + 1) / 3
  b = b + i * i % 7
end
print(a + b)
