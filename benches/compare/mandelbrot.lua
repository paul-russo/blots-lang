local n = 300
local total = 0
for k = 0, n * n - 1 do
  local cr = (k % n) / n * 3 - 2
  local ci = (k // n) / n * 2 - 1
  local zr, zi, i = 0.0, 0.0, 0
  while i < 50 and zr * zr + zi * zi <= 4 do
    zr, zi = zr * zr - zi * zi + cr, 2 * zr * zi + ci
    i = i + 1
  end
  total = total + i
end
print(total)
