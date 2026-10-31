local n = 160
local a, b, c = {}, {}, {}
for i = 0, n - 1 do
  local ra, rb = {}, {}
  for j = 0, n - 1 do ra[j + 1] = i + j; rb[j + 1] = i - j end
  a[i + 1] = ra; b[i + 1] = rb
end
for i = 1, n do
  local row, ai = {}, a[i]
  for j = 1, n do
    local acc = 0
    for k = 1, n do acc = acc + ai[k] * b[k][j] end
    row[j] = acc
  end
  c[i] = row
end
local total = 0
for i = 1, n do
  local row = c[i]
  for j = 1, n do total = total + row[j] end
end
print(total)
