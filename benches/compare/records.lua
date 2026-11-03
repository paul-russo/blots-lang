local points = {}
for i = 0, 399999 do points[i + 1] = { x = i, y = i * 2, z = i % 7 } end
local acc = 0
for i = 1, #points do
  local p = points[i]
  acc = acc + p.x + p.y * p.z
end
print(acc)
