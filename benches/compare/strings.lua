local parts = {}
for i = 0, 299999 do parts[i + 1] = "item-" .. tostring(i) end
local total = 0
for i = 1, #parts do total = total + #parts[i] end
print(total + #table.concat(parts, ","))
