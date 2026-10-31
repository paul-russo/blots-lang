local function make(d)
  if d == 0 then return {} end
  return { l = make(d - 1), r = make(d - 1) }
end
local function check(t)
  if t.l == nil then return 1 end
  return 1 + check(t.l) + check(t.r)
end
local total = 0
for i = 1, 20 do total = total + check(make(14)) end
print(total)
