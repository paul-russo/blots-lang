local function make_adder(n) return function(x) return x + n end end
local adders = {}
for i = 0, 999 do adders[i + 1] = make_adder(i) end
local acc = 0
for i = 0, 1999 do
  local a = 0
  for j = 1, #adders do a = a + adders[j](i) end
  acc = acc + a
end
print(acc)
