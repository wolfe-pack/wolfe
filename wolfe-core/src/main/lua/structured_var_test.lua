--
-- Created by IntelliJ IDEA.
-- User: riedel
-- Date: 02/11/15
-- Time: 22:27
-- To change this template use File | Settings | File Templates.
--

require "wolfe"

-- dofile("struct_var.lua")

v = wolfe.StructVar({torch.LongStorage({1,1}), torch.LongStorage({2,2})})()
sig = nn.Sigmoid()(nn.SelectTable(2)(v))
g = nn.gModule({v},{sig})
ret = g:forward({})
print(ret)
ret = g:backward({}, torch.ones(2,2))
print(v.data.module.gradWeight[2])

--

--
--id = nn.Identity()()
--
--
--print(v.data.module.gradWeight)
--
lin = nn.Linear(2,2)()
siglin = nn.Sigmoid()(lin)
gLin = nn.gModule({lin},{siglin})

lin.data.module.bias = torch.Tensor(2)
lin.data.module.weight = v.data.module.weight[2]

print(gLin:forward(torch.diag(torch.ones(2))))
gLin:backward(torch.diag(torch.ones(2)),torch.ones(2,2))
print(lin.data.module.gradWeight)






