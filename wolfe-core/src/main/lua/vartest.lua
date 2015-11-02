--
-- Created by IntelliJ IDEA.
-- User: riedel
-- Date: 02/11/15
-- Time: 22:27
-- To change this template use File | Settings | File Templates.
--

dofile("var.lua")

v = Var(2,2)()
sig = nn.Sigmoid()(v)
g = nn.gModule({v},{sig})
ret = g:forward({})
print(ret)
ret = g:backward({}, torch.ones(2,2))
--
print(v.data.module.gradWeight)

lin = nn.Linear(2,2)()
siglin = nn.Sigmoid()(lin)
gLin = nn.gModule({lin},{siglin})

lin.data.module.bias = torch.Tensor(2)
lin.data.module.weight = v.data.module.weight

print(gLin:forward(torch.diag(torch.ones(2))))
gLin:backward(torch.diag(torch.ones(2)),torch.ones(2,2))
print(lin.data.module.gradWeight)






