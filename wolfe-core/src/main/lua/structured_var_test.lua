--
-- Created by IntelliJ IDEA.
-- User: riedel
-- Date: 02/11/15
-- Time: 22:27
-- To change this template use File | Settings | File Templates.
--

require "wolfe"

-- matrix factorization

local col = wolfe.ParamAccess({ 0, "?" })()
local row = wolfe.ParamAccess({ 0, "?" })()
local dot = nn.MM()({ col, row })
local loss = nn.Log()(nn.Sigmoid()(dot))
local mf = nn.gModule({ col, row }, { loss })

local dims = {
    { torch.LongStorage({ 1, 2 }), torch.LongStorage({ 1, 2 }) },
    { torch.LongStorage({ 2, 1 }), torch.LongStorage({ 2, 1 }) }
}

col.data.module:initWeight(dims)
col.data.module:shareWeight(row.data.module)

local weight = col.data.module.weight
weight[1][1] = torch.ones(1, 2):mul(0.0001)
--weight[2][1] = torch.ones(2, 1)

for i = 1, 10 do

    print("Iteration " .. i)
    local mfForward = mf:forward({ { 0 }, { 0 } })
    print(mfForward)

    mf:backward({ { 0 }, { 0 } }, torch.ones(1))

    print(row.data.module.gradWeight)
    print(col.data.module.gradWeight)

    col.data.module:updateParameters(-1.0)
    row.data.module:updateParameters(-1.0)

    print(weight[2][1])
    print(weight[1][1])

end



--
--    --
--
--v = wolfe.ParamAccess({ torch.LongStorage({ 2, 2 }), torch.LongStorage({ 2, 2 }) }, { "?" })()
--v.data.module:initWeights()
--sig = nn.Sigmoid()(v) --gradient should be 0.5 * 0.5 = 0.25
--g = nn.gModule({ v }, { sig })
--
--ret = g:forward({ 2 })
--print(ret)
--
--ret = g:backward({}, torch.ones(2, 2))
--print(ret)
--print("Gradient:")
--print(v.data.module.gradWeight)
--print(v.data.module.weight[2])
--v.data.module:updateParameters(1.0)
--print(v.data.module.weight[2])
--
--grad = wolfe.aggregateGradients({ v.data.module })
--
--print("Structured Gradient:")
--print(grad[2])
--print(grad[1])
--
--- - dofile("struct_var.lua")
--
-- v = wolfe.StructParam({ torch.LongStorage({ 1, 1 }), torch.LongStorage({ 2, 2 }) })()
-- sig = nn.Sigmoid()(nn.SelectTable(2)(v))
-- g = nn.gModule({ v }, { sig })
--
-- ret = g:forward({})
-- print(ret)
-- ret = g:backward({}, torch.ones(2, 2))
-- print(v.data.module.gradWeight[2])
--
----
--
----
---- id = nn.Identity()()
----
----
---- print(v.data.module.gradWeight)
----
-- lin = nn.Linear(2, 2)()
-- siglin = nn.Sigmoid()(lin)
-- gLin = nn.gModule({ lin }, { siglin })
--
-- lin.data.module.bias = torch.Tensor(2)
-- lin.data.module.weight = v.data.module.weight[2]
--
-- print(gLin:forward(torch.diag(torch.ones(2))))
-- gLin:backward(torch.diag(torch.ones(2)), torch.ones(2, 2))
-- print(lin.data.module.gradWeight)






