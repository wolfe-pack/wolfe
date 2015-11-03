require 'nn'
require 'nngraph'

-- sigmoid(W * x + b)

linear0 = nn.Linear(2,2)()
sigm0 = nn.Sigmoid()(linear0)

-- create module that has linear0 as input, and sigm0 as output
gmod = nn.gModule({linear0}, {sigm0})

function initW(W)
    -- module.init(W := Tensor((1,2),(2,3))
    linear0.data.module.weight = W
end

function initb(b)
    -- module.init(b := Tensor(1,2)
    linear0.data.module.bias = b
end

function initTheta(theta)
    linear0.data.module.weight = theta[1]
    linear0.data.module.bias = theta[2]
end

function gradTheta()
    return {
        linear0.data.module.gradWeight,
        linear0.data.module.gradBias
    }
end

-- module.forward(x := Tensor(2,3))
function forward(x)
    gmod:forward(x)
end

function gradientW()
    return linear0.data.module.gradWeight
end

function gradientb()
    return linear0.data.module.gradBias
end


--  module.output()

output = gmod.output


