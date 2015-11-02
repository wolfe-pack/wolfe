--
-- Created by IntelliJ IDEA.
-- User: riedel
-- Date: 02/11/15
-- Time: 18:44
-- To change this template use File | Settings | File Templates.
--
local Var, parent = torch.class('nn.Var', 'nn.Module')

function Var:__init(inputSize, outputSize)
    parent.__init(self)

    self.weight = torch.Tensor(outputSize, inputSize)
    self.gradWeight = torch.Tensor(outputSize, inputSize)

    self:reset()
end

function Var:reset(stdv)
    if stdv then
        stdv = stdv * math.sqrt(3)
    else
        stdv = 1. / math.sqrt(self.weight:size(2))
    end
    if nn.oldSeed then
        for i = 1, self.weight:size(1) do
            self.weight:select(1, i):apply(function()
                return torch.uniform(-stdv, stdv)
            end)
        end
    else
        self.weight:uniform(-stdv, stdv)
    end

    return self
end

function Var:updateOutput(input)
    self.output:copy(self.weight)
    return self.output
end

function Var:updateGradInput(input, gradOutput)
end

function Var:accGradParameters(input, gradOutput, scale)
    scale = scale or 1
    self.gradWeight:add(scale, gradOutput)
--    self.gradWeight:addmm(scale, gradOutput:t(), input)
end

-- we do not need to accumulate parameters when sharing
Var.sharedAccUpdateGradParameters = Var.accUpdateGradParameters


function Var:__tostring__()
    return torch.type(self) ..
            string.format('Var(%d x %d)', self.weight:size(2), self.weight:size(1))
end

