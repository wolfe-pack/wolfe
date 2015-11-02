--
-- Created by IntelliJ IDEA.
-- User: riedel
-- Date: 02/11/15
-- Time: 18:44
-- To change this template use File | Settings | File Templates.
--
local nn = require "nn"
require "nngraph"
local Var, parent = torch.class('Var', 'nn.Module')

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
    self.output = self.weight
    --    if input:dim() == 1 then
    --        self.output:resize(self.weight:size(2))
    --        self.output:addmv(1, self.weight, input)
    --    elseif input:dim() == 2 then
    --        local nframe = input:size(1)
    --        local nElement = self.output:nElement()
    --        self.output:resize(nframe, self.bias:size(1))
    --        if self.output:nElement() ~= nElement then
    --            self.output:zero()
    --        end
    --        self.addBuffer = self.addBuffer or input.new()
    --        if self.addBuffer:nElement() ~= nframe then
    --            self.addBuffer:resize(nframe):fill(1)
    --        end
    --        self.output:addmm(0, self.output, 1, input, self.weight:t())
    --    else
    --        error('input must be vector or matrix')
    --    end

    return self.output
end

function Var:updateGradInput(input, gradOutput)
    if self.gradInput then

        --        local nElement = self.gradInput:nElement()
        --        self.gradInput:resizeAs(input)
        --        if self.gradInput:nElement() ~= nElement then
        --            self.gradInput:zero()
        --        end
        --        if input:dim() == 1 then
        --            self.gradInput:addmv(0, 1, self.weight:t(), gradOutput)
        --        elseif input:dim() == 2 then
        --            self.gradInput:addmm(0, 1, gradOutput, self.weight)
        --        end

        self.gradInput = torch.Tensor()
        return self.gradInput
    end
end

function Var:accGradParameters(input, gradOutput, scale)
    scale = scale or 1
    --    if input:dim() == 1 then
    --        self.gradWeight:addr(scale, gradOutput, input)
    --- -        self.gradBias:add(scale, gradOutput)
    -- elseif input:dim() == 2 then

    self.gradWeight:add(scale, gradOutput)
    --self.gradWeight = gradOutput
    --self.gradWeight:addmm(scale, gradOutput:t(), input)
    --        self.gradBias:addmv(scale, gradOutput:t(), self.addBuffer)
    --    end
end

-- we do not need to accumulate parameters when sharing
Var.sharedAccUpdateGradParameters = Var.accUpdateGradParameters


function Var:__tostring__()
    return torch.type(self) ..
            string.format('(%d -> %d)', self.weight:size(2), self.weight:size(1))
end


