--
-- Created by IntelliJ IDEA.
-- User: riedel
-- Date: 02/11/15
-- Time: 18:44
-- To change this template use File | Settings | File Templates.
--
require "nn"
require "nngraph"

local StructVar, parent = torch.class('wolfe.StructVar', 'nn.Module')

function StructVar:__init(dims)
    parent.__init(self)

    self.weight = createNestedTable(dims)
    self.gradWeight = createNestedTable(dims)

    self:reset()
end


function createNestedTable(dims)
    if torch.type(dims) == "torch.LongStorage" then
        return torch.Tensor(dims)
    else
        local result = {}
        for k, v in pairs(dims) do
            result[k] = createNestedTable(v)
        end
        return result
    end
end

function resetNestedTable(data, stdv)
    if torch.type(data) == "torch.DoubleTensor" then
        if stdv then
            stdv = stdv * math.sqrt(3)
        else
            stdv =  0.1 --1. / math.sqrt(data:size(2))
        end
        if nn.oldSeed then
            for i = 1, data:size(1) do
                data:select(1, i):apply(function()
                    return torch.uniform(-stdv, stdv)
                end)
            end
        else
            data:uniform(-stdv, stdv)
        end
    else
        for _, v in pairs(data) do
            resetNestedTable(v, stdv)
        end
    end
end

function StructVar:reset(stdv)
    resetNestedTable(self.weight, stdv)
end

function StructVar:updateOutput(input)
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

function StructVar:updateGradInput(input, gradOutput)
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


function addNestedTable(target, scale, toAdd)
    if torch.type(target) == "torch.DoubleTensor" then
        target:add(scale,toAdd)
    else
        for k, v in pairs(target) do
            addNestedTable(v, scale, toAdd[k])
        end
    end
end

function StructVar:accGradParameters(input, gradOutput, scale)
    scale = scale or 1
    --    if input:dim() == 1 then
    --        self.gradWeight:addr(scale, gradOutput, input)
    --- -        self.gradBias:add(scale, gradOutput)
    -- elseif input:dim() == 2 then

    addNestedTable(self.gradWeight, scale, gradOutput)

--    self.gradWeight:add(scale, gradOutput)
    --self.gradWeight = gradOutput
    --self.gradWeight:addmm(scale, gradOutput:t(), input)
    --        self.gradBias:addmv(scale, gradOutput:t(), self.addBuffer)
    --    end
end

-- we do not need to accumulate parameters when sharing
StructVar.sharedAccUpdateGradParameters = StructVar.accUpdateGradParameters


function StructVar:__tostring__()
    return torch.type(self) ..
            "TODO"
end


