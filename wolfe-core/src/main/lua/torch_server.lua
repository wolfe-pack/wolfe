-- Torch interface to Julia (server)
local json = require("dkjson")
local zmq = require "lzmq"
local context = zmq.init(1)

require("wolfe")

local socket = context:socket(zmq.REP)
socket:bind("tcp://*:7000")

function serialize(data)
    if torch.type(1) == torch.type(data) then
        return { data }
    elseif torch.type(torch.Tensor(1)) == torch.type(data) then
        --for now it assumes data is a 1-D array
        local dims = {}
        local size = data:size()
        for i = 1, size:size() do
            dims[i] = size[i]
        end
        local storage = {}
        local rawStorage = data:storage()
        for i = 1, rawStorage:size() do
            storage[i] = rawStorage[i]
        end
        return { dims = dims, storage = storage, _datatype = "tensor" }
    elseif type(data) == "table" then
        local result = {}
        for k, v in pairs(data) do
            result[k] = serialize(v)
        end
        return result
    else
        return data
    end
end

function deserialize(data)
    if type(data) == "table" and data["_datatype"] == "tensor" then
        local dims = data["dims"]
        local storage = torch.Storage(data["storage"])
        local dimsStorage = torch.LongStorage(table.getn(dims))
        for k, v in pairs(dims) do
            dimsStorage[k] = v
        end
        local result = torch.Tensor(storage, 1, dimsStorage)
        return result
    elseif type(data) == "table" then
        local result = {}
        for k, v in pairs(data) do
            result[k] = deserialize(v)
        end
        return result
    else
        return data
    end
end

function getFunc(parent, path, index)
    if (path[index] == nil) then
        return parent
    else
        return getFunc(parent[path[index]], path, index + 1)
    end
end

while true do
    --  Wait for next request from client
    local request = socket:recv()
    -- print("Received Hello [" .. request .. "]")
    -- print(request)
    if request ~= nil then
        request = json.decode(request, 1, nil)
        --        print('req:', request, ' | cmd:', request.cmd)
        if request.cmd == "load" then
            require(request.name)
            local ret = { 1 }
            ret = json.encode(ret, { indent = true })
            socket:send(ret)
        elseif request.cmd == "call" then
            local func_name = request.msg.func
            local args = request.msg.args
            for k, v in pairs(args) do
                args[k] = deserialize(v)
            end
            local func = getFunc(_G, func_name, 1)
            --            print(func_name)
            --            print(func)
            --            print(args)
            --            local status, ret = pcall(function() func(unpack(args)) end)
            local status, ret = pcall(func, unpack(args))
            print(status)
            print(ret)
            -- ret = torch.rand(10):float()
            ret = serialize(ret)
            ret = json.encode(ret, { indent = true })
            socket:send(ret)
        end
    end
end
--  We never get here but if we did, this would be how we end
socket:close()
context:term()



