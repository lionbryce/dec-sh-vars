dec_shvars = {}
dec_shvars.registeredVars = {}
dec_shvars.registeredVarsByID = {}
dec_shvars.storedVars = {}
dec_shvars.lastID = 0
dec_shvars.lastSecond = math.floor(CurTime())
dec_shvars.messagesSentThisSecond = 0


local IDBITS = 16 -- we could easily hit 255, if we ever hit 65535 we probably have a problem
local MAXID = 2 ^ IDBITS
local INSTANCEBITS = 16
local MAXINSTANCE = 2 ^ INSTANCEBITS

local function fp(tbl)
    local func = tbl[1]

    return function(...)
        local fnArgs = {}

        local arg = {...}

        local tblN = table.maxn(tbl)

        for i = 2, tblN do
            fnArgs[i - 1] = tbl[i]
        end

        for i = 1, table.maxn(arg) do
            fnArgs[tblN + i - 1] = arg[i]
        end

        return func(unpack(fnArgs, 1, table.maxn(fnArgs)))
    end
end

local function fnFlip(f)
    if not f then
        error("not a function")
    end

    return function(b, a, ...) return f(a, b, ...) end
end

local function getUIntInterface(v)
    return {fp{fnFlip(net.WriteInt), v}, fp{net.ReadInt, v}, "number", 0}
end

local interfaces_enum = {
    ["WRITE"] = 1,
    ["READ"] = 2,
    ["TYPE"] = 3,
}

--["DEFAULT"] = 4,
dec_shvars.interfaceTypes = {
    ["string"] = {net.WriteString, net.ReadString, "string", ""},
    ["uint32"] = getUIntInterface(32),
    ["uint16"] = getUIntInterface(16),
    ["uint8"] = getUIntInterface(8),
    ["bool"] = {net.WriteBool, net.ReadBool, "boolean", false},
    ["bit"] = {net.WriteBit, net.ReadBit, "number", 0},
    ["color"] = {net.WriteColor, net.ReadColor, "table", color_white},
    ["vector"] = {net.WriteVector, net.ReadVector, "Vector", Vector()},
}

local interfaces = dec_shvars.interfaceTypes

function dec_shvars.GetStruct(args)
    local args_len = #args
    assert(args_len > 0, "no arguments supplied")
    assert(args_len % 2 == 0, "varargs should be an even number")
    local out = {}

    for i = 1, args_len, 2 do
        local data_type = args[i]
        local data_name = args[i + 1]
        local data_interface = interfaces[data_type]
        assert(data_interface, "invalid interface " .. tostring(data_type or ""))

        out[#out + 1] = {
            write = data_interface[interfaces_enum.WRITE],
            read = data_interface[interfaces_enum.READ],
            type = data_interface[interfaces_enum.TYPE],
            --default = data_interface[interfaces_enum.DEFAULT], 
            name = data_name,
        }
    end

    return out
end

function dec_shvars.Register(name, structTable, instanced)
    local struct = dec_shvars.GetStruct(structTable)
    local lastID = dec_shvars.lastID + 1
    assert(lastID < MAXID, string.format("Trying to use impossible id: %s ; number of available bits: %s", lastID, IDBITS))
    dec_shvars.lastID = lastID
    struct.id = lastID
    struct.name = name
    struct.instanced = instanced
    dec_shvars.registeredVars[name] = struct
    dec_shvars.registeredVarsByID[lastID] = struct
end

function dec_shvars.GetRegistration(name)
    local registration = dec_shvars.registeredVars[name]
    assert(registration, "trying to use unregistered var: " .. tostring(name or ""))

    return registration
end

function dec_shvars.GetRegistrationByID(id)
    local registration = dec_shvars.registeredVarsByID[id]
    assert(registration, "trying to use unregistered var: " .. tostring(id or ""))

    return registration
end

dec_shvars.UpdateClients = CLIENT and function() end or function(name,ply,instanceid)
    dec_queues.addTask("dec_shvars", {name,ply,instanceid})
end

function dec_shvars.Set(name, ...)
    local registration = dec_shvars.GetRegistration(name)
    if registration.instanced then return dec_shvars.SetInstanced(name, ...) end

    local args = {...}

    assert(#args == #registration, string.format("incorrect number of args for dec_shvar %s, got %s, needed %s", name, #args, #registration))
    local keyed = {}

    for k, v in ipairs(args) do
        local reg_k = registration[k]
        local type_v = type(v)
        local type_needed = reg_k.type
        assert(type_v == type_needed, string.format("Unable to set, bad vararg at position %s, was type %s, needed %s", k, type_v, type_needed))
        keyed[reg_k.name] = v
    end

    local prev = dec_shvars.storedVars[name]
    local t = prev and prev.t or nil

    -- r for raw, k for keyed
    dec_shvars.storedVars[name] = {
        r = args,
        k = keyed,
        t = t
    }

    dec_shvars.UpdateClients(name)
end

function dec_shvars.SetInstanced(name, id, ...)
    local registration = dec_shvars.GetRegistration(name)

    if tonumber(id) then
        assert(id > 0 and id < MAXINSTANCE, "instanced ids must be greater than 0 and less than %s, received %s", MAXINSTANCE, id)
    end

    local args = {...}

    assert(#args == #registration, string.format("incorrect number of args for isntanced dec_shvar %s, got %s, needed %s", name, #args, #registration))
    local keyed = {}

    for k, v in ipairs(args) do
        local reg_k = registration[k]
        local type_v = type(v)
        local type_needed = reg_k.type
        assert(type_v == type_needed, string.format("Unable to set, bad vararg at position %s, was type %s, needed %s", k, type_v, type_needed))
        keyed[reg_k.name] = v
    end

    dec_shvars.storedVars[name] = dec_shvars.storedVars[name] or {}

    dec_shvars.storedVars[name][id] = {
        r = args,
        k = keyed
    }

    dec_shvars.UpdateClients(name,nil,id)
end

function dec_shvars.GetRaw(name, ...)
    local args = {...}

    local registration = dec_shvars.GetRegistration(name)

    if registration.instanced then
        local id = args[1]
        local stored = dec_shvars.storedVars[name]
        if id and stored and stored[id] then return stored[id].r end
        assert(#args - 1 == #registration, string.format("incorrect number of args for default instanced dec_shvar %s, got %s, needed %s", name, #args, #registration))

        return {unpack(args, 2)}
    else
        local stored = dec_shvars.storedVars[name]
        if stored then return stored.r end
        assert(#args == #registration, string.format("incorrect number of args for default dec_shvar %s, got %s, needed %s", name, #args, #registration))

        return args
    end
end

function dec_shvars.Get(name, ...)
    local args = {...}

    local stored = dec_shvars.storedVars[name]
    local registration = dec_shvars.GetRegistration(name)

    if registration.instanced then
        local id = args[1]
        assert(id, "attempting to get instanced variable missing id: " .. tostring(name))

        return stored and stored[id] and stored[id].k or args[2]
    else
        return stored and stored.k or args[2]
    end
end

dec_shvars.Register("ClanData", {"string", "name", "string", "tag", "color", "tagColor"}, true)
dec_shvars.Register("NotRandom", {"uint32", "value"})
dec_shvars.Register("NotRandom1", {"uint32", "value"})
dec_shvars.Register("NotRandom2", {"uint32", "value"})
dec_shvars.Register("NotRandom3", {"uint32", "value"})

if SERVER then
    util.AddNetworkString("dec_shvars")

    dec_queues.defineQueue("dec_shvars",16,16*8,function(d)
        dec_shvars.Notify(unpack(d))
    end)

    function dec_shvars.Notify(name, ply, instanceid)
        local registration = dec_shvars.GetRegistration(name)
        local instanced = registration.instanced

        if instanced then
            if instanceid then
                return dec_shvars.NotifyInstanced(name, ply, instanceid)
            else
                return dec_shvars.NotifyAllInstances(name, ply)
            end
        end

        local data = dec_shvars.GetRaw(name)
        local id = registration.id
        net.Start("dec_shvars")
		dec_shvars.WriteTimeData()
        net.WriteUInt(id, IDBITS)

        for i = 1, #registration do
            registration[i].write(data[i])
        end

        net.Send(ply or player.GetHumans())
    end

    function dec_shvars.NotifyInstanced(name, ply, instanceid)
        local registration = dec_shvars.GetRegistration(name)
        local data = dec_shvars.GetRaw(name, instanceid)
        local id = registration.id
        net.Start("dec_shvars")
		dec_shvars.WriteTimeData()
        net.WriteUInt(id, IDBITS)
        net.WriteUInt(1, INSTANCEBITS)
        net.WriteUInt(instanceid, INSTANCEBITS)

        for i = 1, #registration do
            registration[i].write(data[i])
        end

        net.Send(ply or player.GetHumans())
    end

    function dec_shvars.NotifyAllInstances(name, ply)
        local registration = dec_shvars.GetRegistration(name)
        local id = registration.id
        local instances = {}

        for instanceid in pairs(dec_shvars.storedVars[name] or {}) do
            if tonumber(instanceid) then
                instances[#instances + 1] = {instanceid, dec_shvars.GetRaw(name, instanceid)}
            end
        end

        net.Start("dec_shvars")
		dec_shvars.WriteTimeData()
        net.WriteUInt(id, IDBITS)
        net.WriteUInt(#instances, INSTANCEBITS)
		
        for j = 1, #instances do
            local instances_j = instances[j]
            local instanceid = instances_j[1]
            local data = instances_j[2]
            net.WriteUInt(instanceid, INSTANCEBITS)

            for i = 1, #registration do
                registration[i].write(data[i])
            end
        end

        net.Send(ply or player.GetHumans())
    end

    function dec_shvars.SendAll(ply)
        for name in pairs(dec_shvars.storedVars) do
            dec_shvars.Notify(name, ply)
        end
    end

	function dec_shvars.WriteTimeData()
		local ctime = math.floor(CurTime())
		if ctime ~= dec_shvars.lastSecond then
            --player.GetHumans()[1]:ChatPrint(string.format("%s - %s - %s",ctime,dec_shvars.lastSecond,dec_shvars.messagesSentThisSecond))
			dec_shvars.lastSecond = ctime
			dec_shvars.messagesSentThisSecond = 0
		end

		dec_shvars.messagesSentThisSecond = dec_shvars.messagesSentThisSecond + 1
        --player.GetHumans()[1]:ChatPrint(dec_shvars.messagesSentThisSecond)

		net.WriteUInt(ctime,32)
		net.WriteUInt(dec_shvars.messagesSentThisSecond,32)
	end

    -- dec_shvars.Set("ClanData", 1, "b", "c", color_white)
    -- dec_shvars.Set("ClanData", 2, "d", "e", Color(255, 0, 0))
    -- dec_shvars.Set("ClanData", 3, "d", "e", Color(255, 0, 0))
    dec_shvars.Set("NotRandom", 7)
    dec_shvars.Set("NotRandom1", 8)
    dec_shvars.Set("NotRandom2", 7)
    dec_shvars.Set("NotRandom3", 7)

    concommand.Add("_sendSHVars", function(ply)
        if ply.dec_shvarssent and ply.dec_shvarssent > (CurTime() - 3) then return end
        ply.dec_shvarssent = CurTime()
        dec_shvars.SendAll(ply)
    end)

    timer.Create("testingDecSHVars",1,0,function()
        --dec_shvars.Set("NotRandom3", 7)
    end)
end

if CLIENT then
	function dec_shvars.VerifyTimeData(name,instanceid,ctime,messageNum)
		local d = dec_shvars.storedVars[name]
		if !d then return true end

		if instanceid then
            d = d[instanceid]
            if !d then
                return true
            end
        end
	
        Print(d.t)
        local t = d.t or {-1,-1}
        d.t = t

        local f = "%s - %s"
        --Print("time data",string.format(f,ctime,t[1]),string.format(f,messageNum,t[2]))
        if ctime > t[1] or ctime == t[1] and messageNum > t[2] then
            t[1] = ctime
            t[2] = messageNum
            Print(t)
            return true
        end

        Print("time data behind",string.format(f,ctime,t[1]),string.format(f,messageNum,t[2]))

        return false
	end

	function dec_shvars.ReceiveInstanced(registration,ctime,messageNum)
		local count = net.ReadUInt(INSTANCEBITS)
		
		for j = 1, count do
			local instanceid = net.ReadUInt(INSTANCEBITS)

			local data = {}
			for i = 1, #registration do
				data[i] = registration[i].read()
			end

			if !dec_shvars.VerifyTimeData(registration.name,instanceid,ctime,messageNum) then
				continue -- have to wait till after the .read() here or the nets will get out of order
			end

			dec_shvars.Set(registration.name, instanceid, unpack(data))
        	Print(data)
		end
	end

	function dec_shvars.ReadTimeData()
		local ctime = net.ReadUInt(32)
		local messagesSentThisSecond = net.ReadUInt(32)
		return ctime, messagesSentThisSecond
	end

    net.Receive("dec_shvars", function(len)
		local ctime, messageNum = dec_shvars.ReadTimeData()

        local id = net.ReadUInt(IDBITS)
        local registration = dec_shvars.GetRegistrationByID(id)

		if registration.instanced then
			return dec_shvars.ReceiveInstanced(registration, ctime, messageNum)
		end

		if !dec_shvars.VerifyTimeData(registration.name,nil,ctime,messageNum) then
			return
		end

        local data = {}

        for i = 1, #registration do
            data[i] = registration[i].read()
        end

        dec_shvars.Set(registration.name, unpack(data))
        Print(data)
    end)

    hook.Add("InitPostEntity", "dec_shvars", function()
        RunConsoleCommand("_sendSHVars")
    end)
end