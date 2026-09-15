# LuaLinks 电脑方块 Lua 文档

## 简介
- 电脑方块会把同一份 Lua 脚本分别加载到服务端和客户端。
- 服务端负责逻辑、Cimulink 总线访问、跨电脑数值共享，以及向客户端同步数据。
- 客户端负责渲染、玩家视线投影、客户端船只信息读取，以及可选的客户端上行数据。
- 两端 Lua 状态不共享；需要跨端通信时使用 `Network`。

## Lua 文件结构
```lua
function onServerTick()
    -- 服务端每游戏 tick 调用；可选
end

function onClientTick()
    -- 客户端每游戏 tick 调用；可选
end

function onPlayerEvent(event)
    -- 当前不可用；见下方说明
end
```

- 三个回调都可以省略，缺失时对应端不会执行。
- 当前实现会解析 `onPlayerEvent(event)`，但 Touch / Watch 事件没有接入实际触发链路，因此脚本里不要依赖它。
- 如果需要“玩家正在看屏幕的位置”，请在客户端使用 `Player.getPlayerWatch()`。

## 脚本加载注意事项
- 电脑会分别为不同入口函数加载脚本。顶层代码可能在服务端加载阶段被执行多次，因此推荐让顶层逻辑保持幂等。
- Lua 环境禁用了 `dofile`、`loadfile`、`load`。
- 全局提供常见 Lua 标准库：`math`、`string`、`table`、`coroutine`、`bit32` 等。

## 双端可用 API
### `Phys`
```lua
Phys.position() -> Vector3d
Phys.positionCenterModel() -> Vector3d
Phys.positionCenter() -> Vector3d
Phys.velocity() -> Vector3d
Phys.angularVelocity() -> Vector3d
Phys.quaternionToWorld() -> Quaterniond
Phys.mass() -> number
Phys.inertia() -> number
```

- `position()` 返回电脑方块位置。
- `positionCenterModel()` 返回当前物理结构质心在 ship/model 坐标中的位置。
- `positionCenter()` 返回当前物理结构质心在世界坐标中的位置。
- `velocity()` / `angularVelocity()` 返回世界坐标系下的速度与角速度。

### `Block`
```lua
Block.frontLocal() -> Vector3d
Block.leftLocal() -> Vector3d
Block.upLocal() -> Vector3d
Block.front() -> Vector3d
Block.left() -> Vector3d
Block.up() -> Vector3d
Block.yardPosition() -> Vector3d
```

- `frontLocal/leftLocal/upLocal` 返回电脑方块本地坐标中的方向。
- `front/left/up` 返回转到世界/yard 坐标后的方向。
- `yardPosition()` 返回电脑方块在世界/yard 坐标中的位置。

### `TextUtil`
```lua
TextUtil.serialize(value) -> string
TextUtil.serialize(value, compact) -> string
TextUtil.serialize(value, options) -> string
```

`options` 格式：
```lua
{
    compact = true,   -- 是否单行输出，默认 true
    pretty = false,   -- true 时等价于 compact = false
    sortKeys = true,  -- 是否排序 table 键，默认 true
    maxDepth = 16     -- 最大递归深度，默认 16
}
```

- 适合把 table、向量、网络数据等转成字符串后画到屏幕或写入日志。
- 遇到循环引用会输出 `"<cycle>"`。
- 超过最大深度会输出 `"<max-depth>"`。

### `Vector3d` 与 `Quaterniond`
```lua
local v = Vector3d:new(1, 2, 3)
local q = Quaterniond:new(0, 0, 0, 1)
```

- 具体方法见 `src/main/resources/data/vscontrolcraft/lua/luaml.lua`。
- 常用向量方法包括 `add`、`sub`、`mul`、`div`、`length`、`normalize`、`dot`、`cross`、`distance`、`negate`。
- 常用四元数方法包括 `mul`、`normalize`、`conjugate`、`inverse`、`transform`、`slerp`。

## 服务端 API
### `World`
```lua
World.yell(distance, message)
World.log(message)
World.beep(distance, volume, pitch)
World.debugLog(message) -> boolean
World.debugLog(fileName, message) -> boolean
World.resetDebugLog() -> boolean
World.resetDebugLog(fileName) -> boolean
World.getDebugLogPath() -> string
World.getDebugLogPath(fileName) -> string
World.gameClock() -> number
World.physClock() -> number
World.dbf() -> table
```

- `yell` 会向附近玩家发送聊天消息。
- `log` 会写入 Control Craft 日志。
- `debugLog` / `resetDebugLog` 使用 Java 侧异步文件日志，适合调试采样数据。
- `gameClock()` 返回统一游戏 tick 计数。
- `physClock()` 返回统一物理 tick 计数。
- `dbf()` 是 flap 调试接口，返回 `{ fx, fy, fz, tx, ty, tz }`；电脑方块当前通常得到默认调试值。

### `Bus`
```lua
Bus.retrieve(componentName, outputPortName) -> number
Bus.propagate(componentName, inputPortName, value)
```

- 电脑通过所在约束簇访问 Cimulink 命名组件。
- `retrieve` 读取组件输出端口。
- `propagate` 写入组件输入端口。
- 目标不存在或端口不匹配时通常会返回 `0.0` 或忽略写入。
- 如果还不知道当前约束簇里有哪些 Plant、它们叫什么、暴露了哪些端口，可以先用下面的 `Plants.list()` / `Plants.find()` 枚举。

### 服务端 `Network`
```lua
Network.set(slotName, value)
Network.clear(slotName)
Network.touch(slotName) -> boolean
Network.isDirty(slotName) -> boolean
Network.peek(slotName) -> value | nil
Network.retrieve(slotName) -> value | nil
Network.setAuthority(playerUuid) -> boolean, info
Network.clearAuthority() -> boolean, info
Network.getAuthority() -> table | nil
```

- `Network` 现在拆成了 `Network.state`、`Network.event`、`Network.authority` 三层；上面这组顶层方法仍然保留，适合做简单的兼容写法。
- 顶层 `set/clear/touch/isDirty/peek/retrieve` 都是状态槽别名。
- 顶层 `setAuthority/clearAuthority/getAuthority` 都是权限别名。
- `peek/get` 只读取，不清除 dirty 标记；`retrieve/consume` 会清除 dirty 标记。
- 顶层 `Network.set(slot, nil)` 等价于清空该槽；`Network.touch(slot)` 会在值不变时强制重新标脏并推进版本号。

#### `Network.state`
```lua
Network.state.get(key) -> value | nil
Network.state.getMany({ key1, key2, ... }) -> table
Network.state.consume(key) -> value | nil
Network.state.changed(key) -> boolean
Network.state.revision(key) -> integer
Network.state.set(key, value)
Network.state.setMany({ key1 = value1, key2 = value2, ... })
Network.state.clear(key)
Network.state.touch(key) -> boolean
```

- `state` 用于“服务端维护状态，客户端镜像读取”的同步槽。
- `changed(key)` 的标记会一直保留到 `consume(key)` 或顶层 `retrieve(key)` 被调用。
- `revision(key)` 是该槽的单调递增版本号；`set`、`clear`、`touch` 都会推进它。
- `Network.state.set(key, nil)` 当前会直接报错；要清空槽请用 `Network.state.clear(key)`，或者继续使用顶层兼容别名 `Network.set(key, nil)`。
- 当前安全可传的值主要是 `number`、`boolean`、`string`，以及由这些值组成的 Lua table。
- 连续的正整数键 table 会按数组发送；其它 table 会按字符串键 map 发送。
- `userdata` 等非常规值最好先手动转成普通 table/number/string，否则跨网络后通常只能得到字符串化结果。

#### `Network.event`
```lua
Network.event.emit(name [, payload]) -> boolean, info
Network.event.peek(name) -> event | nil
Network.event.poll(name) -> event | nil
Network.event.backlog(name) -> integer
Network.event.clear(name)
```

- `event` 是事件队列，不是状态镜像，适合做“按钮触发”“一次性命令”“客户端上报动作”等瞬时消息。
- 服务端 `emit(...)` 会把事件加入发往客户端的出站队列，并返回 `ok, info`；当前成功时 `info` 至少包含 `state = "sent"`、`name`、`seq`。
- `peek(name)` 查看队首事件但不消费，`poll(name)` 会取出并消费队首事件。
- 每个事件名当前最多保留 `32` 条积压，超过后会丢弃最旧事件。
- 事件不会出现在状态 full sync 里；新进入范围的客户端只能收到之后的新事件，不会补历史事件。
- 事件对象格式：

```lua
{
    seq = <number>,
    name = "lock",
    payload = <any | nil>,
    tick = <number>,
    sender = {
        side = "server" | "client",
        ownerUuid = "..." -- 服务端发出的事件通常没有这个字段
    }
}
```

#### `Network.authority`
```lua
Network.authority.get() -> table | nil
Network.authority.force(playerUuid) -> boolean, info
Network.authority.clearForced() -> boolean, info
```

- 电脑方块的客户端上行现在走显式权限控制。服务端可以把权限强制交给某个玩家，也可以清掉强制权限。
- `force(playerUuid)` 需要传玩家 UUID 字符串；成功时会清掉现有租约并切换到强制模式。
- `clearForced()` 会移除当前强制权限；当前实现里也会一并清掉已存在的租约状态。
- `get()` 无权限持有者时返回 `nil`；有权限时返回：

`getAuthority()` / `Network.authority.get()` 返回结构：
```lua
{
    state = "owned",
    mode = "leased" | "forced",
    ownerUuid = "...",
    leaseExpireTick = <number>,
    token = <number>
}
```

- `force/clearForced` 的返回值是 `ok, info`；失败时 `info.reason` 可能是 `invalid_uuid`、`force_failed`、`clear_failed` 之类的原因码。
### `LuaToComputer`
```lua
LuaToComputer.set(computerName, key, value)
LuaToComputer.isPresent(computerName, key) -> boolean
LuaToComputer.clear(computerName, key)
LuaToComputer.get(computerName, key) -> number | nil
```

- 用于同船上按设备名访问电脑方块保存的数值。
- `set` / `clear` 会作用于当前船上所有同名电脑。
- `get` 返回第一个存在该 key 的同名电脑数值。
- 当前值类型为 `double`，适合做轻量数值广播。

### `Plants`
```lua
Plants.list() -> { entry1, entry2, ... }
Plants.find(typeKey) -> { entry1, entry2, ... }
```

- 用于枚举当前电脑所在约束簇中的 Plant。
- 只在服务端 `onServerTick()` 中可用；客户端没有 `Plants`。
- `list()` 返回当前约束簇里的全部 Plant。
- `find(typeKey)` 按类型筛选，既支持完整 key，也支持省略命名空间后的后缀；最稳妥的做法是先 `Plants.list()` 看一遍 `type` 字段，再把它原样传给 `find(...)`。
- 返回顺序不保证稳定；如果需要锁定某一个 Plant，建议结合 `name`、`pos` 或 `metadata` 再筛选。

每个 entry 的结构：
```lua
{
    key = "controlcraft:camera",
    type = "controlcraft:camera", -- 当前与 key 相同
    name = "camera_0",
    shipId = 12345,
    dimension = "minecraft:overworld",
    yard = { x = 0, y = 64, z = 0 },
    pos = { x = 0, y = 64, z = 0 }, -- 当前与 yard 相同
    inputs = { "set_pitch", "set_yaw" },
    outputs = { "yaw", "pitch", "used" },
    metadata = {
        block = "controlcraft:camera",
        blockEntity = "controlcraft:camera",
        direction = "north",
        axis = "z"
    }
}
```

- `inputs` / `outputs` 是当前 Plant 暴露的端口名列表。
- `metadata` 是附加信息，当前常见字段有 `block`、`blockEntity`、`direction`、`axis`；某些 Plant 可能没有这些字段。
- `pos` / `yard` 使用整数方块坐标，当前两者内容相同。
- 如果需要继续理解这些端口的含义，可以对照 `doc/luacuit/plants.md`。

按类型枚举示例：
```lua
function onServerTick()
    local cameras = Plants.find("camera")
    for _, entry in ipairs(cameras) do
        World.log(
            entry.name .. " @ " ..
            entry.dimension .. " (" ..
            entry.pos.x .. ", " .. entry.pos.y .. ", " .. entry.pos.z .. ")"
        )
    end
end
```

调试返回结构示例：
```lua
function onServerTick()
    local plants = Plants.list()
    if #plants > 0 then
        World.log(TextUtil.serialize(plants[1], { pretty = true }))
    end
end
```

## 客户端 API
### `render`
```lua
render.setColor(r, g, b [, a])
render.setAlpha(a)
render.setOpacity(opacity)
render.setResolution(width, height)
render.setSurfaceSize(width, height)
render.setOffset(x, y, z)
render.drawRect(x, y, w, h)
render.drawRectOutline(x, y, w, h [, thickness])
render.drawLine(x0, y0, x1, y1 [, thickness])
render.drawCircle(cx, cy, radius [, segments])
render.drawCircleOutline(cx, cy, radius [, thickness [, segments]])
render.drawText(text, x, y [, scale])
render.pushLayer()
render.clear()
render.submit()
render.getWidth() -> number
render.getHeight() -> number
render.getSurfaceWidth() -> number
render.getSurfaceHeight() -> number
render.width
render.height
```

- 默认分辨率为 `256 x 256`，默认物理尺寸为 `1.0 x 1.0`。
- `setResolution` 范围为 `1 ~ 4096`。
- `setSurfaceSize` 范围为 `0.05 ~ 64.0`。
- `setOffset` 每轴范围为 `-10 ~ 10`，用于移动屏幕渲染平面。
- `setColor/setAlpha` 的颜色通道范围为 `0 ~ 255`。
- `setOpacity` 范围为 `0.0 ~ 1.0`。
- `drawLine` 与描边图元默认线宽为 `1.0`。
- `drawCircle` 与 `drawCircleOutline` 默认 `32` 段，实际段数会限制在 `8 ~ 256`。
- `pushLayer()` 只递增绘制层，没有 `popLayer()`；`clear()` 和 `submit()` 会重置层级。
- 必须调用 `submit()`，本帧绘制命令才会显示到屏幕。

### 客户端 `Network`
```lua
Network.isDirty(slotName) -> boolean
Network.peek(slotName) -> value | nil
Network.retrieve(slotName) -> value | nil
Network.send(slotName, value) -> boolean, info
Network.clear(slotName) -> boolean, info
Network.canSend() -> boolean
Network.requestAuthority() -> boolean, info
Network.releaseAuthority() -> boolean, info
Network.getAuthority() -> table | nil
```

- 客户端 `Network` 同样拆成 `state`、`event`、`authority` 三层；上面这组顶层方法是兼容别名。
- 顶层 `peek/retrieve/isDirty` 是状态槽读别名。
- 顶层 `send/clear` 是状态槽写别名。
- 顶层 `canSend/requestAuthority/releaseAuthority/getAuthority` 是权限别名。
- 现在绝大多数“向服务端发东西”的接口都返回 `ok, info` 两个值，而不再只是单个 boolean。
- 顶层 `Network.send(slot, nil)` 等价于清空该槽。
- 单个客户端上行状态包或事件包当前都不能超过约 `1024` 字节；超限会返回 `payload_too_large`。
- `send(...)` 只是把数据发往服务端，不会立刻改写客户端本地镜像；本地看到的新值仍要等服务端回写同步。

#### `Network.state`
```lua
Network.state.get(key) -> value | nil
Network.state.getMany({ key1, key2, ... }) -> table
Network.state.consume(key) -> value | nil
Network.state.changed(key) -> boolean
Network.state.revision(key) -> integer
Network.state.send(key, value) -> boolean, info
Network.state.sendMany({ key1 = value1, key2 = value2, ... }) -> boolean, info
Network.state.clear(key) -> boolean, info
```

- 读接口和服务端 `state` 基本对称：`get/peek` 不清 dirty，`consume/retrieve` 清 dirty。
- `send(key, value)` 会向服务端发送一次写槽请求；如果 `value` 为 `nil`，请改用 `clear(key)`。
- `sendMany(table)` 会把 table 拆成多条独立的状态写包发送，不保证原子性。
- 当客户端尚未持有上行权限时，`send/clear/sendMany` 会自动尝试申请权限，并返回 `ok = false` 配合 `info.state = "requested"` 或 `info.state = "cooldown"`。
- 如果权限当前属于别人，通常会返回 `ok = false`，并带上 `info.state = "denied"` 与 `info.reason = "leased_by_other"` 或 `forced_owner`。
- `Network.state.send(key, nil)` 不会清槽，而是返回 `ok = false` 与 `reason = "invalid_nil"`；要清空请用 `Network.state.clear(key)`。

#### `Network.event`
```lua
Network.event.emit(name [, payload]) -> boolean, info
Network.event.peek(name) -> event | nil
Network.event.poll(name) -> event | nil
Network.event.backlog(name) -> integer
Network.event.clear(name)
```

- `emit(name, payload)` 会把事件发给服务端事件队列；不会自动回显到客户端本地队列。
- 客户端事件也使用和服务端相同的事件结构，`sender.side` 通常为 `client`，`sender.ownerUuid` 是发送玩家 UUID。
- `event` 很适合做“点按一次就发一次”的输入；如果你需要的是最新状态镜像，优先用 `state`。

#### `Network.authority`
```lua
Network.authority.canSend() -> boolean
Network.authority.request() -> boolean, info
Network.authority.release() -> boolean, info
Network.authority.get() -> table | nil
```

- 客户端上行权限分两种：`leased` 租约权限，以及服务端强制指定的 `forced` 权限。
- 当前实现里，客户端请求权限的冷却约为 `10` 个游戏 tick；租约持续约 `30` 个游戏 tick，并在持续成功上行时续期。
- `request()` 已经拥有权限时会返回 `true, { state = "owned", ... }`。
- `request()` 成功发出申请时会返回 `true, { state = "requested", retryInTicks = ... }`；这表示请求已发出，但本 tick 还不能立刻发送状态包。
- `release()` 只能释放自己持有的 `leased` 权限，不能释放服务端强制指定给别人的 `forced` 权限。
- `get()` 在完全空闲时返回 `nil`；在“别人持有权限”或“自己刚发起申请但还在冷却”时会返回带状态的 table。

客户端 `getAuthority()` / `Network.authority.get()` 返回结构：
```lua
{
    state = "owned" | "denied" | "requested" | "cooldown",
    mode = "leased" | "forced",
    ownerUuid = "...",
    leaseExpireTick = <number>,
    token = <number>,
    isSelf = <boolean>,
    retryInTicks = <number>
}
```

- `retryInTicks` 只会在请求中的冷却阶段出现。
- 客户端写接口返回的 `info` 里常见字段有：
  - `action`：如 `state_set`、`state_clear`、`state_set_many`、`event_emit`
  - `state`：如 `owned`、`requested`、`cooldown`、`denied`
  - `reason`：如 `awaiting_authority`、`leased_by_other`、`forced_owner`、`payload_too_large`
  - `key` / `seq` / `count`：对应具体槽名、事件序号或批量发送数量
### `Player`
```lua
Player.yaw([partialTicks]) -> number
Player.pitch([partialTicks]) -> number
Player.getShipMountedToData([partialTicks]) -> table | nil
Player.getPlayerWatch([partialTicks]) -> table | nil
Player.projectWorldPoint(worldPos [, partialTicks]) -> table | nil
Player.projectWorldPoint(x, y, z [, partialTicks]) -> table | nil
```

`getShipMountedToData()` 返回：
```lua
{
    shipMountedTo = <LoadedShip userdata>,
    shipMountedToId = <number>,
    mountPosInShip = <Vector3d>
}
```

`getPlayerWatch()` 与 `projectWorldPoint()` 返回：
```lua
{
    x = <number>,
    y = <number>,
    distance = <number>,
    onScreen = <boolean>,
    hitPosInWorld = <Vector3d>,
    screenCenterInWorld = <Vector3d>,

    -- 兼容旧脚本的别名
    hitPosInShip = <Vector3d>,
    screenCenterInShip = <Vector3d>
}
```

- `x/y` 是当前屏幕分辨率下的像素坐标。
- `onScreen` 表示交点是否落在屏幕范围内。
- `getPlayerWatch()` 计算玩家视线与当前电脑屏幕平面的交点。
- `projectWorldPoint(...)` 计算“玩家眼睛到指定世界点”的射线与屏幕平面的交点。
- 若射线与屏幕平面平行、交点在玩家身后、或输入无效，则返回 `nil`。

### `Ship`
```lua
Ship.getAllShips() -> { id1, id2, ... }
Ship.getPositionOf(id) -> Vector3d | nil
Ship.getVelocityOf(id) -> Vector3d | nil
Ship.getQuaternionOf(id) -> Quaterniond | nil
Ship.getAngularVelocityOf(id) -> Vector3d | nil
```

- `getAllShips()` 返回客户端当前已加载船只的 id 列表。
- 找不到指定 id 时，其他 `get...Of(id)` 函数返回 `nil`。

## 网络同步建议
### 服务端到客户端状态
```lua
function onServerTick()
    Network.state.set("tick", World.gameClock())
end

function onClientTick()
    local tick = Network.state.get("tick") or 0
end
```

- 服务端写入后，附近客户端会收到同步补丁。
- 对于只同步一个槽的简单脚本，继续用顶层兼容别名 `Network.set(...)` / `Network.peek(...)` 也可以。

### 客户端到服务端状态
```lua
function onClientTick()
    local ok, info = Network.state.send("client_pitch", Player.pitch(0.0))
    if not ok and info and (info.state == "requested" or info.state == "cooldown") then
        return
    end
end

function onServerTick()
    if Network.state.changed("client_pitch") then
        local pitch = Network.state.consume("client_pitch")
        World.log("client pitch = " .. tostring(pitch))
    end
end
```

- 现在可以直接调用 `Network.state.send(...)`，让它在缺权限时自动触发申请。
- 如果你希望把“申请权限”和“实际发送”拆开，也可以先调 `Network.authority.request()`，稍后再发送。

### 客户端到服务端事件
```lua
function onClientTick()
    if shouldFire then
        local ok, info = Network.event.emit("fire", {
            pitch = Player.pitch(0.0),
            yaw = Player.yaw(0.0)
        })
    end
end

function onServerTick()
    while Network.event.backlog("fire") > 0 do
        local event = Network.event.poll("fire")
        if event then
            World.log(TextUtil.serialize(event.payload, { pretty = true }))
        end
    end
end
```

- 事件适合发送一次性动作；状态适合发送“当前值”。
- 如果需要让其它客户端也看到某个客户端事件，通常做法是服务端收到事件后再用 `Network.event.emit(...)` 或 `Network.state.set(...)` 转发。
## 渲染最小示例
```lua
local init = false

function onClientTick()
    if not init then
        render.setResolution(320, 180)
        render.setSurfaceSize(1.8, 1.0)
        render.setOffset(0.0, 1.0, -0.5)
        init = true
    end

    render.clear()
    render.setColor(16, 32, 48, 220)
    render.drawRect(0, 0, render.getWidth(), render.getHeight())

    render.pushLayer()
    render.setColor(255, 255, 255, 255)
    render.drawText("LuaLinks", 8, 8, 1.0)
    render.drawLine(8, 24, 120, 24, 2)
    render.drawCircleOutline(160, 90, 24, 2, 48)

    render.submit()
end
```

## 玩家视线投影示例
```lua
function onClientTick()
    local watch = Player.getPlayerWatch(0.0)

    render.clear()
    render.setColor(255, 255, 255, 255)

    if watch and watch.onScreen then
        render.drawRect(watch.x - 2, watch.y - 2, 5, 5)
        render.drawText("watch", watch.x + 6, watch.y, 0.7)
    else
        render.drawText("not looking at screen", 8, 8, 0.8)
    end

    render.submit()
end
```

## 加载脚本
- 脚本目录：`<游戏目录>/lualinks`
- 支持路径：
  - `<游戏目录>/lualinks/<name>.lua`
  - `<游戏目录>/lualinks/<玩家名>/<name>.lua`
- 常用命令：
  - `/cimulink upload-lua <name>` 上传本地脚本到服务器
  - `/cimulink load-computer-lua <name>` 把脚本加载到当前看着的电脑方块

## 示例脚本
- `run/lualinks/api_demo.lua`：渲染、网络、Player、Block API 总览。
- `run/lualinks/render_demo.lua`：基础渲染与服务端同步示例。注意其中 `onPlayerEvent` 部分当前不会被触发。
- `run/lualinks/hud.lua`：姿态 HUD 示例。
- `run/lualinks/player_watch_demo.lua`：使用 `Player.getPlayerWatch()` 显示玩家视线落点。
- `run/lualinks/bus_angle_demo.lua`：服务端读取 `Bus` 并同步显示。
- `run/lualinks/bus_angle_write_demo.lua`：服务端通过 `Bus.propagate` 周期性写入。
- `run/lualinks/tick_clock_demo.lua`：输出 `gameClock/physClock`。
