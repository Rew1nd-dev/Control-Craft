# LuaLinks 电脑脚本文档（中文）

## 1. 概览
- 同一份 Lua 脚本会在服务端与客户端分别运行。
- 服务端主要负责逻辑、总线访问、跨电脑通信与网络数据生产。
- 客户端主要负责渲染，以及读取客户端玩家/船只信息。
- 两端 Lua 状态互不共享；跨端同步请使用 `Network`。
- 以下回调均为可选：
  - `onServerTick()`
  - `onClientTick()`
  - `onPlayerEvent(event)`

```lua
function onServerTick()
end

function onClientTick()
end

function onPlayerEvent(event)
end
```

### 1.1 `onPlayerEvent(event)`
目前电脑服务端会向 `onPlayerEvent(event)` 传入两类事件：

```lua
{
    type = "touch",
    x = <number>,
    y = <number>
}
```

```lua
{
    type = "watch",
    x = <number>,
    y = <number>
}
```

说明：
- `x/y` 为当前屏幕分辨率下的像素坐标。
- `touch` 表示玩家与屏幕交互。
- `watch` 表示玩家视线落到屏幕上的位置。

## 2. 可用 API 总览

### 2.1 双端可用

#### `Phys`
- `Phys.position() -> Vector3d`
- `Phys.velocity() -> Vector3d`
- `Phys.angularVelocity() -> Vector3d`
- `Phys.quaternionToWorld() -> Quaterniond`
- `Phys.mass() -> number`
- `Phys.inertia() -> number`

#### `Block`
- `Block.frontLocal() -> Vector3d`
- `Block.leftLocal() -> Vector3d`
- `Block.upLocal() -> Vector3d`
- `Block.front() -> Vector3d`
- `Block.left() -> Vector3d`
- `Block.up() -> Vector3d`
- `Block.yardPosition() -> Vector3d`

说明：
- `Local` 版本返回方块在自身局部坐标中的方向。
- 非 `Local` 版本返回转到世界/yard 坐标后的方向。
- `Block.yardPosition()` 返回当前电脑所在位置的 yard/world 坐标。

#### 向量与四元数类型
- 全局提供 `Vector3d` 与 `Quaterniond`，来自 `luaml`。
- 详细成员方法可参考 `src/main/resources/data/vscontrolcraft/lua/luaml.lua`。

### 2.2 仅服务端可用

#### `World`
- `World.yell(distance, message)`
- `World.log(message)`
- `World.beep(distance, volume, pitch)`
- `World.debugLog(message) -> boolean`
- `World.debugLog(fileName, message) -> boolean`
- `World.resetDebugLog() -> boolean`
- `World.resetDebugLog(fileName) -> boolean`
- `World.getDebugLogPath() -> string`
- `World.getDebugLogPath(fileName) -> string`
- `World.gameClock() -> number`
- `World.physClock() -> number`
- `World.dbf() -> table`

`World.dbf()` 返回结构：
```lua
{
    fx = <number>,
    fy = <number>,
    fz = <number>,
    tx = <number>,
    ty = <number>,
    tz = <number>
}
```

说明：
- `World.debugLog(...)` 使用 Java 侧异步日志线程写盘，比 Lua `io` 更适合物理线程环境。
- 调试日志默认写到 `<游戏目录>/controlcraft-debug/lua/`。
- 不传 `fileName` 时，会使用当前设备的默认日志文件名。
- `World.gameClock()` 为统一的服务器游戏 tick 计数。
- `World.physClock()` 为统一的物理 tick 计数。
- `World.dbf()` 是调试接口，用于读取上一 tick 的 flap 调试量。

#### `Bus`
- `Bus.retrieve(componentName, outputPortName) -> number`
- `Bus.propagate(componentName, inputPortName, value)`

说明：
- 电脑通过所在约束簇访问 `NamedComponent`。
- `Bus.retrieve(...)` 在组件不存在或端口不匹配时返回 `0.0`。
- `Bus.propagate(...)` 在组件不存在或端口不匹配时会直接忽略。

#### `LuaToComputer`
- `LuaToComputer.set(computerName, key, value)`
- `LuaToComputer.isPresent(computerName, key) -> boolean`
- `LuaToComputer.clear(computerName, key)`
- `LuaToComputer.get(computerName, key) -> number | nil`

说明：
- 按“电脑设备名称”访问同船上的电脑。
- `set/clear` 会对当前船上所有同名电脑生效。
- `isPresent` 只要任意同名电脑有该键就返回 `true`。
- `get` 返回第一个存在该键值的同名电脑的数据；若不存在则返回 `nil`。

#### 服务端 `Network`
- `Network.set(slotName, value)`
- `Network.isDirty(slotName) -> boolean`
- `Network.peek(slotName) -> value | nil`
- `Network.retrieve(slotName) -> value | nil`

### 2.3 仅客户端可用

#### 客户端 `Network`
- `Network.isDirty(slotName) -> boolean`
- `Network.peek(slotName) -> value | nil`
- `Network.retrieve(slotName) -> value | nil`
- `Network.send(slotName, value) -> true`

#### `render`
- `render.setColor(r, g, b [, a])`
- `render.setAlpha(a)`
- `render.setOpacity(opacity)`
- `render.setResolution(width, height)`
- `render.setSurfaceSize(width, height)`
- `render.setOffset(x, y, z)`
- `render.drawRect(x, y, w, h)`
- `render.drawText(text, x, y [, scale])`
- `render.pushLayer()`
- `render.clear()`
- `render.submit()`
- `render.getWidth() -> number`
- `render.getHeight() -> number`
- `render.getSurfaceWidth() -> number`
- `render.getSurfaceHeight() -> number`
- `render.width`
- `render.height`

参数范围：
- `render.setColor(...)` / `render.setAlpha(...)`：颜色分量范围 `0 ~ 255`
- `render.setOpacity(opacity)`：范围 `0.0 ~ 1.0`
- `render.setResolution(width, height)`：范围 `1 ~ 4096`
- `render.setSurfaceSize(width, height)`：范围 `0.05 ~ 64.0`
- `render.setOffset(x, y, z)`：每轴范围 `-10 ~ 10`

说明：
- 渲染分辨率、屏幕物理尺寸、屏幕偏移均由客户端脚本决定。
- `render.width` / `render.height` 是当前分辨率的便捷字段。
- `render.pushLayer()` 只会让之后发出的绘制命令层级加一，没有 `popLayer()`。
- `render.clear()` 会清空当前帧命令，并把层级重置为 `0`。
- 只有调用 `render.submit()` 后，本帧内容才会提交到屏幕。

#### `Player`
- `Player.yaw([partialTicks]) -> number`
- `Player.pitch([partialTicks]) -> number`
- `Player.getShipMountedToData([partialTicks]) -> table | nil`
- `Player.getPlayerWatch([partialTicks]) -> table | nil`
- `Player.projectWorldPoint(worldPos [, partialTicks]) -> table | nil`
- `Player.projectWorldPoint(x, y, z [, partialTicks]) -> table | nil`

`Player.getShipMountedToData()` 返回结构：
```lua
{
    shipMountedTo = <LoadedShip userdata>,
    shipMountedToId = <number>,
    mountPosInShip = <Vector3d>
}
```

`Player.getPlayerWatch()` 与 `Player.projectWorldPoint()` 返回结构一致：
```lua
{
    x = <number>,
    y = <number>,
    distance = <number>,
    onScreen = <boolean>,
    hitPosInWorld = <Vector3d>,
    screenCenterInWorld = <Vector3d>,

    -- 兼容旧脚本保留的别名
    hitPosInShip = <Vector3d>,
    screenCenterInShip = <Vector3d>
}
```

说明：
- `x/y` 是当前屏幕分辨率下的像素坐标。
- `distance` 是从玩家眼睛到“屏幕平面交点”的距离，不是到目标世界点本身的距离。
- `onScreen` 表示交点是否落在屏幕范围内。
- `Player.getPlayerWatch()` 使用玩家真实眼睛位置和视线方向，计算视线与屏幕的交点。
- 若玩家 mount 在船上，会先结合该船姿态把 `yaw/pitch` 对应的本地方向转到世界中；若未 mount，则使用单位变换。
- `Player.projectWorldPoint(...)` 取“玩家眼睛到目标世界点”的连线，并计算这条射线打到屏幕平面后的像素位置。
- `projectWorldPoint(...)` 的第一个参数可以是 `Vector3d`，也可以是含 `x/y/z` 的 Lua table。
- 若射线与屏幕平面平行、交点在眼睛后方、或输入无效，则返回 `nil`。

#### `Ship`
- `Ship.getAllShips() -> { id1, id2, ... }`
- `Ship.getPositionOf(id) -> Vector3d | nil`
- `Ship.getVelocityOf(id) -> Vector3d | nil`
- `Ship.getQuaternionOf(id) -> Quaterniond | nil`
- `Ship.getAngularVelocityOf(id) -> Vector3d | nil`

说明：
- `Ship.getAllShips()` 返回客户端当前已加载的船 id 列表。
- 查不到对应 `id` 时，其余 `get...Of(id)` 返回 `nil`。

## 3. 屏幕配置与最小示例

### 3.1 推荐初始化方式
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
    render.setColor(255, 255, 255, 255)
    render.drawText("Hello", 8, 8, 1.0)
    render.submit()
end
```

### 3.2 透明度与层级
- `render.setAlpha(a)` 使用 `0 ~ 255`
- `render.setOpacity(opacity)` 使用 `0.0 ~ 1.0`
- 多个重叠图元建议在后绘制内容前调用 `render.pushLayer()`，减少同层 z-fighting

## 4. 网络同步与限制

### 4.1 服务端到客户端
- 服务端 `Network.set(...)` 后，会通过每 tick 同步下发。
- 客户端使用 `peek/retrieve/isDirty` 读取。

### 4.2 客户端到服务端
- 客户端使用 `Network.send(slotName, value)` 上行。
- 服务端通过 `peek/retrieve/isDirty` 读取。
- 单次上行消息大于 `1024 Byte` 会被服务端拒收。

### 4.3 支持的数据类型
- 基础类型：`string`、`number`、`boolean`
- 复合类型：普通 `table`
- 不建议发送：函数、线程、复杂 userdata 等对象

## 5. 运行限制与错误
- 每次回调执行都受指令数、执行时间与内存分配保护。
- 超限或运行时错误会抛出 `LuaError`，并中断本次回调。
- Lua 环境禁用了 `dofile`、`loadfile`、`load`。

## 6. 脚本加载位置与命令
- 脚本目录：`<游戏目录>/lualinks`
- 支持两种路径：
  - `<游戏目录>/lualinks/<name>.lua`
  - `<游戏目录>/lualinks/<玩家名>/<name>.lua`
- 常用命令：
  - `/cimulink upload-lua <name>`
  - `/cimulink load-computer-lua <name>`

## 7. 示例脚本
- `run/lualinks/api_demo.lua`：渲染、网络、Player、Block API 总览
- `run/lualinks/render_demo.lua`：基础渲染示例
- `run/lualinks/hud.lua`：姿态 HUD 示例
- `run/lualinks/player_watch_demo.lua`：显示玩家视线在屏幕上的落点
- `run/lualinks/bus_angle_demo.lua`：服务端读取总线 `angle` 并显示到屏幕
- `run/lualinks/bus_angle_write_demo.lua`：服务端周期性写入总线 `angle`
- `run/lualinks/tick_clock_demo.lua`：输出 `gameClock/physClock`
- `run/lualinks/api_demo.lua`：包含 `Network.send`、`Player`、`Block`、屏幕配置等新接口示例

## 8. `Player.projectWorldPoint()` 最小示例
```lua
local init = false

function onClientTick()
    if not init then
        render.setResolution(320, 180)
        render.setSurfaceSize(1.8, 1.0)
        render.setOffset(0.0, 1.0, -0.5)
        init = true
    end

    local pos = Block.yardPosition():add(Block.front():mul(10.0))
    local p = Player.projectWorldPoint(pos, 0.0)

    render.clear()
    render.setColor(15, 30, 55, 255)
    render.setOpacity(0.5)
    render.drawRect(0, 0, render.getWidth(), render.getHeight())
    render.setOpacity(1.0)

    if p then
        render.setColor(255, 220, 90, 255)
        render.drawRect(p.x - 2, p.y - 2, 5, 5)
    end

    render.submit()
end
```
