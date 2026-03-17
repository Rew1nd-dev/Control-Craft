# Lua 电脑方块使用方法

## 简介
- 电脑方块会把同一份 Lua 脚本分别运行在服务端和客户端。
- 服务端负责逻辑与状态生产，客户端负责渲染。
- 两端 Lua 状态互不共享；跨端通信请使用 `Network`。

## Lua 文件结构
### 基本回调
```lua
function onServerTick()
    -- 服务端逻辑（可选）
end

function onPlayerEvent(event)
    -- 服务端玩家交互事件（可选）
end

function onClientTick()
    -- 客户端渲染逻辑（可选）
end
```

- 三个回调都可选；缺失时该阶段不会执行。
- 推荐分工：
  - `onServerTick`：计算并 `Network.set(...)`
  - `onClientTick`：读取 `Network.peek/retrieve(...)` 并绘制

### `onPlayerEvent` 事件结构
```lua
-- 触摸事件
local touch_event = { type = "touch", x = 123, y = 45 }

-- 注视事件
local watch = { type = "watch", x = 123, y = 45 }
```

- `x/y` 为屏幕像素坐标。
- 事件触发由交互层负责，脚本中按上面结构处理即可。

## 可用库
### 双端可用
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

#### 向量/四元数
- 全局可用 `Vector3d` 与 `Quaterniond`：
```lua
local v = Vector3d:new(1, 2, 3)
local q = Quaterniond:new(0, 0, 0, 1)
```
- 详细方法见：`src/main/resources/data/vscontrolcraft/lua/luaml.lua`

### 仅服务端可用
#### `Network`（写）
- `Network.set(slotName, value)`

#### `World`
- `World.yell(distance, message)`
- `World.log(message)`
- `World.beep(distance, volume, pitch)`

### 仅客户端可用
#### `Network`（读）
- `Network.peek(slotName) -> value | nil`
- `Network.retrieve(slotName) -> value | nil`（读取后清除该 slot 的 dirty 标记）
- `Network.isDirty(slotName) -> boolean`

#### `render`
- `render.setColor(r, g, b [, a])`（每项 0-255）
- `render.setAlpha(a)`（0-255）
- `render.setOpacity(opacity)`（0.0-1.0）
- `render.drawRect(x, y, w, h)`
- `render.drawText(text, x, y [, scale])`
- `render.pushLayer()`（后续命令层级 +1）
- `render.clear()`（清空当前帧命令并重置层级）
- `render.submit()`（提交到屏幕；不调用则不会刷新显示）
- `render.getWidth()` / `render.getHeight()`
- `render.width` / `render.height`

## 同步与时序
- 服务端通过 `Network.set(...)` 写入数据，客户端通过 `peek/retrieve` 读取。
- 客户端可用 `isDirty + retrieve` 实现“只在更新时处理”。
- 同步数据会序列化为 NBT；单次补丁超过约 `1024` 字节时会被丢弃（发送空补丁）。

## 运行限制与异常
- 每次回调有执行保护：约 `10000` 指令、`2ms`、`10MB` 临时分配。
- 超限或运行错误会抛 `LuaError`，本次回调会被中断。
- Lua 环境禁用了 `dofile/loadfile/load`。

## 加载脚本
- 脚本目录：`<游戏目录>/lualinks`
- 允许两种位置：
  - `<游戏目录>/lualinks/<name>.lua`
  - `<游戏目录>/lualinks/<玩家名>/<name>.lua`
- 常用命令：
  - `/cimulink upload-lua <name>`：上传本地脚本到服务器
  - `/cimulink load-computer-lua <name>`：把脚本加载到你正在看的电脑方块

## 注意事项
- 电脑方块模式下不提供 `Bus` 库（与 Lua 电路板不同）。
- 客户端渲染必须调用 `render.submit()` 才会更新到屏幕。
- `render.pushLayer()` 只有递增，没有 `popLayer()`；通过 `clear()`/`submit()` 重置层级。
- 脚本加载阶段会为不同入口函数解析代码；不建议在顶层执行有副作用的逻辑。

## 示例脚本
- 渲染管线示例：`run/lualinks/test/render_demo.lua`
  - 服务端写入 `tick/pulse`
  - 客户端读取并绘制动画
- 姿态 HUD 示例：`run/lualinks/test/attitude_hud_demo.lua`
  - 客户端结合 `Phys` 与 `Block` 计算地平线并绘制 HUD

## 最小模板
```lua
local tick = 0

function onServerTick()
    tick = tick + 1
    Network.set("tick", tick)
end

function onPlayerEvent(event)
    if type(event) == "table" and event.type == "touch" then
        Network.set("touchX", event.x)
        Network.set("touchY", event.y)
    end
end

function onClientTick()
    local w = (render.getWidth and render.getWidth()) or render.width or 256
    local h = (render.getHeight and render.getHeight()) or render.height or 256
    local t = Network.peek("tick") or 0

    render.clear()
    render.setColor(255, 255, 255, 255)
    render.drawText("tick: " .. tostring(t), 8, 8, 1.0)
    render.drawRect(8, h - 12, math.min(w - 16, t % math.max(1, w - 16)), 6)
    render.submit()
end
```
