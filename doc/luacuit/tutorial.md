# Luacuit 简明教程

Luacuit 是 Cimulink 里的 Lua 电路板组件。它把 Lua 脚本包装成一个带命名输入/输出端口的组件：Cimulink 负责连线和传播信号，Lua 负责在每个更新边沿读取输入、计算逻辑、写入输出。

这份教程只覆盖上手必需的内容；完整接口说明可以继续参考同目录下的 `zh-cn.md`。

## 1. 最小脚本

一个 Luacuit 脚本通常由两个函数组成：

```lua
function define()
    defineInput("a", 0)
    defineInput("b", 0)
    defineOutput("sum")
end

function loop()
    local a = getInput("a")
    local b = getInput("b")
    setOutput("sum", a + b)
end
```

`define()` 用来声明端口，`loop()` 用来运行逻辑。`loop()` 是必须提供的；`define()` 可以省略，但省略后脚本不会自动创建端口。

## 2. 声明端口

```lua
defineInput("name", defaultValue)
defineOutput("name")
```

输入端口需要默认值，输出端口不需要默认值。

端口名需要保持唯一。输入和输出不能重名，也建议避免使用 `@`，因为 Cimulink 内部会用它拼接全局端口名。

## 3. 读取输入与写入输出

```lua
local value = getInput("name")
setOutput("name", value)
```

`getInput(name)` 读取当前输入端口值。

`setOutput(name, value)` 写入输出端口值。同一个输出在一次 `loop()` 中可以多次写入，最终以最后一次写入的值为准。

如果读取或写入不存在的端口，脚本会报错并使当前 Luacuit 停止运行，直到重新加载脚本。

## 4. 更新时机

Luacuit 是一个时序组件。它不会在输入变化的同一组合传播阶段立刻重新计算，而是在 Cimulink 的正边沿更新阶段执行 `loop()`。

这意味着：

- 可以用 Luacuit 安全地保存状态。
- 纯组合闭环仍然不允许，闭环里至少需要 Luacuit、Delay、Scope 等时序组件隔开。
- 如果启用了物理线程运行配置，Luacuit 会跟随物理线程阶段更新；否则跟随主线程 tick 更新。

一个简单的状态示例：

```lua
local count = 0

function define()
    defineInput("inc", 0)
    defineOutput("count")
end

function loop()
    if getInput("inc") > 0.5 then
        count = count + 1
    end
    setOutput("count", count)
end
```

这里的 `count` 是 Lua 脚本里的状态变量，会在后续 `loop()` 调用之间保留。

## 5. 加载脚本

脚本文件放在游戏目录下的 `lualinks` 文件夹中：

```text
<game>/lualinks/<name>.lua
```

常用流程：

1. 把脚本保存到 `lualinks` 目录。
2. 使用 Lua 编译器物品加载脚本，或执行 `/cimulink load-lua <name>`。
3. 用加载好的编译器物品右击 Lua 电路板方块。
4. 打开端口界面，连接输入和输出端口。

如果通过服务器上传脚本，也可能位于：

```text
<game>/lualinks/<player>/<name>.lua
```

## 6. 运行限制

Luacuit 会对脚本运行做保护：

- `define()` 阶段有较短超时，适合只写端口声明，不适合做耗时计算。
- `loop()` 阶段有运行超时，超时后当前 Luacuit 会被禁用。
- Lua 抛错后当前 Luacuit 会被禁用，重新加载脚本后恢复。

因此建议：

- 不要在 `loop()` 里写死循环。
- 不要在 `define()` 里访问世界、方块或执行复杂计算。
- 把端口声明和运行逻辑分开，端口只在 `define()` 中定义。

## 7. 可用能力概览

Luacuit 脚本运行时会注入若干库。常用的有：

| 名称 | 用途 |
| --- | --- |
| `math` | Lua 数学函数 |
| `Vector3d` | 三维向量相关能力 |
| `Quaterniond` | 四元数相关能力 |
| `Phys` | 读取/操作物理相关信息 |
| `World` | 访问世界相关能力 |
| `Bus` | 访问总线数据 |
| `LuaToComputer` | 与电脑方块侧 Lua 能力交互 |

这些库的具体函数会随版本扩展。写脚本时可以优先把 Luacuit 当成 Cimulink 里的“逻辑/状态节点”，再按需要接入物理、世界或总线接口。

## 8. 常见模式

### 比例控制

```lua
function define()
    defineInput("target", 0)
    defineInput("current", 0)
    defineInput("kp", 1)
    defineOutput("control")
end

function loop()
    local error = getInput("target") - getInput("current")
    setOutput("control", error * getInput("kp"))
end
```

### 边沿触发

```lua
local last = 0
local count = 0

function define()
    defineInput("signal", 0)
    defineOutput("count")
end

function loop()
    local now = getInput("signal")
    if last <= 0.5 and now > 0.5 then
        count = count + 1
    end
    last = now
    setOutput("count", count)
end
```

### 输出限幅

```lua
function clamp(x, minValue, maxValue)
    return math.max(minValue, math.min(maxValue, x))
end

function define()
    defineInput("x", 0)
    defineOutput("y")
end

function loop()
    setOutput("y", clamp(getInput("x"), -1, 1))
end
```

## 9. 排错清单

| 现象 | 可能原因 |
| --- | --- |
| 没有端口 | `define()` 未声明端口，或脚本没有重新加载到方块 |
| 脚本加载后不运行 | 缺少 `loop()`，或上一次运行报错/超时后被禁用 |
| 读取端口时报错 | `getInput()` 名称和 `defineInput()` 名称不一致 |
| 写入端口时报错 | `setOutput()` 名称和 `defineOutput()` 名称不一致 |
| 闭环报错 | 线路里存在纯组合闭环，需要插入 Luacuit 或其他时序组件 |

