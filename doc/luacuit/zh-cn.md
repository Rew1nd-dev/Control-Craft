# Lua电路板使用方法

## 简介
- Lua电路板通过Lua代码定义电路板的输入和输出，并通过循环体函数在每一tick
获取输入并计算输出，在Lua电路中，可以访问物理结构的信息，如速度，角速度，
质量，也可以通过代码与世界进行交互，使用内置的外设总线从而省去连线等。

## Lua文件结构
- 对于可以被Lua电路板执行的Lua脚本，需要具有以下几个部分：

#### 输入输出定义
```lua
function define()
    defineInput("a", 1.0)   -- 定义端口“a”,并带有默认初始值1.0
    defineInput("b")        -- 等同于defineInput("b", 0.0)
    defineOutput("x")
    defineOutput("y")
end
```
- 你需要一个**没有local**修饰符的define函数，要求无参数，内部通过调用
```lua
defineInput("b")
defineOutput("x")
```
- 来分别定义一个名称为b的输入和x的输出，名字可以为任意字符串，但不要包含“@”，建议言简意赅

#### 设置输入和输出
```lua
local a = getInput("a")
local b = getInput("b")
setOutput("x", 0.1)
setOutput("y", -0.1)
```
在脚本中调用以上函数，可以获取当前电路板某些端口的输入，或者设置指定输出端的值，一轮循环中，对给定端口只有最后一次设置的输入有效

注意，如果调用了不存在的端口，将抛出`LuaError`，具体后果参考下文

#### 循环体
```lua

function loop() 
    -- do something
    local a = getInput("a")
    local b = getInput("b")
    local x = a + b
    local y = a * b
    setOutput("x", x)
    setOutput("y", y)
end
```
在你的脚本中，需要定义循环体函数，它是一个没有local修饰的名称为loop的无参数函数
该函数会被每一物理刻/游戏刻(取决于电路板当前运行在哪个线程，用指令切换)调用
在循环体中你可以使用上面提到的输入输出函数来计算

## Lua代码中的可用库
#### Lua的math库：
- 你可以使用`math.abs(), math.cos()`等库函数
#### 矢量库
- 我为lua环境提供了`Vector3d`类型和`Quaterniond`类型，使用方式如下
```lua
local v0 = Vector3d:new(1, 2, 3) -- x, y, z
local q0 = Quaterniond:new(0, 0, 0, 1) -- x, y, z, w
```
- 对于它们可以调用的方法，你可以在控制学mod的jar里，找到
`data/vscontrolcraft/lua/luaml.lua`
在这里你可以查看可以调用的方法

#### 物理信息获取
- 你可以用以下方式获取当前电路板所在的物理结构信息
```lua
local p = Phys.position() -- lua电路板方块的世界位置
local q = Phys.quaternionToWorld() -- 旋转四元数，将造船厂坐标系的矢量转换到世界坐标系
local w = Phys.angularVelocity() -- 角速度，世界坐标系
local v = Phys.velocity() -- 质心速度，世界坐标系
local m = Phys.mass() -- 质量
local i = Phys.inertia() -- 转动惯量，double, 
--由于vs的特性，任意刚体转动惯量矩阵为等效于球，故取转动惯量矩阵00元素
```
- 其中，所返回的矢量均为前面所提到的Vector3d类型，四元数即为Quaterniond类型

#### 与世界交互
- 你可以用以下方式发送调试信息或与世界交互
```lua
local msg = string.format("%.2s", 0.05)
World.yell(20, msg) -- 向半径20格内玩家发送消息，消息内容为字符串
World.log(msg) -- 向后台打印调试信息，内容为字符串
World.beep(distance, volume, pitch) -- 蜂鸣器，参数含义：半径、响度、音调，其中音调具体含义我也不清楚，我调用的原版api
```
#### 与总线交互
- lua电路板理论上可以不暴露任何端口或连线就进行控制
```lua

local v = Bus.retrieve("motor_0", "angle") 
-- 获取当前船组（被约束连在一起的整体）上任意一个名称为“motor_0”的设备的"angle"端口的值
-- 如果有重名，则不能保证获取到的是谁的
-- 如果没有该设备或该端口不是输出型端口，则返回0.0
Bus.propagate("motor_0", "target", v + 0.1)
-- 将数值设置到当前船组（被约束连在一起的整体）上**所有**名称为“motor_0”的设备的"target"端口
-- 如果没有该设备或该端口不是输入型端口，则无效
```

## 加载代码
- 把你的lua代码放到`lualinks`文件夹，其位置参考`cimulinks`文件夹，是一个与`mods`文件夹平行的文件夹
- 在游戏里执行`/cimulinks load-lua <文件名.lua>` 加载你的代码，它会给你一个lua代码编译器
- 在多人游戏中，使用指令`/cimulinks upload-lua <文件名.lua>` 可以将本地的代码上传到服务端
- 右键lua电路板将代码加载到其中

## 异常
#### 超时
- 电路板的一次loop调用超时（3s）
#### LuaError
- 如果代码执行中发生Lua错误，如试图调用nil的字段，访问不存在的端口
#### 以上异常导致的后果
- 电路板将发生爆炸粒子特效与音效（不会破坏方块），且无法继续使用，
需要重新加载lua代码到电路板，在log文件中，你可以查看异常的具体原因