# Lua电路板使用方法

## 简介
- Lua电路板通过Lua代码定义电路板的输入和输出，并通过循环体函数在每一tick
获取输入并计算输出，在Lua电路中，可以访问物理结构的信息，如速度，角速度等。

## Lua文件结构
- 对于可以被Lua电路板执行的Lua脚本，需要具有以下几个部分：

#### 输入输出定义
```lua
function define()
    defineInput("a")
    defineInput("b")
    defineOutput("x")
    defineOutput("y")
end
```
- 你需要一个没有local修饰符的define函数，要求无参数，内部通过调用
```lua
defineInput("b")
defineOutput("x")
```
- 来分别定义一个名称为b的输入和x的输出，名字可以为任意字符串，建议言简意赅

#### 设置输入和输出
```lua
local a = getInput("a")
local b = getInput("b")
setOutput("x", 0.1)
setOutput("y", -0.1)
```
在脚本中调用以上函数，可以获取当前电路板某些端口的输入，或者设置指定输出端的值
注意，如果调用了不存在的端口，将抛出LuaError，具体后果参考下文

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
- 你可以使用math.abs(), math.cos()等库函数
#### 矢量库
- 我为lua环境提供了Vector3d类型和Quaterniond类型，使用方式如下
```lua
local v0 = Vector3d:new(1, 2, 3) -- x, y, z
local q0 = Quaterniond:new(0, 0, 0, 1) -- x, y, z, w
```
- 对于它们可以调用的方法，你可以在控制学mod的jar里，找到
data/vscontrolcraft/lua/luaml.lua
在这里你可以查看可以调用的方法

#### 物理信息获取
- 你可以用以下方式获取当前电路板所在的物理结构信息
```lua
local p = Phys.position() -- 质心位置
local q = Phys.quaternionToWorld() -- 旋转四元数，将造船厂坐标系的矢量转换到世界坐标系
local w = Phys.angularVelocity() -- 角速度，世界坐标系
local v = Phys.velocity() -- 速度，世界坐标系
```
- 其中，所返回的矢量均为前面所提到的Vector3d类型，四元数即为Quaterniond类型

## 加载代码
- 把你的lua代码放到lualinks文件夹，其位置参考cimulinks文件夹，是一个与mods文件夹平行的文件夹
- 在游戏里执行/cimulinks load-lua <文件名.lua> 加载你的代码，它会给你一个lua代码编译器
- 右键lua电路板将代码加载到其中

## 异常
#### 超时
- 电路板的一次loop调用超时（3s）
#### LuaError
- 如果代码执行中发生Lua错误，如试图调用nil的字段，访问不存在的端口
#### 以上异常导致的后果
- 电路板将发生爆炸粒子特效与音效（不会破坏方块），连接被断开，且无法继续使用，
需要重新加载lua代码到电路板，在log文件中，你可以查看异常的具体原因