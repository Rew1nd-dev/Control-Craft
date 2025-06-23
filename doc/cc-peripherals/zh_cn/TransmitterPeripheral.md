# TransmitterPeripheral Methods

This document describes the Lua methods available in the `TransmitterPeripheral` peripheral.



### `setProtocol(p)`
- **参数**：
  - `p` (`long`): 
- **返回值**：`void` - 
- **描述**：设置外设代理的频道

### `callRemote(remoteName, mothodName, args...)`
- **参数**：
  - `remoteName` (`string`): 外设接口的名称
  - `mothodName` (`string`): 外设接口所贴外设的目标方法名称
  - `...args` (`any`): 传递给目标方法的参数，不定长，需要与目标方法的参数列表一致
- **返回值**：`MethodResult` - 调用方法返回值
- **描述**：用于调用远程外设，立刻返回方法结果，Lua线程可能会被目标外设方法阻塞
- **示例**：
```lua
-- 假设外设接口贴在轴承上，名称为servo，频道为1
p = peripheral.find("transmitter")
p.setProtocol(1)
p.callRemote("servo", "setTargetValue", 1.0)
v = p.callRemote("getTargetValue") -- v = 1.0
```

### `callRemoteAsync(slotName, remoteName, mothodName, args...)`
- **参数**：
  - `slotName` (`string`): 该异步调用的标识符
  - `remoteName` (`string`): 外设接口的名称
  - `mothodName` (`string`): 外设接口所贴外设的目标方法名称
  - `args...` (`any`): 传递给目标方法的参数，不定长，需要与目标方法的参数列表一致
- **返回值**：`void` - 
- **描述**：用于异步调用远程外设，
被调用的方法将会在下一个游戏刻处理，
因此没有返回值，Lua线程不会被阻塞，
在两个游戏刻之间，相同标识符的调用只有最新的一次有效



