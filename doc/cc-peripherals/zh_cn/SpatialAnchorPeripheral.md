# SpatialAnchorPeripheral Methods

This document describes the Lua methods available in the `SpatialAnchorPeripheral` peripheral.

### `setOffset(offset)`
- **参数**：
  - `offset` (`double`): 偏移量
- **返回值**：`void` - 
- **描述**：设置空间锚相对目标的偏移量，只有在自身不为static状态时才有效


### `setChannel(channel)`
- **参数**：
  - `channel` (`long`): 
- **返回值**：`void` - 
- **描述**：设置频道，非static空间锚只会寻找相同频道下的static空间锚作为目标


### `setStatic(isStatic)`
- **参数**：
  - `isStatic` (`boolean`): 
- **返回值**：`void` - 
- **描述**：设置是否为static空间锚
- **示例**：
  ```lua

  ```

### `setPPID(p, i, d)`
- **参数**：
  - `p` (`double`): 
  - `i` (`double`): 
  - `d` (`double`): 
- **返回值**：`void` - 
- **描述**：设置用于控制位置的PID控制器参数


### `setRunning(isRunning)`
- **参数**：
  - `isRunning` (`boolean`): 
- **返回值**：`void` - 
- **描述**：设置是否处于运行状态，非运行状态空间锚既不会成为其他空间锚的目标，也不会寻找目标空间锚


### `setQPID(p, i, d)`
- **参数**：
  - `p` (`double`): 
  - `i` (`double`): 
  - `d` (`double`): 
- **返回值**：`void` - 
- **描述**：设置用于控制旋转的PID控制器参数



