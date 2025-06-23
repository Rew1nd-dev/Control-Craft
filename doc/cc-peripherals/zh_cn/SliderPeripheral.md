# SliderPeripheral Methods

This document describes the Lua methods available in the `SliderPeripheral` peripheral.

### `lock`
- **参数**：
  - 无
- **返回值**：`void` -
- **描述**：如果活塞未锁定，则锁定活塞


### `unlock`
- **参数**：
  - 无
- **返回值**：`void` -
- **描述**：如果活塞已锁定，则解锁活塞


### `isLocked`
- **参数**：
  - 无
- **返回值**：`boolean` - 锁定状态
- **描述**：返回活塞锁定状态

### `setPID(p, i, d)`
- **参数**：
  - `p` (`double`): 比例系数
  - `i` (`double`): 积分系数
  - `d` (`double`): 微分系数
- **返回值**：`void` -
- **描述**：设置PID控制器的参数

### `getPhysics`
- **参数**：
  - 无
- **返回值**：表，包含servomotor键以及companion键，内容分别为
  - `servomotor` [`ShipPhysics`](Common.md#shipphysics-type): 包含伺服电机的物理属性
  - `companion` [`ShipPhysics`](Common.md#shipphysics-type): 包含负载的物理属性
- **描述**：返回自身以及负载物理结构的物理状态

### `setOutputForce(scale)`
- **参数**：
  - `scale` (`double`): 力大小
- **返回值**：`void` - 
- **描述**：设置输出力大小


### `getDistance`
- **参数**：
  - 无
- **返回值**：`double` - 距离
- **描述**：获取负载离自身距离


### `setTargetValue(value)`
- **参数**：
  - `value` (`double`): 目标值
- **返回值**：`void` -
- **描述**：当活塞模式为距离模式时，目标值为轴承的目标距离m，速度模式时，设置目标速度m/s


### `getTargetValue`
- **参数**：
  - 无
- **返回值**：`double` -
- **描述**：获取当前活塞的目标值

### `getCurrentValue`
- **参数**：
  - 无
- **返回值**：`double` -
- **描述**：获取活塞的当前值，位置模式时，返回值为距离m，速度模式时，返回速度m/s，锁定时该项不会更新


