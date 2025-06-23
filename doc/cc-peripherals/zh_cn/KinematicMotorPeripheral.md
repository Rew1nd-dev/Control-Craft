# KinematicMotorPeripheral Methods

This document describes the Lua methods available in the `KinematicMotorPeripheral` peripheral.

### `getAngle`
- **参数**：
  - 无
- **返回值**：`double` - 角度
- **描述**：返回当前负载相对于轴承的角度rad


### `getPhysics`
- **参数**：
  - 无
- **返回值**：表，包含servomotor键以及companion键，内容分别为
  - `servomotor` [`ShipPhysics`](Common.md#shipphysics-type): 包含伺服电机的物理属性
  - `companion` [`ShipPhysics`](Common.md#shipphysics-type): 包含负载的物理属性
- **描述**：返回自身以及负载物理结构的物理状态

### `setControlTarget(target)`
- **参数**：
  - `target` (`double`): 目标值
- **返回值**：`void` - 
- **描述**：设置运动学轴承的控制目标，角度模式时为目标角度，角速度模式时为目标角速度


### `setIsForcingAngle(isForcingAngle)`
- **参数**：
  - `isForcingAngle` (`boolean`): 
- **返回值**：`void` - 
- **描述**：True->为强制角度模式 False->为强制角速度模式


### `setTargetAngle(angle)`
- **参数**：
  - `angle` (`double`): 
- **返回值**：`void` - 
- **描述**：设置目标角度，当轴承为角度模式时有效，角速度模式下会被马上覆盖


### `getControlTarget`
- **参数**：
  - 无
- **返回值**：`double` - 目标值
- **描述**：获取控制目标值


### `getTargetAngle`
- **参数**：
  - 无
- **返回值**：`double` - 
- **描述**：获取控制目标角度


### `getRelative`
- **参数**：
  - 无
- **返回值**：[Matrix3d](Common.md#matrix3d-type) - 旋转矩阵
- **描述**：返回负载本体坐标系相对轴承坐标系的旋转矩阵


