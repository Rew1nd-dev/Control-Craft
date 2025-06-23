# DynamicMotorPeripheral Methods

This document describes the Lua methods available in the `DynamicMotorPeripheral` peripheral.

### `lock`
- **参数**：
  - 无
- **返回值**：`void` - 
- **描述**：如果电机未锁定，则锁定电机


### `unlock`
- **参数**：
  - 无
- **返回值**：`void` - 
- **描述**：如果电机已锁定，则解锁电机


### `isLocked`
- **参数**：
  - 无
- **返回值**：`boolean` - 锁定状态
- **描述**：返回电机锁定状态


### `setIsAdjustingAngle(isAdjustingAngle)`
- **参数**：
  - `isAdjustingAngle` (`boolean`): 是否为调整角度模式
- **返回值**：`void` - 
- **描述**：设置电机是否处于调整角度模式


### `setPID(p, i, d)`
- **参数**：
  - `p` (`double`): 比例系数
  - `i` (`double`): 积分系数
  - `d` (`double`): 微分系数
- **返回值**：`void` - 
- **描述**：设置PID控制器的参数


### `getAngle`
- **参数**：
  - 无
- **返回值**：`double` - 弧度，范围为`[-π, π]`
- **描述**：获取当前伺服角


### `getPhysics`
- **参数**：
  - 无
- **返回值**：表，包含servomotor键以及companion键，内容分别为
  - `servomotor` [`ShipPhysics`](Common.md#shipphysics-type): 包含伺服电机的物理属性
  - `companion` [`ShipPhysics`](Common.md#shipphysics-type): 包含负载的物理属性
- **描述**：返回自身以及负载物理结构的物理状态



### `setOutputTorque(scale)`
- **参数**：
  - `scale` (`double`): 力矩大小
- **返回值**：`void` - 
- **描述**：设置轴承的附加输出力矩


### `setTargetValue(value)`
- **参数**：
  - `value` (`double`): 目标值
- **返回值**：`void` - 
- **描述**：当轴承模式为角度模式时，目标值为轴承的目标角度的弧度值rad,范围`[-π, π]`，角速度模式时，目标值为目标角速度rad/s


### `getTargetValue`
- **参数**：
  - 无
- **返回值**：`double` - 
- **描述**：获取当前轴承的目标值


### `getAngularVelocity`
- **参数**：
  - 无
- **返回值**：`double` - 角速度
- **描述**：获取当前轴承的角速度，单位为rad/s


### `getCurrentValue`
- **参数**：
  - 无
- **返回值**：`double` - 
- **描述**：获取轴承当前值，角度模式时，返回值为角度rad，角速度模式时，返回角速度rad/s，锁定时该项不会更新


### `getRelative`
- **参数**：
  - 无
- **返回值**：[Matrix3d](Common.md#matrix3d-type) - 旋转矩阵 
- **描述**：返回负载本体坐标系相对轴承坐标系的旋转矩阵



