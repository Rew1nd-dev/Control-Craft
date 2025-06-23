# SpinalyzerPeripheral Methods

This document describes the Lua methods available in the `SpinalyzerPeripheral` peripheral.

### `getPosition`
- **参数**：
  - 无
- **返回值**：[`Vector3d`](Common.md#vector3d-type) 
- **描述**：获取所在船的质心位置


### `getPhysics`
- **参数**：
  - 无
- **返回值**：[`ShipPhysics`](Common.md#shipphysics-type)
- **描述**：返回当前船的物理状态


### `getRotationMatrix`
- **参数**：
  - 无
- **返回值**：[`Matrix3d`](Common.md#matrix3d-type)
- **描述**：获取当前船相对世界坐标系的旋转矩阵


### `getRotationMatrixT`
- **参数**：
  - 无
- **返回值**：[`Matrix3d`](Common.md#matrix3d-type)
- **描述**：获取当前船相对世界坐标系的旋转矩阵转置

### `getVelocity`
- **参数**：
  - 无
- **返回值**：[`Vector3d`](Common.md#vector3d-type)
- **描述**：获取当前船的质心速度


### `getQuaternionJ`
- **参数**：
  - 无
- **返回值**：[`Quaternion`](Common.md#quaternion-type) 
- **描述**：获取当前船的旋转四元数共轭

### `getQuaternion`
- **参数**：
  - 无
- **返回值**：[`Quaternion`](Common.md#quaternion-type)
- **描述**：获取当前船的旋转四元数

### `getAngularVelocity`
- **参数**：
  - 无
- **返回值**：[`Vector3d`](Common.md#vector3d-type)
- **描述**：获取当前船的旋转角速度

### `getSpinalyzerVelocity`
- **参数**：
  - 无
- **返回值**：[`Vector3d`](Common.md#vector3d-type)
- **描述**：获取船上旋转分析仪处位置的速度

### `getSpinalyzerPosition`
- **参数**：
  - 无
- **返回值**：[`Vector3d`](Common.md#vector3d-type)
- **描述**：获取旋转分析仪所在的绝对位置


