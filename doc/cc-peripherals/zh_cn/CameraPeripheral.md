# CameraPeripheral Methods

This document describes the Lua methods available in the `CameraPeripheral` peripheral.

#### 摄像头本体坐标系：
- +Z轴为其视线正前方，+X轴为其右侧

#### 摄像头的“视野”：
- ...

#### 摄像头的“视线”：
- ...

### `reset`
- **参数**：
  - 无
- **返回值**：`void` - 
- **描述**：将摄像头的视角调整为正对方块朝向


### `getAbsViewTransform`
- **参数**：
  - 无
- **返回值**：[`Quaternion`](Common.md): 获取摄像头 [`本体坐标系`](#摄像头本体坐标系) 相对世界坐标系的旋转矩阵
- **描述**：


### `getLocViewTransform`
- **参数**：
  - 无
- **返回值**：[`Quaternion`](Common.md): 获取摄像头 [`本体坐标系`](#摄像头本体坐标系) 相对船坐标系的旋转矩阵
- **描述**：
- **示例**：
  ```lua

  ```

### `clipNewEntityInView`
- **参数**：
  - 无
- **返回值**：`void`
- **描述**：让摄像头安排一次“寻找[`视野`](#摄像头的视野)内最近的实体”射线检测任务，下一游戏tick执行，结果保存在方块中
- **参考**： [`使用latestEntity获取结果`](#latestentity)


### `clipNewServerPlayer`
- **参数**：
  - 无
- **返回值**：`void` - 
- **描述**：让摄像头安排一次“检测[`视线`](#摄像头的视线)中的玩家”任务，下一游戏tick执行，结果保存在方块中
- **参考**： [`使用latestPlayer获取结果`](#latestplayer)

### `clip`
- **参数**：
  - 无
- **返回值**：[`BlockHitResult`](Common.md#blockhitresult-type)
- **描述**：立即获取摄像头 [`视线`](#摄像头的视线) 中的方块，会阻塞Lua线程


### `getPitch`
- **参数**：
  - 无
- **返回值**：`double` - 
- **描述**：返回当前摄像头俯仰角


### `getMobs(radius)`
- **参数**：
  - `radius` (`double`): 半径
- **返回值**：`List` - 返回内容为[EntityResult](Common.md#entityresult-type)类型的列表
- **描述**：获取半径内所有怪物


### `raycast(x_0, y_0, z_0, x_1, y_1, z_1)`
- **参数**：
  - `x_0` (`double`): 射线检测起点x
  - `y_0` (`double`): 射线检测起点y
  - `z_0` (`double`): 射线检测起点z
  - `x_1` (`double`): 射线检测终点x
  - `y_1` (`double`): 射线检测终点y
  - `z_1` (`double`): 射线检测终点z
- **返回值**：[`BlockHitResult`](Common.md#blockhitresult-type)
- **描述**：执行自定义起点和终点的射线检测


### `clipEntity`
- **参数**：
  - 无
- **返回值**：[EntityHitResult](Common.md#entityhitresult-type) 
- **描述**：立即获取摄像头 [`视线`](#摄像头的视线) 中的实体，会阻塞Lua线程


### `latestShip`
- **参数**：
  - 无
- **返回值**：[ShipHitResult](Common.md#shiphitresult-type)
- **描述**： 获取上一次进行船只射线检测的结果，如果未检测到，则返回nil


### `setYaw(degree)`
- **参数**：
  - `degree` (`double`): 角度，范围为`[-180, 180]`
- **返回值**：`void` - 
- **描述**：设置摄像头偏航角


### `getYaw`
- **参数**：
  - 无
- **返回值**：`double` - 角度，范围为`[-180, 180]`
- **描述**：获取当前摄像头偏航角


### `setPitch(degree)`
- **参数**：
  - `degree` (`double`): 
- **返回值**：`void` - 
- **描述**：设置摄像头俯仰角，范围为`[-90, 90]`


### `clipPlayer`
- **参数**：
  - 无
- **返回值**：[`EntityHitResult`](Common.md#entityhitresult-type)
- **描述**：获取仅限于玩家的射线检测结果，摄像头 [`视线`](#摄像头的视线) 中的玩家实体，会阻塞Lua线程


### `getCameraPosition`
- **参数**：
  - 无
- **返回值**：[`Vector3d`](Common.md#vector3d-type)
- **描述**：获取摄像头的绝对位置


### `clipShip`
- **参数**：
  - 无
- **返回值**：[`ShipHitResult`](Common.md#shiphitresult-type)
- **描述**： 获取摄像头 [`视线`](#摄像头的视线) 中的船只，会阻塞Lua线程


### `setClipRange(range)`
- **参数**：
  - `range` (`double`): 距离
- **返回值**：`void` - 
- **描述**：设置射线检测的最大距离



### `getLocViewForward`
- **参数**：
  - 无
- **返回值**：[`Vector3d`](Common.md#vector3d-type) 
- **描述**： 获取摄像头 [`视线`](#摄像头的视线)在船坐标系中的向量


### `getAbsViewForward`
- **参数**：
  - 无
- **返回值**：[`Vector3d`](Common.md#vector3d-type)
- **描述**：获取摄像头 [`视线`](#摄像头的视线)在世界坐标系中的向量


### `clipNewShip`
- **参数**：
  - 无
- **返回值**：`void` - 
- **描述**： 让摄像头安排一次“检测[`视线`](#摄像头的视线)中的船只”任务，下一游戏tick执行，结果保存在方块中


### `forcePitchYaw(pitch, yaw)`
- **参数**：
  - `pitch` (`double`): 俯仰角
  - `yaw` (`double`): 偏航角
- **返回值**：`void` - 
- **描述**：强制设置摄像头的俯仰角和偏航角，范围为`[-90, 90]`和`[-180, 180]`，当摄像头被玩家使用时也有效


### `latestPlayer`
- **参数**：
  - 无
- **返回值** [`EntityHitResult`](Common.md#entityhitresult-type)
- **描述**： 获取上一次进行玩家射线检测的结果，如果未检测到，则返回nil


### `latestEntity`
- **参数**：
  - 无
- **返回值**：[`EntityHitResult`](Common.md#entityhitresult-type)
- **描述**：获取上一次进行实体射线检测的结果，如果未检测到，则返回nil


### `latestBlock`
- **参数**：
  - 无
- **返回值**：[`BlockHitResult`](Common.md#blockhitresult-type) 
- **描述**：获取上一次进行方块射线检测的结果，如果未检测到，则返回nil


### `getEntities(radius)`
- **参数**：
  - `radius` (`double`): 
- **返回值**：`List` - 返回一个包含[`EntityResult`](Common.md#entityresult-type)的列表
- **描述**：获取半径内所有活的实体



### `getDirection`
- **参数**：
  - 无
- **返回值**：`String` - 方向的枚举值
- **描述**： 获取摄像头的朝向，返回值为`"NORTH"`、`"SOUTH"`、`"EAST"`、`"WEST"`、`"UP"`或`"DOWN"`


### `getClipDistance`
- **参数**：
  - 无
- **返回值**：`double` - 距离
- **描述**：获取摄像头的射线检测最大距离


### `clipNewEntity`
- **参数**：
  - 无
- **返回值**：`void` - 
- **描述**： 让摄像头安排一次“寻找[`视线`](#摄像头的视线)中的实体”射线检测任务，下一游戏tick执行，结果保存在方块中


### `setConeAngle(angle)`
- **参数**：
  - `angle` (`double`): 弧度值，范围为`[0, π/2]`
- **返回值**：`void` - 
- **描述**：设置摄像头的视锥角度


### `clipAllEntity`
- **参数**：
  - 无
- **返回值**：[`EntityHitResult`](Common.md#entityhitresult-type)
- **描述**：获取摄像头 [`视线`](#摄像头的视线) 中的实体（任意类型），会阻塞Lua线程


### `clipNewBlock`
- **参数**：
  - 无
- **返回值**：`void` - 
- **描述**： 让摄像头安排一次“检测[`视线`](#摄像头的视线)中的方块”任务，下一游戏tick执行，结果保存在方块中



