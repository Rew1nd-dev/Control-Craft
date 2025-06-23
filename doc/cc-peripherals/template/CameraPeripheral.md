# CameraPeripheral Methods

This document describes the Lua methods available in the `CameraPeripheral` peripheral.

### `reset`
- **参数**：
  - 无
- **返回值**：`void` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `getAbsViewTransform`
- **参数**：
  - 无
- **返回值**：`Map` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `getLocViewTransform`
- **参数**：
  - 无
- **返回值**：`Map` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `clipNewEntityInView`
- **参数**：
  - 无
- **返回值**：`void` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `clipNewServerPlayer`
- **参数**：
  - 无
- **返回值**：`void` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `clip`
- **参数**：
  - 无
- **返回值**：`Map` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `getPitch`
- **参数**：
  - 无
- **返回值**：`double` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `getMobs(radius)`
- **参数**：
  - `radius` (`double`): 
- **返回值**：`List` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `raycast(x_0, y_0, z_0, x_1, y_1, z_1)`
- **参数**：
  - `x_0` (`double`): 
  - `y_0` (`double`): 
  - `z_0` (`double`): 
  - `x_1` (`double`): 
  - `y_1` (`double`): 
  - `z_1` (`double`): 
- **返回值**：`Map` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `clipEntity`
- **参数**：
  - 无
- **返回值**：`Map` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `latestShip`
- **参数**：
  - 无
- **返回值**：`Map` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `setYaw(degree)`
- **参数**：
  - `degree` (`double`): 
- **返回值**：`void` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `getYaw`
- **参数**：
  - 无
- **返回值**：`double` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `setPitch(degree)`
- **参数**：
  - `degree` (`double`): 
- **返回值**：`void` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `clipPlayerDetail`
- **参数**：
  - 无
- **返回值**：`Map` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `getCameraPosition`
- **参数**：
  - 无
- **返回值**：`Map` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `clipShipDetail`
- **参数**：
  - 无
- **返回值**：`Map` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `setClipRange(range)`
- **参数**：
  - `range` (`double`): 
- **返回值**：`void` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `outlineToUser(x, y, z, direction, color, slot)`
- **参数**：
  - `x` (`double`): 
  - `y` (`double`): 
  - `z` (`double`): 
  - `direction` (`String`): 
  - `color` (`int`): 
  - `slot` (`String`): 
- **返回值**：`void` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `getLocViewForward`
- **参数**：
  - 无
- **返回值**：`Map` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `getAbsViewForward`
- **参数**：
  - 无
- **返回值**：`Map` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `clipNewShip`
- **参数**：
  - 无
- **返回值**：`void` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `forcePitchYaw(pitch, yaw)`
- **参数**：
  - `pitch` (`double`): 
  - `yaw` (`double`): 
- **返回值**：`void` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `latestPlayer`
- **参数**：
  - 无
- **返回值**：`Map` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `latestEntity`
- **参数**：
  - 无
- **返回值**：`Map` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `latestBlock`
- **参数**：
  - 无
- **返回值**：`Map` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `getEntities(radius)`
- **参数**：
  - `radius` (`double`): 
- **返回值**：`List` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `clipBlockDetail`
- **参数**：
  - 无
- **返回值**：`Map` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `getDirection`
- **参数**：
  - 无
- **返回值**：`String` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `getClipDistance`
- **参数**：
  - 无
- **返回值**：`double` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `clipNewEntity`
- **参数**：
  - 无
- **返回值**：`void` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `setConeAngle(angle)`
- **参数**：
  - `angle` (`double`): 
- **返回值**：`void` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `clipEntityDetail`
- **参数**：
  - 无
- **返回值**：`Map` - 
- **描述**：
- **示例**：
  ```lua

  ```

### `clipNewBlock`
- **参数**：
  - 无
- **返回值**：`void` - 
- **描述**：
- **示例**：
  ```lua

  ```


