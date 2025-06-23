
## **Vector3d** Type
- `Vector3d` 类型是一个 Lua 表，用于封装 Java 中的 `Vector3d` 类型，它是一个三维向量，用于表示 3D 空间中的位置或方向.
```lua
v = {x = 1.0, y = 2.0, z = 3.0}
print(v.x) -- 1.0
```

## **Quaternion** Type
- `Quaternion` 类型是一个 Lua 表，用于封装 Java 中的 Quaternion 类型，它用于表示 3D 空间中的旋转。
- 它由四个分量组成：x、y、z 和 w。
```lua
q = {x = 0.0, y = 0.0, z = 0.0, w = 1.0}
print(q.x) -- 0.0
```

## **Matrix3d** Type
- `Matrix3d` 类型是一个 Lua 表，用于封装 Java 中的 Matrix3d 类型，它是一个 3x3 矩阵，用于表示 3D 空间中的旋转或变换。
- 它由三个三元素数组，共九个分量组成，每个分量表示矩阵中的一个元素。
```lua
m = {{0.0, 1.0, 2.0}, {4.0, 5.0, 6.0}, {7.0, 8.0, 9.0}}
print(m[1][1]) -- 0.0
```

## **AABB** Type

- `AABB` 类型是一个 Lua 表，用于封装 Java 中的轴对齐包围盒（AABB）类型，它用于表示 3D 空间中的包围盒。
- 它由两个分量组成：min 和 max，两者均为 Vector3d 类型，分别表示包围盒的最小和最大角。
```lua
aabb = {
  min = {}, -- a Vector3d
  max = {}  -- a Vector3d
}
print(q.x) -- 0.0
```

## **ShipPhysics** Type

- `ShipPhysics` 类型是一个 Lua 表，用于封装游戏中船只的物理属性，由某些外设方法返回。
- 表包括
    - `velocity`: [(`Vector3d`)](#vector3d-type) 船只在绝对坐标系中的速度
    - `omega`: [(`Vector3d`)](#vector3d-type) 船只在绝对坐标系中的角速度
    - `position`: [(`Vector3d`)](#vector3d-type) 船只在绝对坐标系中的位置，
    - `positionInShip`: [(`Vector3d`)](#vector3d-type) 船只在造船厂中的位置，
    - `quaternion`: [(`Quaternion`)](#quaternion-type) 船只的旋转四元数
    - `up`: [(`Vector3d`)](#vector3d-type) 船只在绝对坐标系中的上方向
    - `mass`: (`double`) 质量,
    - `inertia`: (`double`) 转动惯量,
    - `id`: (`string`) ID.

- 以下是 ShipPhysics 表的结构，使用占位值来说明其格式:

```lua
sp = {
    velocity = { x = 1.0, y = 2.0, z = 3.0 },
    omega = { x = 0.0, y = 0.0, z = 0.0 },
    position = { x = 0.0, y = 0.0, z = 0.0 },
    positionInShip = { x = 0.0, y = 0.0, z = 0.0 },
    quaternion = { x = 0.0, y = 0.0, z = 0.0, w = 0.0 },
    up = { x = 0.0, y = 0.0, z = 0.0 },
    mass = 0.0,
    inertia = 0.0,
    id = ""
}
print(sp.velocity.x) -- 1.0 
```

## **BlockHitResult** Type
- `BlockHitResult` 类型是一个 Lua 表，用于封装游戏中方块击中的结果，由某些外设方法返回。
- 表包括：
    - `hit`: [(`Vector3d`)](#vector3d-type) 射线检测命中点坐标
    - `direction`: (`string`) 命中点位于方块的哪个面，可取值 "UP", "DOWN", "NORTH", "SOUTH", "WEST", 或者 "EAST".

```lua
result = {
    hit = {x = 1.0, y = 2.0, z = 3.0},
    direction = "UP"
}
```

## **BlockDetailResult** Type

- `BlockDetailResult` 类型是一个 Lua 表，用于封装游戏中方块击中的详细结果，由某些外设方法返回。
- The table includes:
    - `hit`: [(`Vector3d`)](#vector3d-type) 射线检测命中点坐标
    - `direction`: (`string`) 命中点位于方块的哪个面，可取值 "UP", "DOWN", "NORTH", "SOUTH", "WEST", 或者 "EAST".
    - `onShip`: (`boolean`) 表示该位置是否位于船上
    - `shipHitResult`: [(`ShipHitResult`)](#shiphitresult-type) 若方块在船上，则为船信息，如果没有检测到，则为 `nil` 。
```lua
result = {
    hit = {x = 1.0, y = 2.0, z = 3.0},
    direction = "UP",
    onShip = "false", -- or "true"
    shipHitResult = nil -- or a ShipHitResult Type table
}
```

## **EntityHitResult** Type

- `EntityHitResult` 类型是一个 Lua 表，用于封装游戏中实体击中的结果，由某些外设方法返回。
- 表包括
    - `hit`: [(`Vector3d`)](#vector3d-type) 摄像头射线命中实体时的命中点坐标.
    - `type`: (`string`) 实体类型字符串,
    - `name`: (`string`) 实体名称.
    - `health`: (`double`) 实体剩余生命值
    - `velocity`: [(`Vector3d`)](#vector3d-type) 被击中的实体此时的速度，如果为latestXX调用返回的结果，则为调用时速度
    - `position`: [(`Vector3d`)](#vector3d-type) 被击中的实体此时的位置，如果为latestXX调用返回的结果，则为调用时位置

```lua
result = {
    hit = {x = 1.2, y = 2.5, z = 3.3},
    type = "",
    name = "",
    health = 0.0,
    velocity = {x = 0.0, y = 0.0, z = 0.0},
    position = {x = 1.0, y = 2.0, z = 3.0}
}
```

## **EntityResult** Type

- `EntityResult` type is a Lua table that encapsulates the result of an entity found in the game, as returned by certain peripheral methods.
- The table includes:
    - `type`: (`string`) 实体类型字符串,
    - `name`: (`string`) 实体名称.
    - `health`: (`double`) 实体剩余生命值
    - `velocity`: [(`Vector3d`)](#vector3d-type) 被击中的实体此时的速度，如果为latestXX调用返回的结果，则为调用时速度
    - `position`: [(`Vector3d`)](#vector3d-type) 被击中的实体此时的位置，如果为latestXX调用返回的结果，则为调用时位置

```lua
result = {
    type = "",
    name = "",
    health = 0.0,
    velocity = {x = 0.0, y = 0.0, z = 0.0},
    position = {x = 1.0, y = 2.0, z = 3.0}
}
```

## **ShipHitResult** Type

- `ShipHitResult` type is a Lua table that encapsulates the result of a ship hit in the game, as returned by certain peripheral methods.
- The table includes:
    - `hit`: [(`Vector3d`)](#vector3d-type) 摄像头射线命中船时的命中点坐标.
    - `AABB`: [(`AABB`)](#aabb-type) 船的AABB
    - `velocity`: [(`Vector3d`)](#vector3d-type) 被击中的船此时的速度，如果为latestXX调用返回的结果，则为调用时速度
    - `position`: [(`Vector3d`)](#vector3d-type) 被击中的船此时的位置，如果为latestXX调用返回的结果，则为调用时位置
```lua
result = {
    hit = {x = 1.2, y = 2.5, z = 3.3},
    AABB = {}, -- a table, AABB Type
    velocity = {x = 0.0, y = 0.0, z = 0.0},
    position = {x = 1.0, y = 2.0, z = 3.0}
}
```

## **ShipResult** Type

- `ShipHitResult` type is a Lua table that encapsulates the result of a ship hit in the game, as returned by certain peripheral methods.
- The table includes:
    - `slug`: (`string`) 船只名称.
    - `AABB`: [(`AABB`)](#aabb-type) AABB.
    - `velocity`: [(`Vector3d`)](#vector3d-type) 被击中的船此时的速度，如果为latestXX调用返回的结果，则为调用时速度
    - `position`: [(`Vector3d`)](#vector3d-type) 被击中的船此时的位置，如果为latestXX调用返回的结果，则为调用时位置
```lua
result = {
    slug = "",
    AABB = {}, -- a table, AABB Type
    velocity = {x = 0.0, y = 0.0, z = 0.0},
    position = {x = 1.0, y = 2.0, z = 3.0}
}
```