
## **Vector3d** Type
- `Vector3d` type is a Lua table that encapsulates a Vector3d type in java, which is a three-dimensional vector used to represent positions or directions in 3D space.
```lua
v = {x = 1.0, y = 2.0, z = 3.0}
print(v.x) -- 1.0
```

## **Quaternion** Type
- `Quaternion` type is a Lua table that encapsulates a Quaternion type in java, which is used to represent rotations in 3D space.
- It consists of four components: x, y, z, and w.
```lua
q = {x = 0.0, y = 0.0, z = 0.0, w = 1.0}
print(q.x) -- 0.0
```

## **Matrix3d** Type
- `Matrix3d` type is a Lua table that encapsulates a Matrix3d type in java, which is a 3x3 matrix used to represent rotations or transformations in 3D space.
- It consists of nine components, each representing an element of the matrix.
```lua
m = {{0.0, 1.0, 2.0}, {4.0, 5.0, 6.0}, {7.0, 8.0, 9.0}}
print(m[1][1]) -- 0.0
```

## **AABB** Type

- `AABB` type is a Lua table that encapsulates an Axis-Aligned Bounding Box (AABB) type in java, which is used to represent a bounding box in 3D space.
- It consists of two components: `min` and `max`, both of which are `Vector3d` types representing the minimum and maximum corners of the bounding box.
```lua
aabb = {
  min = {}, -- a Vector3d
  max = {}  -- a Vector3d
}
print(q.x) -- 0.0
```

## **ShipPhysics** Type

- `ShipPhysics` type is a Lua table that encapsulates the physical properties of a ship in the game, as returned by certain peripheral methods. 
- The table includes 
  - `velocity`: [(`Vector3d`)](#vector3d-type) Ship Velocity At Absolute Coordinate,
  - `omega`: [(`Vector3d`)](#vector3d-type) Ship Angular Velocity At Absolute Coordinate,
  - `position`: [(`Vector3d`)](#vector3d-type) Ship Position At Absolute Coordinate,
  - `positionInShip`: [(`Vector3d`)](#vector3d-type) Ship Position At Shipyard,
  - `quaternion`: [(`Quaternion`)](#quaternion-type) Ship Orientation At Absolute Coordinate,
  - `up`: [(`Vector3d`)](#vector3d-type) Ship Up Direction At Absolute Coordinate,
  - `mass`: (`double`) Ship Mass,
  - `inertia`: (`double`) Ship Inertia,
  - `id`: (`string`) Ship ID.
  
- Below is the structure of the `ShipPhysics` table, with placeholder values to illustrate its format:

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
- `BlockHitResult` type is a Lua table that encapsulates the result of a block hit in the game, as returned by certain peripheral methods.
- The table includes:
    - `hit`: [(`Vector3d`)](#vector3d-type) The coordinates of the hit point.
    - `direction`: (`string`) The direction of the hit, which can be "UP", "DOWN", "NORTH", "SOUTH", "WEST", or "EAST".

```lua
result = {
    hit = {x = 1.0, y = 2.0, z = 3.0},
    direction = "UP"
}
```

## **BlockDetailResult** Type

- `BlockDetailResult` type is a Lua table that encapsulates the result of a block hit in the game, as returned by certain peripheral methods.
- The table includes:
    - `hit`: [(`Vector3d`)](#vector3d-type) The coordinates of the hit point.
    - `direction`: (`string`) The direction of the hit, which can be "UP", "DOWN", "NORTH", "SOUTH", "WEST", or "EAST".
    - `onShip`: (`string`) A string indicating whether the hit is on a ship ("true" or "false").
    - `shipHitResult`: [(`ShipHitResult`)](#shiphitresult-type) The result of the ship hit, if applicable. It can be `nil` if there is no ship hit.
```lua
result = {
    hit = {x = 1.0, y = 2.0, z = 3.0},
    direction = "UP",
    onShip = "false", -- or "true"
    shipHitResult = nil -- or a ShipHitResult Type table
}
```

## **EntityHitResult** Type

- `EntityHitResult` type is a Lua table that encapsulates the result of an entity hit in the game, as returned by certain peripheral methods.
- The table includes:
    - `hit`: [(`Vector3d`)](#vector3d-type) The coordinates of the view hit point.
    - `type`: (`string`) The type of the EntityType<?>, 
    - `name`: (`string`) The name of the entity hit.
    - `health`: (`double`) The health of the entity hit.
    - `velocity`: [(`Vector3d`)](#vector3d-type) The velocity of the entity at the time when you get this result, not when it's hit (hit once, get realtime value forever).
    - `position`: [(`Vector3d`)](#vector3d-type) The position of the entity at the time when you get this result, not when it's hit.

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
  - `type`: (`string`) The type of the EntityType<?>,
  - `name`: (`string`) The name of the entity hit.
  - `health`: (`double`) The health of the entity hit.
  - `velocity`: [(`Vector3d`)](#vector3d-type) The velocity of the entity at the time when you get this result, not when it's hit.
  - `position`: [(`Vector3d`)](#vector3d-type) The position of the entity at the time when you get this result, not when it's hit.

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
    - `hit`: [(`Vector3d`)](#vector3d-type) The coordinates of the view hit point.
    - `AABB`: [(`AABB`)](#aabb-type) The Axis-Aligned Bounding Box of the ship at the time of the hit.
    - `velocity`: [(`Vector3d`)](#vector3d-type) The velocity of the ship at the time when you get this result, not when it's hit
    - `position`: [(`Vector3d`)](#vector3d-type) The position of the ship at the time when you get this result, not when it's hit
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
    - `slug`: (`string`) The slug of the ship, which is a unique identifier.
  - `AABB`: [(`AABB`)](#aabb-type) The Axis-Aligned Bounding Box of the ship at the time of the hit.
  - `velocity`: [(`Vector3d`)](#vector3d-type) The velocity of the ship at the time of when you get this result, not when it's hit
  - `position`: [(`Vector3d`)](#vector3d-type) The position of the ship at the time of when you get this result, not when it's hit
```lua
result = {
    slug = "",
    AABB = {}, -- a table, AABB Type
    velocity = {x = 0.0, y = 0.0, z = 0.0},
    position = {x = 1.0, y = 2.0, z = 3.0}
}
```