# Control Chair Input Design

## Goal

Control Chair is a ship-mounted player control source with three layers:

1. Client-side input interpretation
2. Server-side session and input state storage
3. BlockEntity-side consumption from the 60 Hz physics/control pipeline

This document only covers the input/session model and the first Control Chair block scaffold.

## Key Decisions

- Mouse speed mode is computed on the client.
- View lock / unlock mode switching is client-local and does not require a packet.
- Network traffic uses sparse state updates:
  - changed axes send absolute values
  - buttons send press / release edges
- Server stores the latest semantic control state instead of reconstructing raw mouse movement.
- Input state is published from the session itself and must not be re-sampled through `BlockEntity.tickServer()`.
- Physics consumers should read the latest published frame directly from the active chair session.

## Input Split

### Continuous Axes

These values are semantic control channels, not raw device axes.

- `front_back`
- `left_right`
- `up_down`
- `roll`
- `yaw_rate`
- `pitch_rate`
- `throttle`
- `brake`

### Button State

Buttons are intentionally generic for now.

- `b0` to `b15`

Later UI/config work can bind devices to these semantic channels and generic button slots.

## Packet Model

Planned packet behavior:

- Client samples raw input at a high frequency.
- Client computes mouse-derived rate axes locally.
- Client sends only changed semantic axes plus button edges.
- Periodic heartbeat / full snapshot packets should still exist for recovery.

The packet payload should behave like:

- `sequence`
- `fullSnapshot`
- `changedAxisMask`
- `axisValues`
- `pressedButtonsMask`
- `releasedButtonsMask`
- optional `heldButtonsMask` for recovery/full snapshot

Important:

- Sparse updates must carry absolute axis values, not numeric diffs.
- Buttons should be represented as state edges and accumulated into held state on the server.

## Server Model

`ControlChairInputManager`

- binds player UUIDs to chair positions
- owns active `ControlChairSession`
- future packet handlers should feed deltas here

`ControlChairSession`

- identifies the controlling player and chair
- owns one `ControlChairInputTimeline`
- records last input timestamp

`ControlChairInputTimeline`

- applies sparse deltas
- stores latest axis values
- stores held / pressed / released button masks
- can return:
  - a snapshot of current state
  - a snapshot that also clears transient button edges

## BlockEntity Consumption

`ControlChairBlockEntity`

- detects the current seated player
- creates / removes the corresponding session binding
- exposes the latest published semantic axes and held buttons through a `Plant`

Current scaffold responsibilities:

- seat occupancy tracking
- session lifecycle
- read-only control outputs for future circuit/control integration
- client-side semantic sampling and sparse packet sending
- server-side off-main-thread packet merge into the session state

Not implemented yet:

- configurable bindings
- custom chair UI
- custom chair-specific seat entity

## Current Packet Pipeline

Implemented scaffold:

- `ControlChairClientInput`
  - runs a dedicated client sender loop
  - samples keyboard / mouse-button state
  - accumulates raw mouse turn deltas from a client mixin
  - computes semantic `yaw_rate` / `pitch_rate` on the client
  - sends sparse deltas plus heartbeat / full snapshots
- `ControlChairInputPacket`
  - transmits sparse absolute axis updates and button edge masks
- `ControlChairInputServerWorker`
  - receives packet payloads from the networking thread
  - merges them into `ControlChairSession` on a dedicated worker thread

Important:

- `ControlChairBlockEntity` is not in the packet path.
- `BlockEntity.tickServer()` must not be used as the publication clock for input state.

## Next Steps

1. Add packet types for semantic sparse input updates.
2. Feed `ControlChairInputManager.accept(...)` from packet handlers without routing through the main game thread for pure state writes.
3. Read chair input from the 60 Hz physics pipeline.
4. Add configurable binding data and UI.
