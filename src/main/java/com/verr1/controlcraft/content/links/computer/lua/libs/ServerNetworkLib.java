package com.verr1.controlcraft.content.links.computer.lua.libs;

import com.verr1.controlcraft.content.links.computer.ComputerAuthoritySnapshot;
import com.verr1.controlcraft.content.links.computer.ComputerNetworkHandler;
import com.verr1.controlcraft.content.links.computer.lua.IComputerServerContext;
import com.verr1.controlcraft.content.links.computer.lua.LuaUtils;
import org.jetbrains.annotations.Nullable;
import org.luaj.vm2.LuaTable;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.Varargs;
import org.luaj.vm2.lib.OneArgFunction;
import org.luaj.vm2.lib.TwoArgFunction;
import org.luaj.vm2.lib.VarArgFunction;
import org.luaj.vm2.lib.ZeroArgFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class ServerNetworkLib extends TwoArgFunction {

    private final IComputerServerContext context;
    private final ComputerNetworkHandler syncManager;

    public ServerNetworkLib(IComputerServerContext context) {
        this.context = context;
        this.syncManager = context.getNetworkHandler();
    }

    @Override
    public LuaValue call(LuaValue modname, LuaValue env) {
        LuaTable root = new LuaTable();
        LuaTable state = buildStateLibrary();
        LuaTable event = buildEventLibrary();
        LuaTable authority = buildAuthorityLibrary();

        root.set("state", state);
        root.set("event", event);
        root.set("authority", authority);

        root.set("isDirty", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue slot) {
                return LuaValue.valueOf(syncManager.isStateChanged(slot.checkjstring()));
            }
        });
        root.set("peek", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue slot) {
                return LuaUtils.toLuaValue(syncManager.getState(slot.checkjstring()));
            }
        });
        root.set("retrieve", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue slot) {
                return LuaUtils.toLuaValue(syncManager.consumeState(slot.checkjstring()));
            }
        });
        root.set("set", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                String slot = args.arg1().checkjstring();
                LuaValue value = args.arg(2);
                if (value.isnil()) {
                    syncManager.clearState(slot);
                    return LuaValue.NONE;
                }

                syncManager.setState(slot, LuaUtils.toJavaValue(value));
                return LuaValue.NONE;
            }
        });
        root.set("clear", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue slot) {
                syncManager.clearState(slot.checkjstring());
                return NIL;
            }
        });
        root.set("touch", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue slot) {
                return LuaValue.valueOf(syncManager.touchState(slot.checkjstring()));
            }
        });
        root.set("setAuthority", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                return forceAuthority(args.arg1());
            }
        });
        root.set("clearAuthority", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                return clearForcedAuthority();
            }
        });
        root.set("getAuthority", new ZeroArgFunction() {
            @Override
            public LuaValue call() {
                return toLuaAuthority(context.getServerNetworkAuthoritySnapshot());
            }
        });

        env.set("Network", root);
        return root;
    }

    private LuaTable buildStateLibrary() {
        LuaTable state = new LuaTable();

        state.set("get", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue key) {
                return LuaUtils.toLuaValue(syncManager.getState(key.checkjstring()));
            }
        });
        state.set("getMany", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue keys) {
                LuaTable result = new LuaTable();
                for (String key : luaArrayToStrings(keys.checktable())) {
                    result.set(key, LuaUtils.toLuaValue(syncManager.getState(key)));
                }
                return result;
            }
        });
        state.set("consume", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue key) {
                return LuaUtils.toLuaValue(syncManager.consumeState(key.checkjstring()));
            }
        });
        state.set("changed", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue key) {
                return LuaValue.valueOf(syncManager.isStateChanged(key.checkjstring()));
            }
        });
        state.set("revision", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue key) {
                return luaLong(syncManager.getStateRevision(key.checkjstring()));
            }
        });
        state.set("set", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                String key = args.arg1().checkjstring();
                LuaValue value = args.arg(2);
                if (value.isnil()) {
                    throw new org.luaj.vm2.LuaError("Network.state.set does not accept nil; use Network.state.clear(key)");
                }
                syncManager.setState(key, LuaUtils.toJavaValue(value));
                return LuaValue.NONE;
            }
        });
        state.set("setMany", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue values) {
                LuaTable table = values.checktable();
                LuaValue key = NIL;
                while (true) {
                    Varargs next = table.next(key);
                    key = next.arg1();
                    if (key.isnil()) {
                        break;
                    }
                    syncManager.setState(luaKeyToString(key), LuaUtils.toJavaValue(table.get(key)));
                }
                return NIL;
            }
        });
        state.set("clear", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue key) {
                syncManager.clearState(key.checkjstring());
                return NIL;
            }
        });
        state.set("touch", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue key) {
                return LuaValue.valueOf(syncManager.touchState(key.checkjstring()));
            }
        });

        return state;
    }

    private LuaTable buildEventLibrary() {
        LuaTable event = new LuaTable();

        event.set("emit", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                String name = args.arg1().checkjstring();
                LuaValue payload = args.arg(2);
                Object javaPayload = payload.isnil() ? null : LuaUtils.toJavaValue(payload);
                ComputerNetworkHandler.EventEntry entry = syncManager.emitOutboundEvent(
                        name,
                        javaPayload,
                        context.getComputerGameTick(),
                        ComputerNetworkHandler.EventSender.server()
                );

                LuaTable info = new LuaTable();
                info.set("state", LuaValue.valueOf("sent"));
                info.set("name", LuaValue.valueOf(name));
                info.set("seq", luaLong(entry.seq()));
                return LuaValue.varargsOf(new LuaValue[]{LuaValue.TRUE, info});
            }
        });
        event.set("peek", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue name) {
                return eventToLua(syncManager.peekEvent(name.checkjstring()));
            }
        });
        event.set("poll", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue name) {
                return eventToLua(syncManager.pollEvent(name.checkjstring()));
            }
        });
        event.set("backlog", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue name) {
                return LuaValue.valueOf(syncManager.eventBacklog(name.checkjstring()));
            }
        });
        event.set("clear", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue name) {
                syncManager.clearEventQueue(name.checkjstring());
                return NIL;
            }
        });

        return event;
    }

    private LuaTable buildAuthorityLibrary() {
        LuaTable authority = new LuaTable();

        authority.set("get", new ZeroArgFunction() {
            @Override
            public LuaValue call() {
                return toLuaAuthority(context.getServerNetworkAuthoritySnapshot());
            }
        });
        authority.set("force", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                return forceAuthority(args.arg1());
            }
        });
        authority.set("clearForced", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                return clearForcedAuthority();
            }
        });

        return authority;
    }

    private Varargs forceAuthority(LuaValue playerUuid) {
        if (playerUuid.isnil()) {
            return result(false, info("reason", "missing_uuid"));
        }

        try {
            UUID playerId = UUID.fromString(playerUuid.checkjstring());
            boolean ok = context.setForcedNetworkAuthority(playerId);
            return result(ok, authorityResult(ok ? "forced" : "denied", ok ? null : "force_failed"));
        } catch (IllegalArgumentException ex) {
            return result(false, info("state", "denied", "reason", "invalid_uuid"));
        }
    }

    private Varargs clearForcedAuthority() {
        boolean ok = context.clearForcedNetworkAuthority();
        return result(ok, authorityResult(ok ? "released" : "denied", ok ? null : "clear_failed"));
    }

    private LuaValue toLuaAuthority(ComputerAuthoritySnapshot snapshot) {
        if (snapshot == null || !snapshot.isActive()) {
            return NIL;
        }

        LuaTable table = new LuaTable();
        table.set("state", LuaValue.valueOf("owned"));
        table.set("mode", LuaValue.valueOf(snapshot.mode().serializedName()));
        table.set("ownerUuid", LuaValue.valueOf(snapshot.ownerId().toString()));
        table.set("leaseExpireTick", luaLong(snapshot.leaseExpireTick()));
        table.set("token", luaLong(snapshot.token()));
        return table;
    }

    private LuaTable authorityResult(String state, @Nullable String reason) {
        LuaTable table = new LuaTable();
        table.set("state", LuaValue.valueOf(state));
        ComputerAuthoritySnapshot snapshot = context.getServerNetworkAuthoritySnapshot();
        if (snapshot != null && snapshot.isActive()) {
            table.set("mode", LuaValue.valueOf(snapshot.mode().serializedName()));
            table.set("ownerUuid", LuaValue.valueOf(snapshot.ownerId().toString()));
            table.set("leaseExpireTick", luaLong(snapshot.leaseExpireTick()));
            table.set("token", luaLong(snapshot.token()));
        }
        if (reason != null) {
            table.set("reason", LuaValue.valueOf(reason));
        }
        return table;
    }

    private LuaValue eventToLua(@Nullable ComputerNetworkHandler.EventEntry event) {
        if (event == null) {
            return NIL;
        }

        LuaTable table = new LuaTable();
        table.set("seq", luaLong(event.seq()));
        table.set("name", LuaValue.valueOf(event.name()));
        table.set("payload", LuaUtils.toLuaValue(event.payload()));
        table.set("tick", luaLong(event.tick()));

        LuaTable sender = new LuaTable();
        sender.set("side", LuaValue.valueOf(event.sender().side()));
        if (event.sender().ownerUuid() != null) {
            sender.set("ownerUuid", LuaValue.valueOf(event.sender().ownerUuid().toString()));
        }
        table.set("sender", sender);
        return table;
    }

    private List<String> luaArrayToStrings(LuaTable table) {
        List<String> keys = new ArrayList<>();
        LuaValue key = NIL;
        while (true) {
            Varargs next = table.next(key);
            key = next.arg1();
            if (key.isnil()) {
                break;
            }
            keys.add(table.get(key).checkjstring());
        }
        return keys;
    }

    private String luaKeyToString(LuaValue key) {
        if (key.isstring()) {
            return key.checkjstring();
        }
        if (key.isnumber()) {
            double number = key.checkdouble();
            long integral = (long) number;
            return integral == number ? Long.toString(integral) : Double.toString(number);
        }
        if (key.isboolean()) {
            return Boolean.toString(key.toboolean());
        }
        return key.tojstring();
    }

    private Varargs result(boolean ok, LuaTable info) {
        return LuaValue.varargsOf(new LuaValue[]{LuaValue.valueOf(ok), info});
    }

    private LuaTable info(Object... fields) {
        LuaTable table = new LuaTable();
        for (int index = 0; index + 1 < fields.length; index += 2) {
            Object key = fields[index];
            Object value = fields[index + 1];
            if (key != null && value != null) {
                table.set(String.valueOf(key), LuaUtils.toLuaValue(value));
            }
        }
        return table;
    }

    private LuaValue luaLong(long value) {
        return LuaValue.valueOf((double) value);
    }
}
