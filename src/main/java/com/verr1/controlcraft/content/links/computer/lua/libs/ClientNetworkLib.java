package com.verr1.controlcraft.content.links.computer.lua.libs;

import com.verr1.controlcraft.content.links.computer.ComputerAuthorityMode;
import com.verr1.controlcraft.content.links.computer.ComputerAuthoritySnapshot;
import com.verr1.controlcraft.content.links.computer.ComputerNetworkHandler;
import com.verr1.controlcraft.content.links.computer.lua.IComputerClientContext;
import com.verr1.controlcraft.content.links.computer.lua.LuaUtils;
import com.verr1.controlcraft.foundation.network.packets.specific.ComputerClientNetworkPacket;
import com.verr1.controlcraft.registry.ControlCraftPackets;
import org.jetbrains.annotations.Nullable;
import org.luaj.vm2.LuaTable;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.Varargs;
import org.luaj.vm2.lib.OneArgFunction;
import org.luaj.vm2.lib.TwoArgFunction;
import org.luaj.vm2.lib.VarArgFunction;
import org.luaj.vm2.lib.ZeroArgFunction;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class ClientNetworkLib extends TwoArgFunction {

    private final IComputerClientContext context;
    private final ComputerNetworkHandler syncManager;

    public ClientNetworkLib(IComputerClientContext context) {
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
        root.set("send", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                String slot = args.arg1().checkjstring();
                LuaValue value = args.arg(2);
                if (value.isnil()) {
                    return dispatchPacket(ComputerClientNetworkPacket.stateClear(
                            context.getBlockPos(),
                            slot,
                            context.currentClientNetworkAuthorityToken()
                    ), "state_clear", slot, null);
                }

                return dispatchPacket(ComputerClientNetworkPacket.stateSet(
                        context.getBlockPos(),
                        slot,
                        LuaUtils.toJavaValue(value),
                        context.currentClientNetworkAuthorityToken()
                ), "state_set", slot, null);
            }
        });
        root.set("clear", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                String slot = args.arg1().checkjstring();
                return dispatchPacket(ComputerClientNetworkPacket.stateClear(
                        context.getBlockPos(),
                        slot,
                        context.currentClientNetworkAuthorityToken()
                ), "state_clear", slot, null);
            }
        });
        root.set("canSend", new ZeroArgFunction() {
            @Override
            public LuaValue call() {
                return LuaValue.valueOf(context.canClientSendNetwork());
            }
        });
        root.set("requestAuthority", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                return requestAuthority();
            }
        });
        root.set("releaseAuthority", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                return releaseAuthority();
            }
        });
        root.set("getAuthority", new ZeroArgFunction() {
            @Override
            public LuaValue call() {
                return authorityInfoOrNil();
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
        state.set("send", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                String key = args.arg1().checkjstring();
                LuaValue value = args.arg(2);
                if (value.isnil()) {
                    return result(false, info("reason", "invalid_nil", "key", key, "hint", "use Network.state.clear(key)"));
                }

                return dispatchPacket(ComputerClientNetworkPacket.stateSet(
                        context.getBlockPos(),
                        key,
                        LuaUtils.toJavaValue(value),
                        context.currentClientNetworkAuthorityToken()
                ), "state_set", key, null);
            }
        });
        state.set("sendMany", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                Map<String, Object> values = luaObjectTable(args.arg1().checktable());
                List<ComputerClientNetworkPacket> packets = new ArrayList<>(values.size());
                for (Map.Entry<String, Object> entry : values.entrySet()) {
                    packets.add(ComputerClientNetworkPacket.stateSet(
                            context.getBlockPos(),
                            entry.getKey(),
                            entry.getValue(),
                            context.currentClientNetworkAuthorityToken()
                    ));
                }
                return dispatchPackets(packets, "state_set_many", values.size());
            }
        });
        state.set("clear", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                String key = args.arg1().checkjstring();
                return dispatchPacket(ComputerClientNetworkPacket.stateClear(
                        context.getBlockPos(),
                        key,
                        context.currentClientNetworkAuthorityToken()
                ), "state_clear", key, null);
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
                long seq = syncManager.nextOutboundEventSeq(name);
                return dispatchPacket(
                        ComputerClientNetworkPacket.eventEmit(
                                context.getBlockPos(),
                                name,
                                javaPayload,
                                seq,
                                context.getComputerGameTick(),
                                context.currentClientNetworkAuthorityToken()
                        ),
                        "event_emit",
                        name,
                        seq
                );
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

        authority.set("canSend", new ZeroArgFunction() {
            @Override
            public LuaValue call() {
                return LuaValue.valueOf(context.canClientSendNetwork());
            }
        });
        authority.set("request", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                return requestAuthority();
            }
        });
        authority.set("release", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                return releaseAuthority();
            }
        });
        authority.set("get", new ZeroArgFunction() {
            @Override
            public LuaValue call() {
                return authorityInfoOrNil();
            }
        });

        return authority;
    }

    private Varargs requestAuthority() {
        if (context.canClientSendNetwork()) {
            return result(true, authorityInfo("owned", null));
        }

        ComputerAuthoritySnapshot snapshot = context.getClientNetworkAuthoritySnapshot();
        if (snapshot != null && snapshot.isActive()) {
            return result(false, authorityInfo("denied", snapshot.mode() == ComputerAuthorityMode.FORCED ? "forced_owner" : "leased_by_other"));
        }

        long cooldown = context.getClientNetworkAuthorityRequestCooldownTicks();
        if (cooldown > 0L) {
            LuaTable info = authorityInfo("cooldown", "request_cooldown");
            info.set("retryInTicks", luaLong(cooldown));
            return result(false, info);
        }

        if (context.requestClientNetworkAuthority()) {
            LuaTable info = authorityInfo("requested", null);
            info.set("retryInTicks", luaLong(context.getClientNetworkAuthorityRequestCooldownTicks()));
            return result(true, info);
        }

        return result(false, info("state", "denied", "reason", "request_failed"));
    }

    private Varargs releaseAuthority() {
        ComputerAuthoritySnapshot snapshot = context.getClientNetworkAuthoritySnapshot();
        if (snapshot == null || !snapshot.isActive()) {
            return result(false, info("state", "denied", "reason", "not_owned"));
        }
        if (snapshot.mode() != ComputerAuthorityMode.LEASED) {
            return result(false, info("state", "denied", "reason", "not_leased"));
        }
        if (!context.canClientSendNetwork()) {
            return result(false, info("state", "denied", "reason", "not_owner"));
        }
        if (context.releaseClientNetworkAuthority()) {
            return result(true, info("state", "released", "mode", snapshot.mode().serializedName()));
        }
        return result(false, info("state", "denied", "reason", "release_failed"));
    }

    private Varargs dispatchPackets(List<ComputerClientNetworkPacket> packets, String action, int count) {
        if (packets.isEmpty()) {
            return result(true, info("action", action, "count", 0));
        }

        for (ComputerClientNetworkPacket packet : packets) {
            if (packet.payloadBytes() > ComputerClientNetworkPacket.MAX_SINGLE_PAYLOAD_BYTES) {
                return result(false, info(
                        "action", action,
                        "reason", "payload_too_large",
                        "count", count,
                        "maxBytes", ComputerClientNetworkPacket.MAX_SINGLE_PAYLOAD_BYTES
                ));
            }
        }

        if (context.canClientSendNetwork()) {
            packets.forEach(packet -> ControlCraftPackets.getChannel().sendToServer(packet));
            return result(true, info("action", action, "count", count, "state", "owned"));
        }

        return failForMissingAuthority(action, null, null, count);
    }

    private Varargs dispatchPacket(ComputerClientNetworkPacket packet, String action, @Nullable String key, @Nullable Long seq) {
        if (packet.payloadBytes() > ComputerClientNetworkPacket.MAX_SINGLE_PAYLOAD_BYTES) {
            LuaTable info = info(
                    "action", action,
                    "reason", "payload_too_large",
                    "maxBytes", ComputerClientNetworkPacket.MAX_SINGLE_PAYLOAD_BYTES
            );
            if (key != null) {
                info.set("key", LuaValue.valueOf(key));
            }
            if (seq != null) {
                info.set("seq", luaLong(seq));
            }
            return result(false, info);
        }

        if (context.canClientSendNetwork()) {
            ControlCraftPackets.getChannel().sendToServer(packet);
            LuaTable info = info("action", action, "state", "owned");
            if (key != null) {
                info.set("key", LuaValue.valueOf(key));
            }
            if (seq != null) {
                info.set("seq", luaLong(seq));
            }
            return result(true, info);
        }

        return failForMissingAuthority(action, key, seq, null);
    }

    private Varargs failForMissingAuthority(String action, @Nullable String key, @Nullable Long seq, @Nullable Integer count) {
        ComputerAuthoritySnapshot snapshot = context.getClientNetworkAuthoritySnapshot();
        if (snapshot != null && snapshot.isActive()) {
            LuaTable info = authorityInfo("denied", snapshot.mode() == ComputerAuthorityMode.FORCED ? "forced_owner" : "leased_by_other");
            info.set("action", LuaValue.valueOf(action));
            if (key != null) {
                info.set("key", LuaValue.valueOf(key));
            }
            if (seq != null) {
                info.set("seq", luaLong(seq));
            }
            if (count != null) {
                info.set("count", LuaValue.valueOf(count));
            }
            return result(false, info);
        }

        long cooldown = context.getClientNetworkAuthorityRequestCooldownTicks();
        if (cooldown > 0L) {
            LuaTable info = info("action", action, "reason", "awaiting_authority", "state", "cooldown");
            info.set("retryInTicks", luaLong(cooldown));
            if (key != null) {
                info.set("key", LuaValue.valueOf(key));
            }
            if (seq != null) {
                info.set("seq", luaLong(seq));
            }
            if (count != null) {
                info.set("count", LuaValue.valueOf(count));
            }
            return result(false, info);
        }

        if (context.requestClientNetworkAuthority()) {
            LuaTable info = info("action", action, "reason", "awaiting_authority", "state", "requested");
            info.set("retryInTicks", luaLong(context.getClientNetworkAuthorityRequestCooldownTicks()));
            if (key != null) {
                info.set("key", LuaValue.valueOf(key));
            }
            if (seq != null) {
                info.set("seq", luaLong(seq));
            }
            if (count != null) {
                info.set("count", LuaValue.valueOf(count));
            }
            return result(false, info);
        }

        LuaTable info = info("action", action, "reason", "denied", "state", "denied");
        if (key != null) {
            info.set("key", LuaValue.valueOf(key));
        }
        if (seq != null) {
            info.set("seq", luaLong(seq));
        }
        if (count != null) {
            info.set("count", LuaValue.valueOf(count));
        }
        return result(false, info);
    }

    private LuaValue authorityInfoOrNil() {
        ComputerAuthoritySnapshot snapshot = context.getClientNetworkAuthoritySnapshot();
        long cooldown = context.getClientNetworkAuthorityRequestCooldownTicks();

        if ((snapshot == null || !snapshot.isActive()) && cooldown <= 0L) {
            return NIL;
        }

        if (snapshot != null && snapshot.isActive()) {
            return authorityInfo(
                    context.canClientSendNetwork() ? "owned" : "denied",
                    context.canClientSendNetwork() ? null : (snapshot.mode() == ComputerAuthorityMode.FORCED ? "forced_owner" : "leased_by_other")
            );
        }

        LuaTable info = info("state", "requested");
        info.set("retryInTicks", luaLong(cooldown));
        return info;
    }

    private LuaTable authorityInfo(String state, @Nullable String reason) {
        LuaTable table = new LuaTable();
        table.set("state", LuaValue.valueOf(state));

        ComputerAuthoritySnapshot snapshot = context.getClientNetworkAuthoritySnapshot();
        if (snapshot != null && snapshot.isActive()) {
            table.set("mode", LuaValue.valueOf(snapshot.mode().serializedName()));
            table.set("ownerUuid", LuaValue.valueOf(snapshot.ownerId().toString()));
            table.set("leaseExpireTick", luaLong(snapshot.leaseExpireTick()));
            table.set("token", luaLong(snapshot.token()));
            table.set("isSelf", LuaValue.valueOf(context.canClientSendNetwork()));
        }

        long cooldown = context.getClientNetworkAuthorityRequestCooldownTicks();
        if (cooldown > 0L) {
            table.set("retryInTicks", luaLong(cooldown));
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

    private Map<String, Object> luaObjectTable(LuaTable table) {
        Map<String, Object> values = new LinkedHashMap<>();
        LuaValue key = NIL;
        while (true) {
            Varargs next = table.next(key);
            key = next.arg1();
            if (key.isnil()) {
                break;
            }
            values.put(luaKeyToString(key), LuaUtils.toJavaValue(table.get(key)));
        }
        return values;
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
