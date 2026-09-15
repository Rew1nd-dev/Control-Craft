package com.verr1.controlcraft.content.links.computer.lua.libs;

import com.verr1.controlcraft.content.links.computer.lua.IComputerServerContext;
import com.verr1.controlcraft.foundation.cimulink.game.PlantDiscoveryEntry;
import com.verr1.controlcraft.utils.ConstraintClusterBusResolver;
import net.minecraft.core.BlockPos;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.lib.OneArgFunction;
import org.luaj.vm2.lib.TwoArgFunction;
import org.luaj.vm2.lib.ZeroArgFunction;

import java.util.Collection;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Stream;

public class ServerPlantLib extends TwoArgFunction {

    private final IComputerServerContext context;

    public ServerPlantLib(IComputerServerContext context) {
        this.context = context;
    }

    @Override
    public LuaValue call(LuaValue modname, LuaValue env) {
        LuaValue library = tableOf();

        library.set("list", new ZeroArgFunction() {
            @Override
            public LuaValue call() {
                return entriesToLua(ConstraintClusterBusResolver.allPlants(context.getShipOrGroundID()));
            }
        });

        library.set("find", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue key) {
                return entriesToLua(ConstraintClusterBusResolver.findPlants(context.getShipOrGroundID(), key.checkjstring()));
            }
        });

        env.set("Plants", library);
        return library;
    }

    private LuaValue entriesToLua(Stream<PlantDiscoveryEntry> entries) {
        LuaValue table = tableOf();
        int index = 1;
        for (PlantDiscoveryEntry entry : entries.toList()) {
            table.set(index++, entryToLua(entry));
        }
        return table;
    }

    private LuaValue entryToLua(PlantDiscoveryEntry entry) {
        LuaValue table = tableOf();
        table.set("key", LuaValue.valueOf(entry.typeKey()));
        table.set("type", LuaValue.valueOf(entry.typeKey()));
        table.set("name", LuaValue.valueOf(entry.name()));
        table.set("shipId", LuaValue.valueOf(entry.shipId()));
        table.set("dimension", LuaValue.valueOf(entry.position().dimensionID()));
        table.set("yard", blockPosToLua(entry.position().pos()));
        table.set("pos", blockPosToLua(entry.position().pos()));
        table.set("inputs", listToLua(entry.inputs()));
        table.set("outputs", listToLua(entry.outputs()));
        table.set("metadata", mapToLua(entry.metadata()));
        return table;
    }

    private LuaValue blockPosToLua(BlockPos pos) {
        LuaValue table = tableOf();
        table.set("x", LuaValue.valueOf(pos.getX()));
        table.set("y", LuaValue.valueOf(pos.getY()));
        table.set("z", LuaValue.valueOf(pos.getZ()));
        return table;
    }

    private LuaValue listToLua(Collection<?> values) {
        LuaValue table = tableOf();
        int index = 1;
        for (Object value : values) {
            table.set(index++, toLuaValue(value));
        }
        return table;
    }

    private LuaValue mapToLua(Map<?, ?> values) {
        LuaValue table = tableOf();
        values.forEach((key, value) -> {
            if (key != null) {
                table.set(key.toString(), toLuaValue(value));
            }
        });
        return table;
    }

    private LuaValue toLuaValue(Object value) {
        if (value == null) {
            return NIL;
        }
        if (value instanceof LuaValue luaValue) {
            return luaValue;
        }
        if (value instanceof Boolean bool) {
            return LuaValue.valueOf(bool);
        }
        if (value instanceof Number number) {
            return LuaValue.valueOf(number.doubleValue());
        }
        if (value instanceof CharSequence text) {
            return LuaValue.valueOf(text.toString());
        }
        if (value instanceof Map<?, ?> map) {
            return mapToLua(map);
        }
        if (value instanceof Collection<?> collection) {
            return listToLua(collection);
        }
        if (value.getClass().isEnum()) {
            return LuaValue.valueOf(((Enum<?>) value).name().toLowerCase(Locale.ROOT));
        }
        return LuaValue.valueOf(value.toString());
    }
}
