package com.verr1.controlcraft.content.links.computer.lua.libs;

import com.verr1.controlcraft.content.links.computer.ComputerBlockEntity;
import com.verr1.controlcraft.content.valkyrienskies.attachments.CimulinkPorts;
import com.verr1.controlcraft.foundation.cimulink.core.api.IWorldAccess;
import org.jetbrains.annotations.Nullable;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.lib.ThreeArgFunction;
import org.luaj.vm2.lib.TwoArgFunction;
import org.valkyrienskies.core.api.ships.LoadedServerShip;

import java.util.List;

public class LuaToComputerLib extends TwoArgFunction {

    private final IWorldAccess worldAccess;

    public LuaToComputerLib(IWorldAccess worldAccess) {
        this.worldAccess = worldAccess;
    }

    @Override
    public LuaValue call(LuaValue modname, LuaValue env) {
        LuaValue library = tableOf();

        library.set("set", new ThreeArgFunction() {
            @Override
            public LuaValue call(LuaValue computerName, LuaValue key, LuaValue value) {
                String targetComputerName = computerName.checkjstring();
                String targetKey = key.checkjstring();
                double targetValue = value.checkdouble();

                findComputers(targetComputerName).forEach(computer -> computer.setLuaValue(targetKey, targetValue));
                return NIL;
            }
        });

        library.set("isPresent", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue computerName, LuaValue key) {
                String targetComputerName = computerName.checkjstring();
                String targetKey = key.checkjstring();

                return LuaValue.valueOf(
                        findComputers(targetComputerName)
                                .stream()
                                .anyMatch(computer -> computer.hasLuaValue(targetKey))
                );
            }
        });

        library.set("clear", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue computerName, LuaValue key) {
                String targetComputerName = computerName.checkjstring();
                String targetKey = key.checkjstring();

                findComputers(targetComputerName).forEach(computer -> computer.clearLuaValue(targetKey));
                return NIL;
            }
        });

        library.set("get", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue computerName, LuaValue key) {
                String targetComputerName = computerName.checkjstring();
                String targetKey = key.checkjstring();

                for (ComputerBlockEntity computer : findComputers(targetComputerName)) {
                    Double value = computer.getLuaValue(targetKey);
                    if (value != null) {
                        return LuaValue.valueOf(value);
                    }
                }

                return NIL;
            }
        });

        env.set("LuaToComputer", library);
        return library;
    }

    private List<ComputerBlockEntity> findComputers(String deviceName) {
        LoadedServerShip ship = worldAccess.loadedServerShip();
        if (ship == null) {
            return List.of();
        }

        CimulinkPorts ports = CimulinkPorts.get(ship);
        if (ports == null) {
            return List.of();
        }

        return ports.getComputersOf(deviceName);
    }
}
