package com.verr1.controlcraft.content.links.computer.lua.libs;

import net.minecraft.client.Minecraft;
import org.jetbrains.annotations.Nullable;
import org.joml.Quaterniondc;
import org.joml.Vector3dc;
import org.luaj.vm2.LuaTable;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.Varargs;
import org.luaj.vm2.lib.TwoArgFunction;
import org.luaj.vm2.lib.VarArgFunction;
import org.luaj.vm2.lib.ZeroArgFunction;
import org.valkyrienskies.core.api.ships.ClientShip;
import org.valkyrienskies.core.api.world.ClientShipWorld;
import org.valkyrienskies.mod.api.ValkyrienSkies;

import java.util.Optional;

public class ClientShipLib extends TwoArgFunction {

    @Override
    public LuaValue call(LuaValue modname, LuaValue env) {
        LuaValue library = tableOf();

        library.set("getAllShips", new ZeroArgFunction() {
            @Override
            public LuaValue call() {
                LuaTable ships = new LuaTable();
                ClientShipWorld shipWorld = getShipWorld();
                if (shipWorld == null) {
                    return ships;
                }

                int index = 1;
                for (ClientShip ship : shipWorld.getLoadedShips()) {
                    ships.set(index++, LuaValue.valueOf(ship.getId()));
                }
                return ships;
            }
        });

        library.set("getPositionOf", vectorGetter(env, ship -> ship.getTransform().getPositionInWorld()));
        library.set("getVelocityOf", vectorGetter(env, ClientShip::getVelocity));
        library.set("getQuaternionOf", quaternionGetter(env, ship -> ship.getTransform().getShipToWorldRotation()));
        library.set("getAngularVelocityOf", vectorGetter(env, ClientShip::getOmega));

        env.set("Ship", library);
        return library;
    }

    private VarArgFunction vectorGetter(LuaValue env, VectorGetter getter) {
        return new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                return toLuaVector(env, findShip(args.checklong(1)).map(getter::get).orElse(null));
            }
        };
    }

    private VarArgFunction quaternionGetter(LuaValue env, QuaternionGetter getter) {
        return new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                return toLuaQuaternion(env, findShip(args.checklong(1)).map(getter::get).orElse(null));
            }
        };
    }

    private static Optional<ClientShip> findShip(long id) {
        ClientShipWorld shipWorld = getShipWorld();
        if (shipWorld == null) {
            return Optional.empty();
        }

        return shipWorld.getLoadedShips()
                .stream()
                .filter(ship -> ship.getId() == id)
                .findFirst();
    }

    private static @Nullable ClientShipWorld getShipWorld() {
        Minecraft client = Minecraft.getInstance();
        if (client.level == null) {
            return null;
        }
        return ValkyrienSkies.getShipWorld(client);
    }

    private static LuaValue toLuaVector(LuaValue env, @Nullable Vector3dc vector) {
        if (vector == null) {
            return LuaValue.NIL;
        }

        LuaValue vectorType = env.get("Vector3d");
        LuaValue ctor = vectorType.get("new");
        if (vectorType.istable() && ctor.isfunction()) {
            return ctor.invoke(LuaValue.varargsOf(new LuaValue[]{
                    vectorType,
                    LuaValue.valueOf(vector.x()),
                    LuaValue.valueOf(vector.y()),
                    LuaValue.valueOf(vector.z())
            })).arg1();
        }

        LuaTable fallback = new LuaTable();
        fallback.set("x", LuaValue.valueOf(vector.x()));
        fallback.set("y", LuaValue.valueOf(vector.y()));
        fallback.set("z", LuaValue.valueOf(vector.z()));
        return fallback;
    }

    private static LuaValue toLuaQuaternion(LuaValue env, @Nullable Quaterniondc quaternion) {
        if (quaternion == null) {
            return LuaValue.NIL;
        }

        LuaValue quaternionType = env.get("Quaterniond");
        LuaValue ctor = quaternionType.get("new");
        if (quaternionType.istable() && ctor.isfunction()) {
            return ctor.invoke(LuaValue.varargsOf(new LuaValue[]{
                    quaternionType,
                    LuaValue.valueOf(quaternion.x()),
                    LuaValue.valueOf(quaternion.y()),
                    LuaValue.valueOf(quaternion.z()),
                    LuaValue.valueOf(quaternion.w())
            })).arg1();
        }

        LuaTable fallback = new LuaTable();
        fallback.set("x", LuaValue.valueOf(quaternion.x()));
        fallback.set("y", LuaValue.valueOf(quaternion.y()));
        fallback.set("z", LuaValue.valueOf(quaternion.z()));
        fallback.set("w", LuaValue.valueOf(quaternion.w()));
        return fallback;
    }

    @FunctionalInterface
    private interface VectorGetter {
        Vector3dc get(ClientShip ship);
    }

    @FunctionalInterface
    private interface QuaternionGetter {
        Quaterniondc get(ClientShip ship);
    }
}