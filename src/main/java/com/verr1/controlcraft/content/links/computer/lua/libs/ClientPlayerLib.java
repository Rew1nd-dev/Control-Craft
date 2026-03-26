package com.verr1.controlcraft.content.links.computer.lua.libs;

import com.verr1.controlcraft.content.links.computer.ComputerDisplayMetrics;
import com.verr1.controlcraft.content.links.computer.ComputerScreen;
import com.verr1.controlcraft.content.links.computer.lua.IComputerClientContext;
import net.minecraft.client.Minecraft;
import net.minecraft.client.player.LocalPlayer;
import org.jetbrains.annotations.Nullable;
import org.joml.Matrix4d;
import org.joml.Matrix4dc;
import org.joml.Quaterniond;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.luaj.vm2.LuaTable;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.Varargs;
import org.luaj.vm2.lib.TwoArgFunction;
import org.luaj.vm2.lib.VarArgFunction;
import org.luaj.vm2.lib.jse.CoerceJavaToLua;
import org.valkyrienskies.mod.common.VSGameUtilsKt;
import org.valkyrienskies.mod.common.entity.ShipMountedToData;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toJOML;

public class ClientPlayerLib extends TwoArgFunction {

    private static final double EPSILON = 1.0e-6;
    private static final Matrix4dc IDENTITY = new Matrix4d();

    private final IComputerClientContext context;

    public ClientPlayerLib(IComputerClientContext context) {
        this.context = context;
    }

    @Override
    public LuaValue call(LuaValue modname, LuaValue env) {
        LuaValue library = tableOf();

        library.set("yaw", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                LocalPlayer player = Minecraft.getInstance().player;
                if (player == null) {
                    return LuaValue.valueOf(0.0);
                }
                float partial = args.narg() >= 1 ? (float) args.checkdouble(1) : 0.0f;
                return LuaValue.valueOf(player.getViewYRot(partial));
            }
        });

        library.set("pitch", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                LocalPlayer player = Minecraft.getInstance().player;
                if (player == null) {
                    return LuaValue.valueOf(0.0);
                }
                float partial = args.narg() >= 1 ? (float) args.checkdouble(1) : 0.0f;
                return LuaValue.valueOf(player.getViewXRot(partial));
            }
        });

        library.set("getShipMountedToData", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                LocalPlayer player = Minecraft.getInstance().player;
                if (player == null) {
                    return NIL;
                }

                Float partialTicks = args.narg() >= 1 ? (float) args.checkdouble(1) : null;
                ShipMountedToData data = VSGameUtilsKt.getShipMountedToData(player, partialTicks);
                if (data == null) {
                    return NIL;
                }

                LuaTable table = new LuaTable();
                table.set("shipMountedTo", CoerceJavaToLua.coerce(data.getShipMountedTo()));
                table.set("shipMountedToId", LuaValue.valueOf(data.getShipMountedTo().getId()));
                table.set("mountPosInShip", toLuaVec(data.getMountPosInShip(), env));
                return table;
            }
        });

        library.set("getPlayerWatch", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                LocalPlayer player = Minecraft.getInstance().player;
                if (player == null) {
                    return NIL;
                }

                float partialTicks = args.narg() >= 1 ? (float) args.checkdouble(1) : 0.0f;
                ScreenProjectionPlane plane = getScreenProjectionPlane();
                ScreenProjectionResult result = projectRayToScreen(
                        getPlayerEyePosition(player, partialTicks),
                        getPlayerViewDirectionWorld(player, partialTicks),
                        plane
                );
                return toLuaProjection(result, env);
            }
        });

        library.set("projectWorldPoint", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                LocalPlayer player = Minecraft.getInstance().player;
                if (player == null) {
                    return NIL;
                }

                ProjectWorldPointArgs parsed = parseProjectWorldPointArgs(args);
                if (parsed == null) {
                    return NIL;
                }

                Vector3d eyePosition = getPlayerEyePosition(player, parsed.partialTicks());
                ScreenProjectionResult result = projectRayToScreen(
                        eyePosition,
                        new Vector3d(parsed.worldPosition()).sub(eyePosition),
                        getScreenProjectionPlane()
                );
                return toLuaProjection(result, env);
            }
        });

        env.set("Player", library);
        return library;
    }

    private @Nullable ScreenProjectionPlane getScreenProjectionPlane() {
        ComputerScreen screen = context.getScreen();
        if (screen == null) {
            return null;
        }

        Quaterniond computerRotation = new Quaterniond(context.getPhysAccess().quaternionToWorld());
        Vector3d normalLocal = safeNormalize(new Vector3d(context.frontLocal()).negate());
        if (normalLocal == null) {
            return null;
        }

        Vector3d screenRightLocal = new Vector3d(0.0, 1.0, 0.0).cross(normalLocal, new Vector3d());
        if (screenRightLocal.lengthSquared() < EPSILON) {
            screenRightLocal = new Vector3d(context.leftLocal()).negate();
        }
        screenRightLocal = safeNormalize(screenRightLocal);
        if (screenRightLocal == null) {
            return null;
        }

        Vector3d screenUpLocal = new Vector3d(normalLocal).cross(screenRightLocal);
        if (screenUpLocal.lengthSquared() < EPSILON) {
            screenUpLocal = new Vector3d(context.upLocal());
        }
        screenUpLocal = safeNormalize(screenUpLocal);
        if (screenUpLocal == null) {
            return null;
        }

        Vector3d normalWorld = safeNormalize(computerRotation.transform(normalLocal, new Vector3d()));
        Vector3d screenRightWorld = safeNormalize(computerRotation.transform(screenRightLocal, new Vector3d()));
        Vector3d screenUpWorld = safeNormalize(computerRotation.transform(screenUpLocal, new Vector3d()));
        if (normalWorld == null || screenRightWorld == null || screenUpWorld == null) {
            return null;
        }

        Vector3d screenCenterWorld = new Vector3d(context.getPhysAccess().position())
                .add(computerRotation.transform(
                        new Vector3d(0.0, 1.0, 0.0)
                                .fma(0.001, normalLocal)
                                .fma(screen.getOffsetX(), screenRightLocal)
                                .fma(screen.getOffsetY(), screenUpLocal)
                                .fma(screen.getOffsetZ(), normalLocal),
                        new Vector3d()
                ));

        return new ScreenProjectionPlane(
                screen.getDisplayMetrics(),
                screenCenterWorld,
                normalWorld,
                screenRightWorld,
                screenUpWorld
        );
    }

    private static Matrix4dc getPlayerShipToWorld(LocalPlayer player, float partialTicks) {
        ShipMountedToData data = VSGameUtilsKt.getShipMountedToData(player, partialTicks);
        if (data == null || data.getShipMountedTo() == null) {
            return IDENTITY;
        }
        return data.getShipMountedTo().getShipToWorld();
    }

    private static Vector3d viewDirectionInShip(float yawDeg, float pitchDeg) {
        double yaw = Math.toRadians(yawDeg);
        double pitch = Math.toRadians(pitchDeg);
        double cosPitch = Math.cos(pitch);
        return new Vector3d(
                Math.sin(-yaw) * cosPitch,
                Math.sin(-pitch),
                Math.cos(yaw) * cosPitch
        ).normalize();
    }

    private static Vector3d getPlayerEyePosition(LocalPlayer player, float partialTicks) {
        return toJOML(player.getEyePosition(partialTicks));
    }

    private static Vector3d getPlayerViewDirectionWorld(LocalPlayer player, float partialTicks) {
        Vector3d rayDirectionLocal = viewDirectionInShip(player.getViewYRot(partialTicks), player.getViewXRot(partialTicks));
        return safeNormalize(getPlayerShipToWorld(player, partialTicks).transformDirection(rayDirectionLocal, new Vector3d()));
    }

    private static @Nullable ScreenProjectionResult projectRayToScreen(
            Vector3dc rayOrigin,
            @Nullable Vector3dc rayDirection,
            @Nullable ScreenProjectionPlane plane
    ) {
        if (plane == null || rayDirection == null || rayDirection.lengthSquared() < EPSILON) {
            return null;
        }

        Vector3d normalizedDirection = new Vector3d(rayDirection).normalize();
        double denominator = normalizedDirection.dot(plane.normalWorld());
        if (Math.abs(denominator) < EPSILON) {
            return null;
        }

        double distance = new Vector3d(plane.screenCenterWorld()).sub(rayOrigin).dot(plane.normalWorld()) / denominator;
        if (distance <= 0.0) {
            return null;
        }

        Vector3d hitPosWorld = new Vector3d(rayOrigin).fma(distance, normalizedDirection);
        Vector3d relativeHit = new Vector3d(hitPosWorld).sub(plane.screenCenterWorld());

        double localX = relativeHit.dot(plane.screenRightWorld());
        double localY = relativeHit.dot(plane.screenUpWorld());
        double pixelX = (localX / plane.metrics().surfaceWidth() + 0.5) * plane.metrics().pixelWidth();
        double pixelY = (0.5 - localY / plane.metrics().surfaceHeight()) * plane.metrics().pixelHeight();

        boolean onScreen = pixelX >= 0.0
                && pixelX <= plane.metrics().pixelWidth()
                && pixelY >= 0.0
                && pixelY <= plane.metrics().pixelHeight();

        return new ScreenProjectionResult(
                pixelX,
                pixelY,
                distance,
                onScreen,
                hitPosWorld,
                new Vector3d(plane.screenCenterWorld())
        );
    }

    private static LuaValue toLuaProjection(@Nullable ScreenProjectionResult projection, LuaValue env) {
        if (projection == null) {
            return LuaValue.NIL;
        }

        LuaTable table = new LuaTable();
        table.set("x", LuaValue.valueOf(projection.x()));
        table.set("y", LuaValue.valueOf(projection.y()));
        table.set("distance", LuaValue.valueOf(projection.distance()));
        table.set("onScreen", LuaValue.valueOf(projection.onScreen()));
        table.set("hitPosInWorld", toLuaVec(projection.hitPosWorld(), env));
        table.set("screenCenterInWorld", toLuaVec(projection.screenCenterWorld(), env));

        // Compatibility aliases for older scripts.
        table.set("hitPosInShip", toLuaVec(projection.hitPosWorld(), env));
        table.set("screenCenterInShip", toLuaVec(projection.screenCenterWorld(), env));
        return table;
    }

    private static @Nullable ProjectWorldPointArgs parseProjectWorldPointArgs(Varargs args) {
        if (args.narg() >= 3 && args.arg(1).isnumber() && args.arg(2).isnumber() && args.arg(3).isnumber()) {
            float partialTicks = args.narg() >= 4 ? (float) args.checkdouble(4) : 0.0f;
            return new ProjectWorldPointArgs(
                    new Vector3d(args.checkdouble(1), args.checkdouble(2), args.checkdouble(3)),
                    partialTicks
            );
        }

        if (args.narg() < 1) {
            return null;
        }

        Vector3d worldPosition = toVector(args.arg(1));
        if (worldPosition == null) {
            return null;
        }

        float partialTicks = args.narg() >= 2 ? (float) args.checkdouble(2) : 0.0f;
        return new ProjectWorldPointArgs(worldPosition, partialTicks);
    }

    private static @Nullable Vector3d toVector(LuaValue value) {
        Object userdata = value.touserdata();
        if (userdata instanceof Vector3dc vector) {
            return new Vector3d(vector);
        }

        if (!value.istable()) {
            return null;
        }

        LuaValue x = value.get("x");
        LuaValue y = value.get("y");
        LuaValue z = value.get("z");
        if (!x.isnumber() || !y.isnumber() || !z.isnumber()) {
            return null;
        }

        return new Vector3d(x.checkdouble(), y.checkdouble(), z.checkdouble());
    }

    private static @Nullable Vector3d safeNormalize(Vector3d vector) {
        if (vector.lengthSquared() < EPSILON) {
            return null;
        }
        return vector.normalize();
    }

    private static LuaValue toLuaVec(Vector3dc v, LuaValue env) {
        if (v == null) {
            return LuaValue.NIL;
        }

        LuaValue vec = env.get("Vector3d");
        if (!vec.isnil()) {
            LuaValue ctorMethod = vec.get("new");
            if (ctorMethod.isfunction()) {
                return ctorMethod.invoke(LuaValue.varargsOf(new LuaValue[]{
                        vec,
                        LuaValue.valueOf(v.x()),
                        LuaValue.valueOf(v.y()),
                        LuaValue.valueOf(v.z())
                })).arg1();
            }
            if (vec.isfunction()) {
                return vec.call(
                        LuaValue.valueOf(v.x()),
                        LuaValue.valueOf(v.y()),
                        LuaValue.valueOf(v.z()));
            }
        }

        LuaTable t = new LuaTable();
        t.set("x", LuaValue.valueOf(v.x()));
        t.set("y", LuaValue.valueOf(v.y()));
        t.set("z", LuaValue.valueOf(v.z()));
        return t;
    }

    private record ScreenProjectionPlane(
            ComputerDisplayMetrics metrics,
            Vector3d screenCenterWorld,
            Vector3d normalWorld,
            Vector3d screenRightWorld,
            Vector3d screenUpWorld
    ) {
    }

    private record ScreenProjectionResult(
            double x,
            double y,
            double distance,
            boolean onScreen,
            Vector3d hitPosWorld,
            Vector3d screenCenterWorld
    ) {
    }

    private record ProjectWorldPointArgs(Vector3d worldPosition, float partialTicks) {
    }
}
