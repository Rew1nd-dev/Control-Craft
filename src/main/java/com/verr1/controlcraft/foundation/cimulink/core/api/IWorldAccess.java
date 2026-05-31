package com.verr1.controlcraft.foundation.cimulink.core.api;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import com.verr1.controlcraft.utils.AsyncDebugFileLogger;
import net.minecraft.core.BlockPos;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import org.jetbrains.annotations.Nullable;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.sounds.SoundSource;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.AABB;
import net.minecraft.world.phys.Vec3;
import org.joml.Vector3dc;
import org.valkyrienskies.core.api.ships.LoadedServerShip;
import org.valkyrienskies.mod.api.ValkyrienSkies;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.ThreadLocalRandom;

public interface IWorldAccess {

    public static final IWorldAccess EMPTY = new IWorldAccess() {
        @Override
        public void yell(float distance, String msg) {

        }

        @Override
        public void log(String msg) {

        }

        @Override
        public boolean debugLog(String fileName, String msg) {
            return false;
        }

        @Override
        public boolean resetDebugLog(String fileName) {
            return false;
        }

        @Override
        public String defaultDebugLogFile() {
            return "debug.log";
        }

        @Override
        public void beep(float distance, float volume, float pitch) {

        }

    };

    void yell(float distance, String msg);

    void log(String msg);

    boolean debugLog(String fileName, String msg);

    boolean resetDebugLog(String fileName);

    String defaultDebugLogFile();

    void beep(float distance, float volume, float pitch);

    default @Nullable LoadedServerShip loadedServerShip() {
        return null;
    }

    default double[] debug_lastTickFlapControls(){
        return new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
    };

    static IWorldAccess of(OnShipBlockEntity be) {
        String defaultDebugLogFile = defaultDebugLogFileOf(be);
        return new IWorldAccess() {
            @Override
            public void yell(float distance, String msg) {
                Runnable task = () -> {
                    Level level = be.getLevel();
                    Vec3 pos = ValkyrienSkies.toMinecraft(be.position());
                    if (level != null && !level.isClientSide) {
                        List<ServerPlayer> players = level.getEntitiesOfClass(
                            ServerPlayer.class,
                            new AABB(BlockPos.containing(pos)).inflate(distance));
                        for (ServerPlayer player : players) {
                            player.sendSystemMessage(Component.literal(msg));
                        }
                    }
                };
                ControlCraftServer.SERVER_EXECUTOR.executeLater(task, 1);
            }

            @Override
            public void log(String msg) {
                ControlCraft.LOGGER.info(msg);
            }

            @Override
            public boolean debugLog(String fileName, String msg) {
                return AsyncDebugFileLogger.appendLine(fileName, msg);
            }

            @Override
            public boolean resetDebugLog(String fileName) {
                return AsyncDebugFileLogger.resetFile(fileName);
            }

            @Override
            public String defaultDebugLogFile() {
                return defaultDebugLogFile;
            }

            @Override
            public void beep(float distance, float volume, float pitch) {
                Runnable task = () -> {
                    Level level = be.getLevel();
                    Vec3 pos = ValkyrienSkies.toMinecraft(be.position());
                    if (level != null && !level.isClientSide) {
                        List<ServerPlayer> players = level.getEntitiesOfClass(
                            ServerPlayer.class,
                            new AABB(BlockPos.containing(pos)).inflate(distance)
                        );
                        for (ServerPlayer player : players) {
                            player.connection.send(new net.minecraft.network.protocol.game.ClientboundSoundPacket(
                                BuiltInRegistries.SOUND_EVENT.wrapAsHolder(SoundEvents.NOTE_BLOCK_PLING.get()),
                                SoundSource.BLOCKS,
                                pos.x, pos.y, pos.z,
                                volume, pitch,
                                ThreadLocalRandom.current().nextLong())
                            );
                        }
                    }
                };
                ControlCraftServer.SERVER_EXECUTOR.executeLater(task, 1);

            }

            @Override
            public double[] debug_lastTickFlapControls() {
                Vector3dc[] c = be.debug_lastTickFlapControls();
                if(c == null)return IWorldAccess.super.debug_lastTickFlapControls();
                return new double[]{c[0].x(), c[0].y(),c[0].z(), c[1].x(), c[1].y(), c[1].z()};
            }

            @Override
            public @Nullable LoadedServerShip loadedServerShip() {
                return be.getLoadedServerShip();
            }
        };
    }


    static IWorldAccess ofImmediate(OnShipBlockEntity be) {
        String defaultDebugLogFile = defaultDebugLogFileOf(be);
        return new IWorldAccess() {
            @Override
            public void yell(float distance, String msg) {
                Runnable task = () -> {
                    Level level = be.getLevel();
                    Vec3 pos = ValkyrienSkies.toMinecraft(be.position());
                    if (level != null && !level.isClientSide) {
                        List<ServerPlayer> players = level.getEntitiesOfClass(
                            ServerPlayer.class,
                            new AABB(BlockPos.containing(pos)).inflate(distance));
                        for (ServerPlayer player : players) {
                            player.sendSystemMessage(Component.literal(msg));
                        }
                    }
                };
                task.run();
            }

            @Override
            public void log(String msg) {
                ControlCraft.LOGGER.info(msg);
            }

            @Override
            public boolean debugLog(String fileName, String msg) {
                return AsyncDebugFileLogger.appendLine(fileName, msg);
            }

            @Override
            public boolean resetDebugLog(String fileName) {
                return AsyncDebugFileLogger.resetFile(fileName);
            }

            @Override
            public String defaultDebugLogFile() {
                return defaultDebugLogFile;
            }

            @Override
            public void beep(float distance, float volume, float pitch) {
                Runnable task = () -> {
                    Level level = be.getLevel();
                    Vec3 pos = ValkyrienSkies.toMinecraft(be.position());
                    if (level != null && !level.isClientSide) {
                        List<ServerPlayer> players = level.getEntitiesOfClass(
                            ServerPlayer.class,
                            new AABB(BlockPos.containing(pos)).inflate(distance)
                        );
                        for (ServerPlayer player : players) {
                            player.connection.send(new net.minecraft.network.protocol.game.ClientboundSoundPacket(
                                BuiltInRegistries.SOUND_EVENT.wrapAsHolder(SoundEvents.NOTE_BLOCK_PLING.get()),
                                SoundSource.BLOCKS,
                                pos.x, pos.y, pos.z,
                                volume, pitch,
                                ThreadLocalRandom.current().nextLong())
                            );
                        }
                    }
                };
                task.run();
            }

            @Override
            public @Nullable LoadedServerShip loadedServerShip() {
                return be.getLoadedServerShip();
            }
        };
    }

    static String defaultDebugLogFileOf(OnShipBlockEntity be) {
        String dimension = Optional
                .ofNullable(be.getLevel())
                .map(level -> be.getDimensionID())
                .orElse("unknown_dimension");
        BlockPos pos = be.getBlockPos();
        String suggested = String.format(
                "%s_%s_%d_%d_%d.log",
                be.getClass().getSimpleName(),
                dimension,
                pos.getX(),
                pos.getY(),
                pos.getZ()
        );
        return AsyncDebugFileLogger.normalizeFileName(suggested);
    }
}