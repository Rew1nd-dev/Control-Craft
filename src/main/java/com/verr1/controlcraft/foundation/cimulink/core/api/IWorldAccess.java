package com.verr1.controlcraft.foundation.cimulink.core.api;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.content.links.integration.LuaBlockEntity;
import net.minecraft.core.BlockPos;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.sounds.SoundSource;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.AABB;
import net.minecraft.world.phys.Vec3;

import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

public interface IWorldAccess {

    public static final IWorldAccess EMPTY = new IWorldAccess() {
        @Override
        public void yell(float distance, String msg) {

        }

        @Override
        public void log(String msg) {

        }

        @Override
        public void beep(float distance, float volume, float pitch) {

        }
    };

    void yell(float distance, String msg);

    void log(String msg);

    void beep(float distance, float volume, float pitch);

    static IWorldAccess of(LuaBlockEntity be) {
        return new IWorldAccess() {
            @Override
            public void yell(float distance, String msg) {
                Runnable task = () -> {
                    Level level = be.getLevel();
                    Vec3 pos = toMinecraft(be.getBasePosition());
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
            public void beep(float distance, float volume, float pitch) {
                Runnable task = () -> {
                    Level level = be.getLevel();
                    Vec3 pos = toMinecraft(be.getBasePosition());
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
        };
    }
}
