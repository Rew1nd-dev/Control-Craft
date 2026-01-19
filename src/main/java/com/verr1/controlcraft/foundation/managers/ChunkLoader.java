package com.verr1.controlcraft.foundation.managers;

import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.TicketType;
import net.minecraft.world.level.ChunkPos;
import net.minecraft.world.level.Level;
import net.shao.shaolib.mixinduck.IMinecraftServerDuck;

import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class ChunkLoader {

    public static final TicketType<Long> CHUNK_LOAD_TICKET = TicketType.create("cai:chunk_load", Long::compareTo);

    private static final Map<ResourceKey<Level>, ChunkLoader> managers = new ConcurrentHashMap<>();
    private static final long EXPIRATION_TIME_MS = 10_000; // 5秒
    private final Map<ChunkPos, ChunkEntry> forcedChunks = new ConcurrentHashMap<>();
    public final ServerLevel serverLevel;

    public static ChunkLoader getManagerForLevel(ServerLevel level) {
        return managers.computeIfAbsent(
                level.dimension(),
                k -> new ChunkLoader(level)
        );
    }

    private static class ChunkEntry {
        final long holderId; // 票证持有者ID
        long expirationTime;
        int range;

        ChunkEntry(long holderId, long expirationTime, int range) {
            this.holderId = holderId;
            this.expirationTime = expirationTime;
            this.range = range;
        }
    }

    public ChunkLoader(ServerLevel level) {
        this.serverLevel = level;
    }

    public void addOrUpdateChunkPos(int chunkX, int chunkZ, int range) {
        ChunkPos chunkPos = new ChunkPos(chunkX, chunkZ);

        // 如果区块已经加载，直接更新过期时间
        if (forcedChunks.containsKey(chunkPos)) {
            forcedChunks.computeIfPresent(chunkPos, (key, entry) -> {
                entry.expirationTime = System.currentTimeMillis() + EXPIRATION_TIME_MS;
                return entry;
            });
            return;
        }

        if (!serverLevel.getChunkSource().hasChunk(chunkPos.x, chunkPos.z)){
            // 检查区块是否已生成
            if (!isChunkGenerated(serverLevel, chunkPos)) {
                return;
            }
            //if (!ServerTerrainCache3d.hasChunk(serverLevel, chunkPos)) return;
        }

        // 创建唯一的票证持有者ID
        long holderId = chunkPos.toLong();
        long newExpiration = System.currentTimeMillis() + EXPIRATION_TIME_MS;

        //serverLevel.setChunkForced(pos.x, pos.z, true);
        // 添加区块加载票证
        serverLevel.getChunkSource().addRegionTicket(
                CHUNK_LOAD_TICKET,
                chunkPos,
                range, // 加载范围（区块半径）
                holderId,
                false // 是否启用ticking
        );

        // 添加到管理列表
        forcedChunks.put(chunkPos, new ChunkEntry(holderId, newExpiration, range));
    }

    /**
     * 检查区块是否已生成
     */
    public static boolean isChunkGenerated(ServerLevel level, ChunkPos chunkPos) {
        // 如果区块已加载
        if (level.hasChunk(chunkPos.x, chunkPos.z)) {
            return true;
        }

        Path worldPath = ((IMinecraftServerDuck)level.getServer()).getStorageSource().getDimensionPath(level.dimension());
        Path regionDir = worldPath.resolve("region");

        int regionX = chunkPos.x >> 5; // 32 chunks per region
        int regionZ = chunkPos.z >> 5;
        String regionFile = String.format("r.%d.%d.mca", regionX, regionZ);

        // 检查区域文件是否存在
        Path regionPath = regionDir.resolve(regionFile);
        if (!Files.exists(regionPath)) {
            return false;
        }

        try (RandomAccessFile raf = new RandomAccessFile(regionPath.toFile(), "r")) {
            // 计算区块在区域文件中的偏移
            int chunkOffset = (chunkPos.x & 31) + (chunkPos.z & 31) * 32;
            long locationOffset = chunkOffset * 4L;

            // 读取区块位置信息
            raf.seek(locationOffset);
            int location = raf.readInt();

            // 检查是否已生成 (位置不为0)
            return (location >> 8) != 0;
        } catch (IOException e) {
            return false;
        }
    }

    public void checkExpiredChunks() {
        long currentTime = System.currentTimeMillis();

        forcedChunks.entrySet().removeIf(entry -> {
            ChunkPos pos = entry.getKey();
            ChunkEntry chunkEntry = entry.getValue();

            if (currentTime >= chunkEntry.expirationTime) {
                serverLevel.getChunkSource().removeRegionTicket(
                        CHUNK_LOAD_TICKET,
                        pos,
                        chunkEntry.range,
                        chunkEntry.holderId,
                        false
                );
                return true;
            }
            return false;
        });
    }

    public int getLoadedChunkCount() {
        return forcedChunks.size();
    }

    public void unloadAllChunks() {
        forcedChunks.forEach((pos, entry) -> {
            serverLevel.getChunkSource().removeRegionTicket(
                    CHUNK_LOAD_TICKET,
                    pos,
                    entry.range,
                    entry.holderId,
                    false
            );
        });
        forcedChunks.clear();
    }

    public void onWorldUnload() {
        unloadAllChunks();
        managers.remove(serverLevel.dimension());
    }

    public static void tick(){

    }


    public static ChunkPos[] calculateChunkPath(double startX, double startZ, double endX, double endZ) {
        // 转换为区块坐标
        int startChunkX = (int) Math.floor(startX / 16.0);
        int startChunkZ = (int) Math.floor(startZ / 16.0);
        int endChunkX = (int) Math.floor(endX / 16.0);
        int endChunkZ = (int) Math.floor(endZ / 16.0);

        List<ChunkPos> chunks = new ArrayList<>();

        // 使用 Bresenham 算法遍历直线经过的区块
        int dx = Math.abs(endChunkX - startChunkX);
        int dz = Math.abs(endChunkZ - startChunkZ);
        int sx = startChunkX < endChunkX ? 1 : -1;
        int sz = startChunkZ < endChunkZ ? 1 : -1;
        int err = dx - dz;

        int currentX = startChunkX;
        int currentZ = startChunkZ;
        chunks.add(new ChunkPos(currentX, currentZ));

        while (currentX != endChunkX || currentZ != endChunkZ) {
            int e2 = 2 * err;
            if (e2 > -dz) {
                err -= dz;
                currentX += sx;
            }
            if (e2 < dx) {
                err += dx;
                currentZ += sz;
            }
            chunks.add(new ChunkPos(currentX, currentZ));
        }

        return chunks.toArray(new ChunkPos[0]);
    }

}
