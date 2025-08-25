package com.verr1.controlcraft.unstable.data.schematic;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.DebugUtils;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtIo;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.packs.resources.Resource;
import net.minecraft.server.packs.resources.ResourceManager;
import net.minecraftforge.server.ServerLifecycleHooks;
import org.valkyrienskies.core.api.ships.ServerShip;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.function.Predicate;

public class AIServerSchematics {
    public static final String SCHEMATICS_KEY = "schematicsKey";
    public static final String SCHEMATICS_CONTENT = "schematicsContent";
    public static final String SCHEMATICS_FOLDER = "ai_schematics";


    private final Map<SchematicKey, AISchematic> LOADED_SCHEMATICS = new HashMap<>();


    public List<String> getAvailableNamespaces(){
        return LOADED_SCHEMATICS.keySet().stream().map(k -> k.namespace).toList();
    }

    public List<String> getAvailableNames(String namespace){
        return LOADED_SCHEMATICS.keySet().stream()
                .filter(k -> k.namespace.equals(namespace))
                .map(k -> k.name)
                .toList();
    }

    public AISchematic getLoaded(SchematicKey key){
        return LOADED_SCHEMATICS.get(key);
    }

    public void loadSchematics(MinecraftServer server){
        /*
        Map<ResourceLocation, CompoundTag> nbtFiles = loadAllNBTFiles(ControlCraft.MODID, SCHEMATICS_FOLDER, server);
        nbtFiles.forEach(((resourceLocation, tag) -> {
            if(!tag.contains(SCHEMATICS_KEY)){
                ControlCraft.LOGGER.error("NBT file {} does not contain the required key '{}'. Skipping.", resourceLocation, SCHEMATICS_KEY);
                return;
            }

            CompoundTag schematicKeyTag = tag.getCompound(SCHEMATICS_KEY);
            CompoundTag schematicContentTag = tag.getCompound(SCHEMATICS_CONTENT);
            SchematicKey key = SchematicKey.deserialize(schematicKeyTag);

            if(LOADED_SCHEMATICS.containsKey(key)){
                ControlCraft.LOGGER.warn("Schematic with key {} already loaded. Skipping duplicate.", key);
                return;
            }

            AISchematic schematic = AISchematic.deserialize(schematicContentTag);

            LOADED_SCHEMATICS.put(key, schematic);

        }));
        * */
        List<CompoundTag> nbtFiles = loadAllNBTFilesFromFolder(SCHEMATICS_FOLDER, server);
        nbtFiles.forEach(tag -> {
            if(!tag.contains(SCHEMATICS_KEY)){
                ControlCraft.LOGGER.error("NBT file does not contain the required key. Skipping.");
                return;
            }

            CompoundTag schematicKeyTag = tag.getCompound(SCHEMATICS_KEY);
            CompoundTag schematicContentTag = tag.getCompound(SCHEMATICS_CONTENT);
            SchematicKey key = SchematicKey.deserialize(schematicKeyTag);

            if(LOADED_SCHEMATICS.containsKey(key)){
                ControlCraft.LOGGER.warn("Schematic with key {} already loaded. Skipping duplicate.", key);
                return;
            }

            AISchematic schematic = AISchematic.deserialize(schematicContentTag);

            LOADED_SCHEMATICS.put(key, schematic);

        });
    }

    public void saveSchematics(SchematicKey key, AISchematic schematic){

        CompoundTag total = CompoundTagBuilder.create()
                .withCompound(SCHEMATICS_KEY, key.serialize())
                .withCompound(SCHEMATICS_CONTENT, schematic.serialize())
                .build();

        saveNBTFile(
                SCHEMATICS_FOLDER,
                key.getFullName(),
                total,
                ServerLifecycleHooks.getCurrentServer()
        );

    }

    public void createSchematicsAsync(WorldBlockPos center, ServerShip ship, String namespace, String name){
        CompletableFuture.supplyAsync(() -> {
            try {
                SchematicKey key = new SchematicKey(namespace, name);
                AISchematic schematics = AISchematic.create(ship, center.pos(), center.level(ServerLifecycleHooks.getCurrentServer()));
                saveSchematics(key, schematics);
                return schematics;
            } catch (Exception e) {
                ControlCraft.LOGGER.error("Failed to create schematics for ship at {}", center, e);
                DebugUtils.stackTrace(e);
            }
            return null;
        }).thenAcceptAsync(
            schematic -> {
                SchematicKey key = new SchematicKey(namespace, name);
                if (schematic != null) {
                    LOADED_SCHEMATICS.put(key, schematic);
                    ControlCraft.LOGGER.info("Successfully created and saved schematics for ship at {}", center);
                } else {
                    ControlCraft.LOGGER.error("Failed to create schematics for ship at {}", center);
                }
            },
            ServerLifecycleHooks.getCurrentServer()
        );
    }

    public static void saveNBTFile(String directory, String name, CompoundTag tag, MinecraftServer server){
        try {
            File worldDir = server.getServerDirectory();
            // 构造目标文件夹路径，例如 world/data/controlcraft/ai_schematics/
            String relativePath = "ai_schematics";
            File targetDir = new File(worldDir, relativePath);

            // 确保目标文件夹存在
            if (!targetDir.exists()) {
                targetDir.mkdirs();
            }

            // 使用 SchematicKey 的信息或时间戳生成唯一文件名
            String fileName = name + ".nbt"; // 简单示例，使用时间戳
            File targetFile = new File(targetDir, fileName);

            // 写入 NBT 文件
            try (FileOutputStream outputStream = new FileOutputStream(targetFile)) {
                NbtIo.writeCompressed(tag, outputStream);
                ControlCraft.LOGGER.info("Successfully wrote NBT file: {}", targetFile.getPath());
            } catch (Exception e) {
                ControlCraft.LOGGER.error("Failed to write NBT file: {}", targetFile.getPath());
                DebugUtils.stackTrace(e);
            }
        } catch (Exception e) {
            ControlCraft.LOGGER.error("Failed to prepare NBT file directory: {}", directory);
            DebugUtils.stackTrace(e);
        }
    }

    public static Map<ResourceLocation, CompoundTag> loadAllNBTFiles(String modId, String directory, MinecraftServer server) {
        Map<ResourceLocation, CompoundTag> nbtFiles = new HashMap<>();
        try {
            // 获取当前服务器实例
            if (server == null) {
                throw new IllegalStateException("Cannot load NBT files: No server instance available.");
            }

            // 获取资源管理器
            ResourceManager resourceManager = server.getResourceManager();

            // 列出指定目录下所有 .nbt 文件
            try {
                // 定义路径，例如 "data/examplemod/xxx"
                String path = "data/" + modId + "/" + directory;

                // 定义过滤器，只获取 .nbt 文件
                Predicate<ResourceLocation> nbtFilter = location -> location.getPath().endsWith(".nbt");

                // 获取所有匹配的资源
                Map<ResourceLocation, Resource> resources = resourceManager.listResources(path, nbtFilter);

                // 遍历并加载每个 NBT 文件
                for (Map.Entry<ResourceLocation, Resource> entry : resources.entrySet()) {
                    ResourceLocation location = entry.getKey();
                    Resource resource = entry.getValue();

                    try (InputStream inputStream = resource.open()) {
                        CompoundTag nbtData = NbtIo.readCompressed(inputStream);
                        nbtFiles.put(location, nbtData);
                        System.out.println("Loaded NBT file: " + location);
                    } catch (Exception e) {
                        System.err.println("Failed to load NBT file: " + location);
                        DebugUtils.stackTrace(e);
                    }
                }
            } catch (Exception e) {
                System.err.println("Failed to scan NBT directory: " + directory);
                DebugUtils.stackTrace(e);
            }
        } catch (Exception e) {
            System.err.println("Failed to scan NBT directory: " + directory);
            DebugUtils.stackTrace(e);
        }
        return nbtFiles;
    }

    public static List<CompoundTag> loadAllNBTFilesFromFolder(String directory, MinecraftServer server) {
    try {
        File worldDir = server.getServerDirectory();
        // 构造目标文件夹路径，例如 world/data/controlcraft/ai_schematics/
        String relativePath = "ai_schematics";
        File targetDir = new File(worldDir, relativePath);

        // 确保目标文件夹存在
        if (!targetDir.exists()) {
            targetDir.mkdirs();
        }

        // 获取目录下所有 .nbt 文件
        File[] files = targetDir.listFiles((dir, name) -> name.endsWith(".nbt"));
        if (files == null) {
            ControlCraft.LOGGER.error("No NBT files found in directory: {}", targetDir.getPath());
            return List.of();
        }

        // 读取每个 NBT 文件并转换为 CompoundTag
        List<CompoundTag> nbtTags = new ArrayList<>();
        for (File file : files) {
            try (InputStream inputStream = new FileInputStream(file)) {
                CompoundTag nbtData = NbtIo.readCompressed(inputStream);
                nbtTags.add(nbtData);
                ControlCraft.LOGGER.info("Loaded NBT file: {}", file.getPath());
            } catch (Exception e) {
                ControlCraft.LOGGER.error("Failed to load NBT file: {}", file.getPath());
                DebugUtils.stackTrace(e);
            }
        }

        return nbtTags;
    } catch (Exception e) {
        ControlCraft.LOGGER.error("Failed to prepare NBT file directory: {}", directory);
        DebugUtils.stackTrace(e);
        return List.of();
    }
}

}
