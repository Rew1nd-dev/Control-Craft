package com.verr1.controlcraft.foundation.cimulink.core.components.lua;

import com.verr1.controlcraft.ControlCraft;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.packs.resources.Resource;
import net.minecraft.server.packs.resources.ResourceManager;
import net.minecraft.server.packs.resources.SimplePreparableReloadListener;
import net.minecraft.util.profiling.ProfilerFiller;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Predicate;

import static com.verr1.controlcraft.ControlCraft.MODID;

public class LuaScriptLoader extends SimplePreparableReloadListener<Map<String, String>> {

    private static final String LUA_PATH = "lua";  // 路径前缀，对应 data/modid/data/lua/ 的 "data/lua"
    private static final Predicate<ResourceLocation> LUA_FILTER = rl -> rl.getPath().endsWith(".lua");

    // 存储 Map 的静态变量（或注入到你的控制器类中）
    public static final Map<String, String> LUA_SCRIPTS = new HashMap<>();

    @Override
    protected @NotNull Map<String, String> prepare(ResourceManager resourceManager, @NotNull ProfilerFiller profiler) {
        Map<String, String> scripts = new HashMap<>();

        Map<ResourceLocation, Resource> resources = resourceManager.listResources(LUA_PATH, LUA_FILTER);

        for (Map.Entry<ResourceLocation, Resource> entry : resources.entrySet()) {
            ResourceLocation loc = entry.getKey();
            if (!loc.getNamespace().equals(MODID)) continue;  // 只加载自己的 modid

            Resource res = entry.getValue();
            try (InputStream is = res.open()) {
                String content = new String(is.readAllBytes(), StandardCharsets.UTF_8);
                // 获取文件名，如 "script.lua"（从路径末尾截取）
                String fileName = loc.getPath().substring(loc.getPath().lastIndexOf('/') + 1);
                scripts.put(fileName, content);
            } catch (IOException e) {
                // 日志错误，例如使用 mod logger
                System.err.println("Failed to load Lua script: " + loc + " - " + e.getMessage());
            }
        }
        return scripts;
    }

    @Override
    protected void apply(@NotNull Map<String, String> scripts, @NotNull ResourceManager resourceManager, @NotNull ProfilerFiller profiler) {
        LUA_SCRIPTS.clear();
        LUA_SCRIPTS.putAll(scripts);
        // 可选：日志或通知加载完成
        ControlCraft.LOGGER.info("Loaded {} Lua scripts.", LUA_SCRIPTS.size());
    }
}