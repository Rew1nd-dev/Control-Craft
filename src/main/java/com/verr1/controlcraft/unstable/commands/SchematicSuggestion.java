package com.verr1.controlcraft.unstable.commands;

import com.mojang.brigadier.suggestion.SuggestionProvider;
import com.verr1.controlcraft.unstable.AIServer;
import net.minecraft.commands.CommandSource;
import net.minecraft.commands.CommandSourceStack;

import java.util.Arrays;
import java.util.List;

public class SchematicSuggestion {

    private static List<String> getNamespaces() {
        // 示例数据，实际应从你的逻辑中获取
        return AIServer.SCHEMATICS_MANAGER.getAvailableNamespaces();
    }

    // 假设的函数，根据 namespace 返回 name 列表
    private static List<String> getNames(String namespace) {
        return AIServer.SCHEMATICS_MANAGER.getAvailableNames(namespace);
    }

    // namespace 的自动补全
    public static final SuggestionProvider<CommandSourceStack> NAMESPACE_SUGGESTIONS = (context, builder) -> {
        // 获取 namespace 列表并提供建议
        List<String> namespaces = getNamespaces();
        for (String namespace : namespaces) {
            builder.suggest(namespace);
        }
        return builder.buildFuture();
    };

    // name 的自动补全
    public static final SuggestionProvider<CommandSourceStack> NAME_SUGGESTIONS = (context, builder) -> {
        // 从命令上下文中获取输入的 namespace
        String namespace = context.getArgument("namespace", String.class);
        // 根据 namespace 获取 name 列表并提供建议
        List<String> names = getNames(namespace);
        for (String name : names) {
            builder.suggest(name);
        }
        return builder.buildFuture();
    };

}
