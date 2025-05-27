package com.verr1.controlcraft.content.commands;


import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.arguments.ArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.builder.RequiredArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.foundation.camera.CameraBoundFakePlayer;
import com.verr1.controlcraft.foundation.camera.CameraClientChunkCacheExtension;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;

@Mod.EventBusSubscriber(modid = ControlCraft.MODID, value = Dist.CLIENT, bus = Mod.EventBusSubscriber.Bus.FORGE)
public class ControlCraftClientCommands {
    private static LiteralArgumentBuilder<CommandSourceStack> lt(String name){
        return LiteralArgumentBuilder.literal(name);
    }

    private static<T> RequiredArgumentBuilder<CommandSourceStack, T> arg(String name, ArgumentType<T> type){
        return RequiredArgumentBuilder.argument(name, type);
    }

    private static int countCacheCommand(CommandContext<CommandSourceStack> context){
        context.getSource().sendSuccess(() -> Component.literal("" + CameraClientChunkCacheExtension.size()), false);
        return 1;
    }

    public static void registerClientCommands(CommandDispatcher<CommandSourceStack> dispatcher){
        dispatcher.register(
                lt("controlcraft")
                        .then(
                                lt("debug-chunk-cache-size").executes(
                                        ControlCraftClientCommands::countCacheCommand
                                )
                        )
        );
    }

    @SubscribeEvent
    public static void onRegisterCommands(RegisterCommandsEvent event) {
        registerClientCommands(event.getDispatcher());
    }

}
