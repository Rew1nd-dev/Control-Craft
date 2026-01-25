package com.verr1.controlcraft.content.commands;


import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.arguments.ArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.builder.RequiredArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.foundation.camera.CameraClientChunkCacheExtension;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.Entity;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import org.joml.Vector3d;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toJOML;

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
                        .then(
                                lt("debug-set-dubins-start").executes(
                                        ControlCraftClientCommands::setDubinsStartCommand
                                )
                        )
                        .then(
                                lt("debug-set-dubins-end").executes(
                                        ControlCraftClientCommands::setDubinsEndCommand
                                )
                        )
                        .then(
                                lt("debug-set-dubins-radius").then(
                                        arg("radius", IntegerArgumentType.integer()).executes(
                                                ControlCraftClientCommands::setDubinsRadiusCommand
                                        )
                                )
                        )
        );
    }

    private static int setDubinsRadiusCommand(CommandContext<CommandSourceStack> context){
        CommandSourceStack source = context.getSource();
        int radius = context.getArgument("radius", Integer.class);
        Entity player = source.getEntity();
        if(player == null){
            source.sendFailure(Component.literal("You must be a player to set neglect a block!"));
            return 0;
        }
        // DebugTester.setRadius(radius);
        return 1;
    }

    private static int setDubinsStartCommand(CommandContext<CommandSourceStack> context){
        CommandSourceStack source = context.getSource();

        Entity player = source.getEntity();
        if(player == null){
            source.sendFailure(Component.literal("You must be a player to set neglect a block!"));
            return 0;
        }
        Vector3d start = toJOML(player.getEyePosition());
        Vector3d startHeading = toJOML(player.getViewVector(1));
        // DebugTester.setStart(start, startHeading);
        return 1;
    }

    private static int setDubinsEndCommand(CommandContext<CommandSourceStack> context){
        CommandSourceStack source = context.getSource();

        Entity player = source.getEntity();
        if(player == null){
            source.sendFailure(Component.literal("You must be a player to set neglect a block!"));
            return 0;
        }
        Vector3d start = toJOML(player.getEyePosition());
        Vector3d startHeading = toJOML(player.getViewVector(1));
        // DebugTester.setEnd(start, startHeading);
        return 1;
    }

    @SubscribeEvent
    public static void onRegisterCommands(RegisterCommandsEvent event) {
        registerClientCommands(event.getDispatcher());
    }

}
