package com.verr1.controlcraft.content.commands;

import com.mojang.brigadier.Command;
import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.arguments.ArgumentType;
import com.mojang.brigadier.arguments.LongArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.builder.RequiredArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.foundation.camera.CameraBoundFakePlayer;
import com.verr1.controlcraft.foundation.managers.PeripheralNetwork;
import com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies;
import com.verr1.controlcraft.registry.ControlCraftAttachments;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.player.Player;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;

import java.util.Arrays;
import java.util.concurrent.atomic.AtomicInteger;

@Mod.EventBusSubscriber(modid = ControlCraft.MODID, bus = Mod.EventBusSubscriber.Bus.FORGE)
public class ControlCraftCommands {

    private static LiteralArgumentBuilder<CommandSourceStack> lt(String name){
        return LiteralArgumentBuilder.literal(name);
    }

    private static<T> RequiredArgumentBuilder<CommandSourceStack, T> arg(String name, ArgumentType<T> type){
        return RequiredArgumentBuilder.argument(name, type);
    }

    private static void clearAllAttachments(){
        ControlCraftServer
                .INSTANCE
                .getAllLevels()
                .forEach(
                    lvl -> ValkyrienSkies
                            .getShipWorld(lvl)
                            .getAllShips()
                            .forEach(
                                s -> Arrays
                                        .stream(ControlCraftAttachments.values())
                                        .map(ControlCraftAttachments::getClazz)
                                        .filter(c -> s.getAttachment(c) != null)
                                        .forEach(
                                                c -> s.saveAttachment(c, null)
                                        )
                            )
                );
    }

    private static int freeCommand(CommandContext<CommandSourceStack> context){
        String name = context.getArgument("name", String.class);
        long protocol = context.getArgument("protocol", Long.class);

        PeripheralNetwork.free(new PeripheralNetwork.PeripheralKey(
                protocol,
                name
        ));

        return 1;
    }

    private static int countFPCommand(CommandContext<CommandSourceStack> context){
        CommandSourceStack source = context.getSource();
        if(source.getPlayer() == null){
            source.sendFailure(Component.literal("You must be a player to set neglect a block!"));
            return 0;
        }
        ServerPlayer player = source.getPlayer();
        var fps = player.level().players().stream().filter(CameraBoundFakePlayer.class::isInstance).toList();
        source.sendSuccess(() -> Component.literal("There are " + fps.size() + " fake players in the world."), false);
        fps.forEach(
                fp -> {
                    source.sendSuccess(() -> Component.literal(
                            "Position: " + fp.position()
                    ), false);
                }
        );

        return 1;
    }

    public static void registerServerCommands(CommandDispatcher<CommandSourceStack> dispatcher){
        dispatcher.register(
            Commands
                .literal("controlcraft")
                .then(
                    LiteralArgumentBuilder.<CommandSourceStack>literal("clear-attachment")
                        .executes(
                                context -> {
                                    clearAllAttachments();
                                    return 1;
                        })
                ).then(
                    lt("free-key").then(
                            arg("protocol", LongArgumentType.longArg()).then(
                                    arg("name", StringArgumentType.string()).executes(
                                        ControlCraftCommands::freeCommand
                                    )
                            )
                    )
                ).then(
                        lt("debug-count-fake-player").executes(
                                ControlCraftCommands::countFPCommand
                        )
                    )
        );
    }

    @SubscribeEvent
    public static void onRegisterCommands(RegisterCommandsEvent event) {
        registerServerCommands(event.getDispatcher());
    }

}
