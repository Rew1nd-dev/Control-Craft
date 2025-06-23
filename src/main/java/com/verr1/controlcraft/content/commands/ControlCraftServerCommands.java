package com.verr1.controlcraft.content.commands;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.arguments.*;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.builder.RequiredArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.content.items.CircuitCompilerItem;
import com.verr1.controlcraft.foundation.camera.CameraBoundFakePlayer;
import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.Circuit;
import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.CircuitDebugger;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.packaged.CircuitLinkPort;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.managers.ConstraintCenter;
import com.verr1.controlcraft.foundation.managers.PeripheralNetwork;
import com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies;
import com.verr1.controlcraft.registry.ControlCraftAttachments;
import com.verr1.controlcraft.registry.ControlCraftItems;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.HitResult;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;

import java.util.Arrays;
import java.util.List;

@Mod.EventBusSubscriber(modid = ControlCraft.MODID, bus = Mod.EventBusSubscriber.Bus.FORGE)
public class ControlCraftServerCommands {

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

    private static int saveAllConstraintsCommand(CommandContext<CommandSourceStack> $){
        ConstraintCenter.saveAll();
        return 1;
    }

    private static int freeCommand(CommandContext<CommandSourceStack> context){
        String name = context.getArgument("name", String.class);
        long protocol = context.getArgument("protocol", Long.class);

        ControlCraftServer.CC_NETWORK.free(new PeripheralNetwork.PeripheralKey(
                protocol,
                name
        ));

        return 1;
    }

    private static int circuitThreadCommand(CommandContext<CommandSourceStack> context){
        CommandSourceStack source = context.getSource();
        boolean bl = context.getArgument("physics", Boolean.class);
        BlockLinkPort.RUN_AT_PHYSICS_THREAD = bl;
        source.sendSuccess(bl ?
                () -> Component.literal("Circuit thread is set to physics thread.")
                :
                () -> Component.literal("Circuit thread is set to main thread."), false
        );

        return 1;
    }

    private static int observeCircuitCommand(CommandContext<CommandSourceStack> context){
        CommandSourceStack source = context.getSource();
        if(source.getPlayer() == null){
            source.sendFailure(Component.literal("You must be a player to set neglect a block!"));
            return 0;
        }
        ServerPlayer player = source.getPlayer();
        HitResult ht = player.pick(5, 1, false);
        if(!(ht instanceof BlockHitResult bht)){
            source.sendFailure(Component.literal("No block Found in your vicinity"));
            return 0;
        }
        BlockLinkPort.of(WorldBlockPos.of(player.serverLevel(), bht.getBlockPos())).filter(blp -> blp instanceof CircuitLinkPort).ifPresentOrElse(
                blp -> {
                    if(!(blp.__raw() instanceof Circuit circuit)){
                        source.sendFailure(Component.literal("blp does not have a circuit yet"));
                        return;
                    }
                    CircuitDebugger debugger = new CircuitDebugger(circuit);
                    List<String> connections = debugger.printConnectionsAsString();
                    connections.forEach(s -> {
                        source.sendSuccess(() -> Component.literal(s), false);
                    });
                    debugger.printConnections(); // at log
                },
                () -> source.sendFailure(Component.literal("BlockLinkPort Not Found"))
        );
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

    public static int inputCommand(CommandContext<CommandSourceStack> context){
        CommandSourceStack source = context.getSource();
        int index = context.getArgument("portId", Integer.class);
        double value = context.getArgument("value", Double.class);
        if(source.getPlayer() == null){
            source.sendFailure(Component.literal("You must be a player to set neglect a block!"));
            return 0;
        }
        ServerPlayer player = source.getPlayer();
        HitResult ht = player.pick(5, 1, false);
        if(!(ht instanceof BlockHitResult bht)){
            source.sendFailure(Component.literal("No block Found in your vicinity"));
            return 0;
        }
        BlockLinkPort.of(WorldBlockPos.of(player.serverLevel(), bht.getBlockPos())).ifPresentOrElse(
                blp -> {
                    blp.input(blp.in(index), value);
                    BlockLinkPort.propagateCombinational(new BlockLinkPort.PropagateContext(), blp);
                },
                () -> source.sendFailure(Component.literal("BlockLinkPort Not Found"))
        );
        return 1;
    }

    public static int outputCommand(CommandContext<CommandSourceStack> context){
        CommandSourceStack source = context.getSource();
        int index = context.getArgument("portId", Integer.class);
        double value = context.getArgument("value", Double.class);
        if(source.getPlayer() == null){
            source.sendFailure(Component.literal("You must be a player to set neglect a block!"));
            return 0;
        }
        ServerPlayer player = source.getPlayer();
        HitResult ht = player.pick(5, 1, false);
        if(!(ht instanceof BlockHitResult bht)){
            source.sendFailure(Component.literal("No block Found in your vicinity"));
            return 0;
        }
        BlockLinkPort.of(WorldBlockPos.of(player.serverLevel(), bht.getBlockPos())).ifPresentOrElse(
                blp -> {
                    blp.__raw().updateOutput(index, value);
                    BlockLinkPort.propagateCombinational(new BlockLinkPort.PropagateContext(), blp);
                },
                () -> source.sendFailure(Component.literal("BlockLinkPort Not Found"))
        );
        return 1;
    }

    public static int saveCircuitCommand(CommandContext<CommandSourceStack> context){
        CommandSourceStack source = context.getSource();
        String saveName = context.getArgument("saveName", String.class);
        if(source.getPlayer() == null){
            source.sendFailure(Component.literal("You must be a player to set save a circuit!"));
            return 0;
        }
        ServerPlayer player = source.getPlayer();
        ItemStack stack = player.getItemInHand(InteractionHand.MAIN_HAND);
        if(!stack.is(ControlCraftItems.CIRCUIT_COMPILER.get())){
            source.sendFailure(Component.literal("No compiler found in your main hand"));
            return 0;
        }
        CircuitCompilerItem.save(saveName, stack);
        return 1;
    }

    public static int loadCircuitCommand(CommandContext<CommandSourceStack> context){
        CommandSourceStack source = context.getSource();
        String saveName = context.getArgument("saveName", String.class);
        if(source.getPlayer() == null){
            source.sendFailure(Component.literal("You must be a player to set save a circuit!"));
            return 0;
        }
        ServerPlayer player = source.getPlayer();
        ItemStack stack = player.getItemInHand(InteractionHand.MAIN_HAND);
        if(!stack.is(ControlCraftItems.CIRCUIT_COMPILER.get())){
            source.sendFailure(Component.literal("No compiler found in your main hand"));
            return 0;
        }
        CircuitCompilerItem.load(saveName, stack);
        return 1;
    }

    public static void registerServerCommands(CommandDispatcher<CommandSourceStack> dispatcher){
        dispatcher.register(
            Commands
                .literal("controlcraft")
                .then(
                    lt("clear-attachment")
                        .executes(
                                context -> {
                                    clearAllAttachments();
                                    return 1;
                        })
                ).then(
                    lt("free-key").then(
                            arg("protocol", LongArgumentType.longArg()).then(
                                    arg("name", StringArgumentType.string()).executes(
                                        ControlCraftServerCommands::freeCommand
                                    )
                            )
                    )
                ).then(
                        lt("debug-count-fake-player").executes(
                                ControlCraftServerCommands::countFPCommand
                        )
                ).then(
                        lt("save-all-constraints").executes(
                                ControlCraftServerCommands::saveAllConstraintsCommand
                        )
                )
        );
        dispatcher.register(
                Commands.literal("cimulink")
                        .then(lt("in")
                            .then(arg("portId", IntegerArgumentType.integer())
                            .then(arg("value", DoubleArgumentType.doubleArg())
                            .executes(
                                ControlCraftServerCommands::inputCommand
                            )))
                        ).then(lt("out")
                            .then(arg("portId", IntegerArgumentType.integer())
                            .then(arg("value", DoubleArgumentType.doubleArg())
                            .executes(
                                    ControlCraftServerCommands::outputCommand
                            )))
                        ).then(lt("tick-at-physics-thread")
                            .then(arg("physics", BoolArgumentType.bool())
                            .executes(
                                    ControlCraftServerCommands::circuitThreadCommand
                            ))
                        ).then(lt("observe-circuit")
                            .executes(
                                    ControlCraftServerCommands::observeCircuitCommand
                            )
                        ).then(lt("save-circuit")
                            .then(arg("saveName", StringArgumentType.string())
                                .executes(ControlCraftServerCommands::saveCircuitCommand))
                        ).then(lt("load-circuit")
                            .then(arg("saveName", StringArgumentType.string())
                                .executes(ControlCraftServerCommands::loadCircuitCommand))
                        )
        );
    }



    @SubscribeEvent
    public static void onRegisterCommands(RegisterCommandsEvent event) {
        registerServerCommands(event.getDispatcher());
    }

}
