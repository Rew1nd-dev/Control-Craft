package com.verr1.controlcraft.unstable.commands;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.arguments.ArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.builder.RequiredArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.unstable.AIServer;
import com.verr1.controlcraft.unstable.data.schematic.AISchematic;
import com.verr1.controlcraft.unstable.data.schematic.SchematicKey;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.arguments.coordinates.Vec3Argument;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.HitResult;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import org.joml.Quaterniond;
import org.valkyrienskies.core.api.ships.ServerShip;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toJOML;


@Mod.EventBusSubscriber(modid = ControlCraft.MODID, bus = Mod.EventBusSubscriber.Bus.FORGE)
public class AIServerCommands {

    private static LiteralArgumentBuilder<CommandSourceStack> lt(String name){
        return LiteralArgumentBuilder.literal(name);
    }

    private static<T> RequiredArgumentBuilder<CommandSourceStack, T> arg(String name, ArgumentType<T> type){
        return RequiredArgumentBuilder.argument(name, type);
    }

    private static int debugSaveSchematicCommand(CommandContext<CommandSourceStack> context){
        CommandSourceStack source = context.getSource();



        String namespace = context.getArgument("namespace", String.class);
        String name = context.getArgument("name", String.class);


        if(source.getPlayer() == null){
            source.sendFailure(Component.literal("You must be a player to perform this command!"));
            return 0;
        }
        ServerPlayer player = source.getPlayer();

        HitResult hit = player.pick(10, 1, false);
        if(!(hit instanceof BlockHitResult blockHitResult))return 0;

        WorldBlockPos pos = WorldBlockPos.of(player.level(), blockHitResult.getBlockPos());
        ServerShip ship = AIServer.MANAGER.getShipAt(pos).orElse(null);

        if(ship == null){
            source.sendFailure(Component.literal("No ship found at your vicinity!"));
            return 0;
        }

        AIServer.SCHEMATICS_MANAGER.createSchematicsAsync(pos, ship, namespace, name);
        return 1;
    }

    private static int debugRepairShipCommand(CommandContext<CommandSourceStack> context){
        CommandSourceStack source = context.getSource();



        String namespace = context.getArgument("namespace", String.class);
        String name = context.getArgument("name", String.class);


        if(source.getPlayer() == null){
            source.sendFailure(Component.literal("You must be a player to perform this command!"));
            return 0;
        }
        ServerPlayer player = source.getPlayer();

        HitResult hit = player.pick(10, 1, false);
        if(!(hit instanceof BlockHitResult blockHitResult))return 0;

        WorldBlockPos pos = WorldBlockPos.of(player.level(), blockHitResult.getBlockPos());
        ServerShip ship = AIServer.MANAGER.getShipAt(pos).orElse(null);

        if(ship == null){
            source.sendFailure(Component.literal("No ship found at your vicinity!"));
            return 0;
        }

        AISchematic schematic = AIServer.SCHEMATICS_MANAGER.getLoaded(new SchematicKey(namespace, name));

        if(schematic == null){
            source.sendFailure(Component.literal("No schematic found with the given namespace and name!"));
            return 0;
        }

        schematic.repairAt(pos.pos(), player.serverLevel());

        return 1;
    }

    private static int debugSpawnCommand(CommandContext<CommandSourceStack> context){
        Vec3 position = Vec3Argument.getVec3(context, "coordinate");
        String namespace = context.getArgument("namespace", String.class);
        String name = context.getArgument("name", String.class);

        AIServer.MANAGER.spawnType(new SchematicKey(namespace, name), toJOML(position), new Quaterniond());

        return 1;
    }

    private static int debugDiscardCommand(CommandContext<CommandSourceStack> context){
        CommandSourceStack source = context.getSource();


        if(source.getPlayer() == null){
            source.sendFailure(Component.literal("You must be a player to perform this command!"));
            return 0;
        }
        ServerPlayer player = source.getPlayer();

        HitResult hit = player.pick(10, 1, false);
        if(!(hit instanceof BlockHitResult blockHitResult))return 0;

        WorldBlockPos pos = WorldBlockPos.of(player.level(), blockHitResult.getBlockPos());
        ServerShip ship = AIServer.MANAGER.getShipAt(pos).orElse(null);

        if(ship == null){
            source.sendFailure(Component.literal("No ship found at your vicinity!"));
            return 0;
        }

        AIServer.MANAGER.discard(ship.getId());

        return 1;
    }

    private static int debugDiscardAllCommand(CommandContext<CommandSourceStack> context){
        AIServer.MANAGER.reset();
        return 1;
    }

    private static int debugJoinPollCommand(CommandContext<CommandSourceStack> context){
        CommandSourceStack source = context.getSource();



        String namespace = context.getArgument("namespace", String.class);
        String name = context.getArgument("name", String.class);

        AIServer.MANAGER.joinPool(new SchematicKey(namespace, name), source.getLevel());

        return 1;
    }

    private static int listAllAICommand(CommandContext<CommandSourceStack> context){
        CommandSourceStack source = context.getSource();
        source.sendSuccess(() -> Component.literal(AIServer.MANAGER.listAllAI().toString()), false);
        return 1;
    }

    private static int listAvailableAICommand(CommandContext<CommandSourceStack> context){
        CommandSourceStack source = context.getSource();
        source.sendSuccess(() -> Component.literal(AIServer.MANAGER.listAvailableAI().toString()), false);
        return 1;
    }

    public static void registerServerCommands(CommandDispatcher<CommandSourceStack> dispatcher){
        dispatcher.register(
                lt("cai")
                .then(
                        lt("debug").then(
                                lt("save-schematic")
                                        .then(
                                                arg("namespace", StringArgumentType.string())
                                                        .then(
                                                                arg("name", StringArgumentType.string())
                                                                        .executes(AIServerCommands::debugSaveSchematicCommand)
                                                        )
                                        )
                        ).then(
                                lt("list-ai").executes(
                                        AIServerCommands::listAllAICommand

                                )
                        ).then(
                                lt("list-pool-ai").executes(
                                        AIServerCommands::listAvailableAICommand

                                )
                        ).then(
                                lt("rewind")
                                        .then(
                                                arg("namespace", StringArgumentType.string())
                                                        .then(
                                                                arg("name", StringArgumentType.string())
                                                                        .executes(AIServerCommands::debugRepairShipCommand)
                                                        )
                                        )
                        )
                ).then(
                        lt("spawn")
                                .then(
                                        arg("namespace", StringArgumentType.string())
                                        .suggests(SchematicSuggestion.NAMESPACE_SUGGESTIONS)
                                                .then(
                                                        arg("name", StringArgumentType.string())
                                                        .suggests(SchematicSuggestion.NAME_SUGGESTIONS)
                                                                .then(
                                                                        arg("coordinate", Vec3Argument.vec3()).executes(
                                                                                AIServerCommands::debugSpawnCommand
                                                                        )
                                                                )

                                                )
                                )
                ).then(
                        lt("kill")
                                .executes(AIServerCommands::debugDiscardCommand)
                ).then(
                        lt("kill-all")
                                .executes(AIServerCommands::debugDiscardAllCommand)
                ).then(
                                lt("join")
                                        .then(
                                                arg("namespace", StringArgumentType.string())
                                                        .suggests(SchematicSuggestion.NAMESPACE_SUGGESTIONS)
                                                        .then(
                                                                arg("name", StringArgumentType.string())
                                                                        .suggests(SchematicSuggestion.NAME_SUGGESTIONS)
                                                                        .executes(AIServerCommands::debugJoinPollCommand)
                                                        )
                                        )
                        )
        );
    }

    @SubscribeEvent
    public static void onRegisterCommands(RegisterCommandsEvent event) {
        registerServerCommands(event.getDispatcher());
    }

}
