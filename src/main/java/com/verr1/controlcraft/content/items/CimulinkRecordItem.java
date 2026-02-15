package com.verr1.controlcraft.content.items;

import com.verr1.controlcraft.content.valkyrienskies.attachments.CimulinkPorts;
import com.verr1.controlcraft.foundation.cimulink.core.records.ComponentPortName;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.utils.*;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.Level;
import org.jetbrains.annotations.NotNull;
import org.valkyrienskies.core.api.ships.ServerShip;

import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

public class CimulinkRecordItem extends Item {
    public static final Serializer<Map<ComponentPortName, ComponentPortName>> MAP_SER =
        SerializeUtils.ofMap(
            ComponentPortName.SER,
            ComponentPortName.SER
        );


    public CimulinkRecordItem(Properties properties) {
        super(properties);
    }

    @Override
    public @NotNull InteractionResult useOn(@NotNull UseOnContext context) {
        Level world = context.getLevel();
        BlockPos blockPos = context.getClickedPos();
        Player player = context.getPlayer();
        ItemStack stack = context.getItemInHand();
        if(world.isClientSide() || player == null) {
            return InteractionResult.SUCCESS;
        }
        boolean store = !player.isShiftKeyDown();
        ServerShip ship = VSAccessUtils.getShipAt(WorldBlockPos.of(world, blockPos)).orElse(null);
        if(ship == null){
            return InteractionResult.PASS;
        }
        List<ServerShip> allShips = ConstraintClusterUtil.clusterOf(ship.getId()).stream().map(VSAccessUtils::getShipOf)
                .filter(Optional::isPresent)
                .map(Optional::get)
                .toList();

        Map<WorldBlockPos, BlockLinkPort> blps = allShips
                .stream()
                .map(CimulinkPorts::getOrCreate)
                .flatMap(b -> b.getAll().stream())
                .map(BlockLinkPort::of)
                .filter(Optional::isPresent)
                .map(Optional::get)
                .collect(Collectors.toMap(
                        BlockLinkPort::pos,
                        b -> b,
                        (a, b) -> a
                ));

        if(store){
            AtomicBoolean hasError = new AtomicBoolean(false);
            Map<String, Integer> duplicateCount = new HashMap<>();
            for(var blp : blps.values()){
                duplicateCount.put(blp.name(), duplicateCount.getOrDefault(blp.name(), 0) + 1);
            }
            duplicateCount.entrySet().stream().filter(e -> e.getValue() > 1).forEach(e -> {
                hasError.set(true);
                player.sendSystemMessage(Component.literal("Duplicate name: " + e.getKey() + " Count: " + e.getValue()));
            });
            if(hasError.get()){
                return InteractionResult.FAIL;
            }

            Map<ComponentPortName, ComponentPortName> in2out = new HashMap<>();
            Set<String> existInNames = new HashSet<>();
            for(var blp : blps.values()){
                if(existInNames.contains(blp.name())){
                    player.sendSystemMessage(Component.literal("Duplicate component name detected: " + blp.name()));
                    return InteractionResult.FAIL;
                }
                existInNames.add(blp.name());
                blp.backwardLinks().forEach((link, blockPort) -> {
                    ComponentPortName in = new ComponentPortName(blp.name(), link);
                    BlockLinkPort outPort = blps.get(blockPort.pos());
                    if(outPort == null){
                        player.sendSystemMessage(Component.literal("Missing: " + blockPort.pos()));
                        return;
                    }
                    ComponentPortName out = new ComponentPortName(outPort.name(), blockPort.portName());

                    in2out.put(in, out);
                });
            }

            stack.getOrCreateTag().put("in2out", MAP_SER.serialize(in2out));
            player.sendSystemMessage(Component.literal("Stored " + in2out.size() + " Links."));
        }else{
            Map<String, BlockLinkPort> name2Port = new HashMap<>();

            for(var blp : blps.values()){
                if(name2Port.containsKey(blp.name())){
                    player.sendSystemMessage(Component.literal("Duplicate component name detected: " + blp.name()));
                    return InteractionResult.FAIL;
                }
                name2Port.put(blp.name(), blp);
            }

            CompoundTag tag = stack.getTag();
            if(tag == null || !tag.contains("in2out")){
                player.sendSystemMessage(Component.literal("No stored links found."));
                return InteractionResult.FAIL;
            }

            Map<ComponentPortName, ComponentPortName> in2out = MAP_SER.deserialize(tag.getCompound("in2out"));
            int appliedCount = 0;
            for(var entry : in2out.entrySet()){
                ComponentPortName in = entry.getKey();
                ComponentPortName out = entry.getValue();

                BlockLinkPort inPort = name2Port.get(in.componentName());
                BlockLinkPort outPort = name2Port.get(out.componentName());

                if(inPort == null){
                    player.sendSystemMessage(Component.literal("Missing input component: " + in.componentName()));
                    continue;
                }
                if(outPort == null){
                    player.sendSystemMessage(Component.literal("Missing output component: " + out.componentName()));
                    continue;
                }
                try{
                    outPort.connectTo(out.portName(), inPort.pos(), in.portName());
                    appliedCount++;
                } catch (Exception e) {
                    player.sendSystemMessage(Component.literal(out + " -> " + in + " Failed With: " + e.getMessage()));
                }
            }

            player.sendSystemMessage(Component.literal("Applied " + appliedCount + " Links."));

        }
        return InteractionResult.SUCCESS;

    }


}
