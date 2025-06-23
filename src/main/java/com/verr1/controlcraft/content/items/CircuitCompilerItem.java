package com.verr1.controlcraft.content.items;

import com.verr1.controlcraft.content.links.circuit.CircuitBlockEntity;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitNbt;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtIo;
import net.minecraft.network.chat.Component;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.Level;
import net.minecraftforge.fml.loading.FMLPaths;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

public class CircuitCompilerItem extends Item {

    public static final Path CIMULINKS = FMLPaths.GAMEDIR.get().resolve("cimulinks");

    public CircuitCompilerItem(Properties p_41383_) {
        super(p_41383_);
    }

    @Override
    public InteractionResult useOn(UseOnContext context) {
        Level world = context.getLevel();
        if (!world.isClientSide) { // 确保在服务端执行
            ItemStack stack = context.getItemInHand();
            Player player = context.getPlayer();
            BlockPos pos = context.getClickedPos();

            if(player == null)return InteractionResult.FAIL;

            CompoundTag nbt = stack.getOrCreateTag();
            // 检查NBT数据是否存在
            if (nbt.contains("circuitNbt")) {
                // 这里可以添加放置电路的逻辑
                CompoundTag circuitNbt = nbt.getCompound("circuitNbt");
                CircuitNbt nbtHolder = CircuitNbt.deserialize(circuitNbt);

                BlockEntityGetter.getLevelBlockEntityAt(world, pos, CircuitBlockEntity.class)
                        .ifPresentOrElse(
                                cbe -> cbe.loadCircuit(nbtHolder),
                                () -> player.sendSystemMessage(Component.literal("Not a circuit block found at the selected position."))
                        );

            }
        }
        return InteractionResult.PASS;
    }


    public static void save(String saveName, ItemStack stack){
        Path file = CIMULINKS.resolve(saveName).toAbsolutePath();
        CompoundTag data = stack.getOrCreateTag();
        try{
            Files.createDirectories(CIMULINKS);
            try(OutputStream out = Files.newOutputStream(file, StandardOpenOption.CREATE)){
                NbtIo.writeCompressed(data, out);
            }

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static void load(String saveName, ItemStack stack){
        Path file = CIMULINKS.resolve(saveName).toAbsolutePath();

        try(InputStream in = Files.newInputStream(file, StandardOpenOption.CREATE)){
            CompoundTag tag = NbtIo.readCompressed(in);
            stack.setTag(tag);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }


    }


/*
*
* @Override
    public InteractionResult onItemUseFirst(ItemStack stack, UseOnContext context) {
        Player player = context.getPlayer();
        if(context.getLevel().isClientSide)return InteractionResult.SUCCESS; // 在客户端直接返回成功
        if(player == null)return InteractionResult.FAIL;

        boolean shift = player.isShiftKeyDown();
        BlockPos pos = context.getClickedPos();


        State currentState = getState(stack);
        transitState(stack, shift, pos, currentState, (ServerLevel)context.getLevel(), player);
        player.sendSystemMessage(Component.literal(currentState.name()));

        return InteractionResult.SUCCESS;


    }

    private State getState(ItemStack stack){
        if(!stack.hasTag())return State.TO_SEL_0;
        CompoundTag nbt = stack.getTag();
        if(nbt == null)return State.TO_SEL_0;

        if(nbt.contains("sel0") && !nbt.contains("sel1")){
            return State.TO_SEL_1;
        }else if(nbt.contains("sel0") && nbt.contains("sel1") && !nbt.contains("circuitNbt")){
            return State.TO_COMPILE;
        } else if (nbt.contains("circuitNbt")) {
            return State.TO_PLACE;
        }
        return State.TO_SEL_0;
    }

    private void transitState(
            ItemStack stack,
            boolean isShiftKeyDown,
            @Nullable BlockPos pos,
            State currentState,
            ServerLevel level,
            Player player
    ) {
        CompoundTag nbt = stack.getOrCreateTag();

        switch (currentState) {
            case TO_SEL_0 -> {
                if (pos != null) {
                    nbt.put("sel0", CompoundTagBuilder.create().withCompound("pos", SerializeUtils.BLOCK_POS.serialize(pos)).build());
                }
            }
            case TO_SEL_1 -> {
                if (isShiftKeyDown) {
                    nbt.remove("sel0");
                } else if (pos != null) {
                    nbt.put("sel1", CompoundTagBuilder.create().withCompound("pos", SerializeUtils.BLOCK_POS.serialize(pos)).build());
                }
            }
            case TO_COMPILE -> {
                if (isShiftKeyDown) {
                    nbt.remove("sel1");
                }
                else if (nbt.contains("sel0") && nbt.contains("sel1")) {
                    BlockPos sel0Pos = SerializeUtils.BLOCK_POS.deserialize(nbt.getCompound("sel0").getCompound("pos"));
                    BlockPos sel1Pos = SerializeUtils.BLOCK_POS.deserialize(nbt.getCompound("sel1").getCompound("pos"));
                    try{
                        CompoundTag compiled = CircuitTagBuilder.compile(level, sel0Pos, sel1Pos).serialize();
                        nbt.put("circuitNbt", compiled);
                        player.sendSystemMessage(Component.literal("Circuit compiled successfully! size: " + compiled.sizeInBytes() + " bytes"));
                    }catch (Exception e){
                        player.sendSystemMessage(Component.literal("Compilation Exception: " + e.getMessage()));
                        nbt.remove("circuitNbt");
                    }

                }
            }
            case TO_PLACE -> {
                if (isShiftKeyDown) {
                    nbt.remove("circuitNbt");
                } else if (nbt.contains("circuitNbt")) {
                    // 这里可以添加放置电路的逻辑
                    CompoundTag circuitNbt = nbt.getCompound("circuitNbt");
                    CircuitNbt nbtHolder = CircuitNbt.deserialize(circuitNbt);

                    if(pos == null){
                        player.sendSystemMessage(Component.literal("Please select circuit block to load the circuit."));
                        return;
                    }
                    BlockEntityGetter.getLevelBlockEntityAt(level, pos, CircuitBlockEntity.class)
                            .ifPresentOrElse(
                                    cbe -> cbe.loadCircuit(nbtHolder),
                                    () -> player.sendSystemMessage(Component.literal("No circuit block found at the selected position."))
                            );

                }
            }
        }
    }

    enum State{
        TO_SEL_0,
        TO_SEL_1,
        TO_COMPILE,
        TO_PLACE,
    }
*
* */


}
