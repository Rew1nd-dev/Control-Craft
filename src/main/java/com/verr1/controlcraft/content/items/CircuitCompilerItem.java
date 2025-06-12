package com.verr1.controlcraft.content.items;

import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
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
import org.jetbrains.annotations.Nullable;

public class CircuitCompilerItem extends Item {

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

            // 检查NBT数据是否存在




        }
        return InteractionResult.PASS;
    }

    @Override
    public InteractionResult onItemUseFirst(ItemStack stack, UseOnContext context) {
        Player player = context.getPlayer();
        if(player == null)return InteractionResult.FAIL;

        boolean shift = player.isShiftKeyDown();
        BlockPos pos = context.getClickedPos();


        State currentState = getState(stack);
        transitState(stack, shift, pos, currentState);
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

    private void transitState(ItemStack stack, boolean isShiftKeyDown, @Nullable BlockPos pos, State currentState) {
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

                    // 留空逻辑
                }
            }
            case TO_PLACE -> {
                if (isShiftKeyDown) {
                    nbt.remove("circuitNbt");
                } else if (nbt.contains("circuitNbt")) {
                    // 这里可以添加放置电路的逻辑
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

}
