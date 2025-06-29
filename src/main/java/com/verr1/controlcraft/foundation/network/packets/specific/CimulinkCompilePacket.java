package com.verr1.controlcraft.foundation.network.packets.specific;

import com.simibubi.create.foundation.networking.SimplePacketBase;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitNbt;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitWorldBuilder;
import com.verr1.controlcraft.registry.ControlCraftItems;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.ChatFormatting;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.network.NetworkEvent;

public class CimulinkCompilePacket extends SimplePacketBase {
    private final BlockPos sel0;
    private final BlockPos sel1;

    public CimulinkCompilePacket(BlockPos sel0, BlockPos sel1) {
        this.sel0 = sel0;
        this.sel1 = sel1;
    }

    public CimulinkCompilePacket(FriendlyByteBuf buffer) {
        this.sel0 = buffer.readBlockPos();
        this.sel1 = buffer.readBlockPos();
    }

    @Override
    public void write(FriendlyByteBuf buffer) {
        buffer.writeBlockPos(sel0);
        buffer.writeBlockPos(sel1);
    }

    @Override
    public boolean handle(NetworkEvent.Context context) {
        context.enqueueWork(() -> {
            ServerPlayer player = context.getSender();
            if(player == null)return;
            ServerLevel level = player.serverLevel();
            try{
                CircuitNbt nbt = CircuitWorldBuilder.compile(level, sel0, sel1);
                CompoundTag tag = new CompoundTag();
                tag.put("circuitNbt", nbt.serialize());
                tag.put("sel0", SerializeUtils.BLOCK_POS.serialize(sel0));
                tag.put("sel1", SerializeUtils.BLOCK_POS.serialize(sel1));
                ItemStack compiledCircuit = new ItemStack(ControlCraftItems.CIRCUIT_COMPILER);
                compiledCircuit.setTag(tag);

                if (player.getInventory().add(compiledCircuit)) {
                    player.inventoryMenu.broadcastChanges(); // 更新客户端库存显示
                } else {
                    // 如果背包已满，掉落在地上
                    player.drop(compiledCircuit, false);
                }

                player.sendSystemMessage(
                        Component.literal("Compilation Done With Size: " + tag.sizeInBytes() + " Bytes"
                ).withStyle(s -> s.withColor(ChatFormatting.GREEN)));
                        ;

            }catch (Exception e){
                player.sendSystemMessage(Component.literal("Compilation Fail With Exception: ").withStyle(s -> s.withColor(ChatFormatting.RED).withBold(true)));
                player.sendSystemMessage(Component.literal(e.getMessage()).withStyle(s -> s.withColor(ChatFormatting.RED)));
            }


        });

        return true;
    }
}
