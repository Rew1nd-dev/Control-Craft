package com.verr1.controlcraft.foundation.network.packets.specific;

import com.simibubi.create.foundation.blockEntity.SmartBlockEntity;
import com.simibubi.create.foundation.networking.SimplePacketBase;
import com.verr1.controlcraft.foundation.api.delegate.ITerminalDevice;
import com.verr1.controlcraft.foundation.type.descriptive.SlotDirection;
import com.verr1.controlcraft.foundation.type.descriptive.SlotType;
import net.minecraft.core.BlockPos;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraftforge.network.NetworkEvent;

public class ExposedFieldSettingsPacket extends SimplePacketBase {

    private final BlockPos pos;
    private final SlotType type;
    private final SlotDirection openTo;
    private final double min;
    private final double max;

    public ExposedFieldSettingsPacket(BlockPos pos, SlotType type, double min, double max, SlotDirection openTo) {
        this.pos = pos;
        this.type = type;
        this.min = min;
        this.max = max;
        this.openTo = openTo;
    }

    public ExposedFieldSettingsPacket(FriendlyByteBuf buffer) {
        pos = buffer.readBlockPos();
        type = buffer.readEnum(SlotType.class);
        min = buffer.readDouble();
        max = buffer.readDouble();
        openTo = buffer.readEnum(SlotDirection.class);
    }

    @Override
    public void write(FriendlyByteBuf buffer) {

        buffer.writeBlockPos(pos);
        buffer.writeEnum(type);
        buffer.writeDouble(min);
        buffer.writeDouble(max);
        buffer.writeEnum(openTo);
    }

    @Override
    public boolean handle(NetworkEvent.Context context) {
        context.enqueueWork(()->{
            BlockEntity be = context.getSender().level().getExistingBlockEntity(pos);
            if(be instanceof ITerminalDevice device){
                device.setExposedField(type, min, max, openTo);
            }
            if(be instanceof SmartBlockEntity){
                be.setChanged();
            }
        });
        return true;
    }
}
