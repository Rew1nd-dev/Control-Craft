package com.verr1.controlcraft.foundation.network.packets.specific;

import com.simibubi.create.foundation.networking.SimplePacketBase;
import com.verr1.controlcraft.foundation.input.ControlChairAxis;
import com.verr1.controlcraft.foundation.input.ControlChairInputDelta;
import com.verr1.controlcraft.foundation.input.ControlChairInputServerWorker;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.network.NetworkEvent;

public class ControlChairInputPacket extends SimplePacketBase {
    private final long sequence;
    private final boolean fullSnapshot;
    private final boolean acceptingInput;
    private final boolean thirdPersonView;
    private final float viewYaw;
    private final float viewPitch;
    private final int axisMask;
    private final float[] axisValues;
    private final long pressedButtonsMask;
    private final long releasedButtonsMask;
    private final boolean hasHeldButtonsMask;
    private final long heldButtonsMask;

    public ControlChairInputPacket(ControlChairInputDelta delta) {
        this.sequence = delta.sequence();
        this.fullSnapshot = delta.isFullSnapshot();
        this.acceptingInput = delta.acceptingInput();
        this.thirdPersonView = delta.thirdPersonView();
        this.viewYaw = (float) delta.viewYaw();
        this.viewPitch = (float) delta.viewPitch();

        int localAxisMask = 0;
        int updateCount = 0;
        for (ControlChairAxis axis : ControlChairAxis.VALUES) {
            if (!delta.axisUpdates().containsKey(axis)) {
                continue;
            }
            localAxisMask |= 1 << axis.ordinal();
            updateCount++;
        }

        this.axisMask = localAxisMask;
        this.axisValues = new float[updateCount];
        int writeIndex = 0;
        for (ControlChairAxis axis : ControlChairAxis.VALUES) {
            Double value = delta.axisUpdates().get(axis);
            if (value == null) {
                continue;
            }
            axisValues[writeIndex++] = value.floatValue();
        }

        this.pressedButtonsMask = delta.pressedButtonsMask();
        this.releasedButtonsMask = delta.releasedButtonsMask();
        this.hasHeldButtonsMask = delta.heldButtonsMask() != null;
        this.heldButtonsMask = delta.heldButtonsMask() == null ? 0L : delta.heldButtonsMask();
    }

    public ControlChairInputPacket(FriendlyByteBuf buf) {
        this.sequence = buf.readVarLong();
        this.fullSnapshot = buf.readBoolean();
        this.acceptingInput = buf.readBoolean();
        this.thirdPersonView = buf.readBoolean();
        this.viewYaw = buf.readFloat();
        this.viewPitch = buf.readFloat();
        this.axisMask = buf.readVarInt();
        this.axisValues = new float[Integer.bitCount(axisMask)];
        for (int i = 0; i < axisValues.length; i++) {
            axisValues[i] = buf.readFloat();
        }
        this.pressedButtonsMask = buf.readLong();
        this.releasedButtonsMask = buf.readLong();
        this.hasHeldButtonsMask = buf.readBoolean();
        this.heldButtonsMask = hasHeldButtonsMask ? buf.readLong() : 0L;
    }

    @Override
    public void write(FriendlyByteBuf buf) {
        buf.writeVarLong(sequence);
        buf.writeBoolean(fullSnapshot);
        buf.writeBoolean(acceptingInput);
        buf.writeBoolean(thirdPersonView);
        buf.writeFloat(viewYaw);
        buf.writeFloat(viewPitch);
        buf.writeVarInt(axisMask);
        for (float axisValue : axisValues) {
            buf.writeFloat(axisValue);
        }
        buf.writeLong(pressedButtonsMask);
        buf.writeLong(releasedButtonsMask);
        buf.writeBoolean(hasHeldButtonsMask);
        if (hasHeldButtonsMask) {
            buf.writeLong(heldButtonsMask);
        }
    }

    @Override
    public boolean handle(NetworkEvent.Context context) {
        ServerPlayer sender = context.getSender();
        if (sender == null) {
            return true;
        }

        ControlChairInputServerWorker.submit(sender.getUUID(), toDelta());
        return true;
    }

    private ControlChairInputDelta toDelta() {
        ControlChairInputDelta.Builder builder = ControlChairInputDelta.builder(sequence)
                .acceptingInput(acceptingInput)
                .thirdPersonView(thirdPersonView)
                .viewAngles(viewYaw, viewPitch);
        if (fullSnapshot) {
            builder.fullSnapshot();
        }

        int readIndex = 0;
        for (ControlChairAxis axis : ControlChairAxis.VALUES) {
            if ((axisMask & (1 << axis.ordinal())) == 0) {
                continue;
            }
            builder.axis(axis, axisValues[readIndex++]);
        }

        builder.pressedButtonsMask(pressedButtonsMask);
        builder.releasedButtonsMask(releasedButtonsMask);
        if (hasHeldButtonsMask) {
            builder.heldButtonsMask(heldButtonsMask);
        }
        return builder.build();
    }
}
