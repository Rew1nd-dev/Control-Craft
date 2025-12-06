package com.verr1.controlcraft.foundation.network.packets.specific.tweak;

import com.getitemfromblock.create_tweaked_controllers.packet.TweakedLinkedControllerAxisPacket;
import com.simibubi.create.foundation.networking.SimplePacketBase;
import com.verr1.controlcraft.content.compact.tweak.TweakControllerServerRecorder;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.network.NetworkEvent;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;
import java.util.stream.Stream;

public class TweakControllerFullAxisPacket extends SimplePacketBase {

    private final float[] fullAxis;

    public TweakControllerFullAxisPacket(float[] fullAxis) {
        this.fullAxis = fullAxis;
    }

    public TweakControllerFullAxisPacket(FriendlyByteBuf buf){
        this.fullAxis = new float[buf.readInt()];
        for(int i = 0; i < fullAxis.length; i++){
            fullAxis[i] = buf.readFloat();
        }
    }

    public void write(FriendlyByteBuf buf){
        buf.writeInt(fullAxis.length);
        for(float f : fullAxis){
            buf.writeFloat(f);
        }
    }

    @Override
    public boolean handle(NetworkEvent.Context context) {
        context.enqueueWork(() -> {
            ServerPlayer sender = context.getSender();
            if(sender == null)return;
            UUID uuid = sender.getUUID();
            List<Double> doubleValues = new ArrayList<>();
            for (float fullAxi : fullAxis) doubleValues.add((double) fullAxi);
            TweakControllerServerRecorder.receiveAxis(uuid, doubleValues);
        });
        return true;
    }


}
