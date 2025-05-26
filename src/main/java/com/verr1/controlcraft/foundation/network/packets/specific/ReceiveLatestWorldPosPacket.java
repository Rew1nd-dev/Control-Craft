package com.verr1.controlcraft.foundation.network.packets.specific;

import com.simibubi.create.foundation.networking.SimplePacketBase;
import com.verr1.controlcraft.foundation.api.delegate.INetworkHandle;
import com.verr1.controlcraft.foundation.managers.ClientCameraManager;
import net.minecraft.client.Minecraft;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.network.NetworkEvent;

import java.util.Optional;

public class ReceiveLatestWorldPosPacket extends SimplePacketBase {

    private final Vec3 latestWorldPos;

    public ReceiveLatestWorldPosPacket(Vec3 latestWorldPos) {
        this.latestWorldPos = latestWorldPos;
    }

    public ReceiveLatestWorldPosPacket(FriendlyByteBuf buf) {
        this.latestWorldPos = new Vec3(
                buf.readDouble(),
                buf.readDouble(),
                buf.readDouble()
        );
    }

    @Override
    public void write(FriendlyByteBuf friendlyByteBuf) {
        friendlyByteBuf.writeDouble(latestWorldPos.x);
        friendlyByteBuf.writeDouble(latestWorldPos.y);
        friendlyByteBuf.writeDouble(latestWorldPos.z);
    }
    // Test Push

    @Override
    public boolean handle(NetworkEvent.Context context) {
        context.enqueueWork(()-> ClientCameraManager.setQueryPos(latestWorldPos));
        return true;
    }
}
