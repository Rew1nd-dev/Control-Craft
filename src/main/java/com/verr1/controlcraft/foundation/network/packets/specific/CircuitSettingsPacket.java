package com.verr1.controlcraft.foundation.network.packets.specific;

import com.simibubi.create.foundation.networking.SimplePacketBase;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.content.links.circuit.CircuitBlockEntity;
import com.verr1.controlcraft.content.links.circuit.IoSettings;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.player.Player;
import net.minecraftforge.network.NetworkEvent;

import java.util.List;

public class CircuitSettingsPacket extends SimplePacketBase {

    private final SerializeUtils.Serializer<List<IoSettings>> SERIALIZERS =
            SerializeUtils.ofList(SerializeUtils.of(
                    IoSettings::serialize,
                    IoSettings::deserialize
            ));

    final BlockPos pos;
    final List<IoSettings> settings;

    public CircuitSettingsPacket(BlockPos pos, List<IoSettings> settings) {
        this.pos = pos;
        this.settings = settings;
    }

    public CircuitSettingsPacket(FriendlyByteBuf buf){
        pos = buf.readBlockPos();
        settings = SERIALIZERS.deserialize(buf.readNbt());
    }

    @Override
    public void write(FriendlyByteBuf buffer) {

        buffer.writeBlockPos(pos);
        buffer.writeNbt(SERIALIZERS.serialize(settings));
    }

    @Override
    public boolean handle(NetworkEvent.Context context) {
        context.enqueueWork(() -> {
            ServerPlayer player = context.getSender();
            if(player == null)return;
            ServerLevel level = player.serverLevel();
            BlockEntityGetter
                    .getLevelBlockEntityAt(level, pos, CircuitBlockEntity.class)
                    .ifPresent(be -> {
                        be.setWithIoSettings(settings);
                        ControlCraftServer.SERVER_EXECUTOR.executeLater(be::setFrequency, 10);
                    });

        });

        return true;
    }
}
