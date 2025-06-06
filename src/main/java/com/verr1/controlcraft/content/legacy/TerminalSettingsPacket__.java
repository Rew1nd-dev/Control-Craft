package com.verr1.controlcraft.content.legacy;


import com.simibubi.create.foundation.networking.SimplePacketBase;
import net.createmod.catnip.data.Couple;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.data.terminal.TerminalRowSetting;
import net.minecraft.core.BlockPos;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.network.NetworkEvent;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class TerminalSettingsPacket__ extends SimplePacketBase {

    private final BlockPos pos;
    private final int rows;
    private final int exposedChannelIndex;
    private final List<Couple<Double>> row_min_max = new ArrayList<>();
    private final List<Boolean> row_enabled = new ArrayList<>();
    private final List<Boolean> row_reversed = new ArrayList<>();

    public TerminalSettingsPacket__(List<TerminalRowSetting> rowSettings, BlockPos pos, int exposedChannelIndex) {
        rowSettings.stream().map(TerminalRowSetting::enabled).forEach(row_enabled::add);
        rowSettings.stream().map(TerminalRowSetting::min_max).forEach(row_min_max::add);
        rowSettings.stream().map(TerminalRowSetting::isReversed).forEach(row_reversed::add);
        this.rows = rowSettings.size();
        this.pos = pos;
        this.exposedChannelIndex = exposedChannelIndex;
    }

    public TerminalSettingsPacket__(FriendlyByteBuf buf) {
        pos = buf.readBlockPos();
        rows = buf.readInt();
        exposedChannelIndex = buf.readInt();
        for (int i = 0; i < rows; i++) {
            row_enabled.add(buf.readBoolean());
            row_min_max.add(Couple.create(buf.readDouble(), buf.readDouble()));
            row_reversed.add(buf.readBoolean());
        }
    }

    @Override
    public void write(FriendlyByteBuf buffer) {
        buffer.writeBlockPos(pos);
        buffer.writeInt(rows);
        buffer.writeInt(exposedChannelIndex);
        for (int i = 0; i < rows; i++) {
            buffer.writeBoolean(row_enabled.get(i));
            buffer.writeDouble(row_min_max.get(i).get(true));
            buffer.writeDouble(row_min_max.get(i).get(false));
            buffer.writeBoolean(row_reversed.get(i));
        }
    }

    @Override
    public boolean handle(NetworkEvent.Context context) {
        context.enqueueWork(() ->
                Optional
                    .ofNullable(context.getSender())
                    .map(ServerPlayer::serverLevel)
                    .flatMap(level -> BlockEntityGetter.getLevelBlockEntityAt(level, pos, TerminalBlockEntity__.class))
                    .ifPresent(terminal -> {
                                terminal.setMinMax(row_min_max);
                                terminal.setEnabled(row_enabled);
                                terminal.setReversed(row_reversed);
                                terminal.setExposedChannel(exposedChannelIndex);
                                ControlCraftServer.SERVER_EXECUTOR.executeLater(terminal::setFrequency, 10);
                        }
            ));
        return true;
    }
}
