package com.verr1.controlcraft.foundation.network.packets.specific;

import com.simibubi.create.foundation.networking.SimplePacketBase;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.content.items.CircuitCompilerItem;
import com.verr1.controlcraft.content.items.LuaCompilerItem;
import com.verr1.controlcraft.content.links.integration.CircuitBlockEntity;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.network.NetworkEvent;

public class CodeUploadPacket extends SimplePacketBase {

    private final String name;
    private final CodeUploadRequestPacket.Type type;
    private final CompoundTag tag;

    public CodeUploadPacket(CodeUploadRequestPacket.Type type, CompoundTag tag, String name) {
        this.type = type;
        this.tag = tag;
        this.name = name;
    }

    public CodeUploadPacket(FriendlyByteBuf buf) {
        this.type = buf.readEnum(CodeUploadRequestPacket.Type.class);
        this.tag = buf.readNbt();
        this.name = buf.readUtf();
    }

    @Override
    public void write(FriendlyByteBuf buffer) {
        buffer.writeEnum(type);
        buffer.writeNbt(tag);
        buffer.writeUtf(name);
    }

    @Override
    public boolean handle(NetworkEvent.Context context) {
        context.enqueueWork(() -> {
            ServerPlayer player = context.getSender();
            if(player == null)return;

            if(type == CodeUploadRequestPacket.Type.CIRCUIT){
                CircuitCompilerItem.saveTag(tag, name, player.getName().getString());
            }
            if(type == CodeUploadRequestPacket.Type.LUACUIT){
                LuaCompilerItem.saveTag(tag, name, player.getName().getString());
            }

        });

        return true;
    }
}
