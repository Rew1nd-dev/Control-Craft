package com.verr1.controlcraft.foundation.network.packets.specific;

import com.simibubi.create.foundation.networking.SimplePacketBase;
import com.verr1.controlcraft.content.items.CircuitCompilerItem;
import com.verr1.controlcraft.content.items.LuaCompilerItem;
import com.verr1.controlcraft.registry.ControlCraftPackets;
import com.verr1.controlcraft.utils.MinecraftUtils;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.network.NetworkEvent;

public class CodeUploadRequestPacket extends SimplePacketBase {

    private final String name;
    private final Type type;

    public CodeUploadRequestPacket(String name, Type type) {
        this.name = name;
        this.type = type;
    }

    public CodeUploadRequestPacket(FriendlyByteBuf buf) {
        this.name = buf.readUtf();
        this.type = buf.readEnum(Type.class);
    }

    @Override
    public void write(FriendlyByteBuf buffer) {
        buffer.writeUtf(name);
        buffer.writeEnum(type);
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public boolean handle(NetworkEvent.Context context) {
        context.enqueueWork(() -> {
            try{
                CompoundTag tag = type == Type.CIRCUIT ?
                    CircuitCompilerItem.loadTag(name)
                    :
                    LuaCompilerItem.loadTag(name)
                    ;
                CodeUploadPacket cup = new CodeUploadPacket(type, tag, name);
                ControlCraftPackets.getChannel().sendToServer(cup);
            }catch (Exception e){
                MinecraftUtils.sendClientMessage(e.getMessage());
            }
        });
        return true;
    }

    public enum Type {
        CIRCUIT,
        LUACUIT
    }

}
