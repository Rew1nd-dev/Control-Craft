package com.verr1.controlcraft.foundation.data.links;

import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.nbt.CompoundTag;

import java.util.List;

public record ProxyPortStatus(List<PortStatus> statuses) {
    public static final ProxyPortStatus EMPTY = new ProxyPortStatus(List.of());

    public static final SerializeUtils.Serializer<List<PortStatus>> SER =
            SerializeUtils.ofList(SerializeUtils.of(
                    PortStatus::serialize,
                    PortStatus::deserialize
            ));

    public CompoundTag serialize(){
        return SER.serialize(statuses);
    }

    public static ProxyPortStatus deserialize(CompoundTag tag){
        return new ProxyPortStatus(SER.deserialize(tag));
    }


}
