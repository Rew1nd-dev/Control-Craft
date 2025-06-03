package com.verr1.controlcraft.foundation.cimulink.game.port;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.Inspectable;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.nbt.CompoundTag;

public abstract class SwitchableLinkPort<T extends Enum<?> & Inspectable<?>> extends BlockLinkPort {

    private final SerializeUtils.Serializer<T> TYPE;

    private T currentType;

    protected SwitchableLinkPort(WorldBlockPos portPos, T defaultValue) {
        super(portPos, defaultValue.inspector().get());
        TYPE = SerializeUtils.ofEnum(clazz());
    }

    protected abstract Class<T> clazz();

    public void setCurrentType(T type){
        if(type == currentType)return;
        currentType = type;
        recreate();
    }

    public T getCurrentType() {
        return currentType;
    }

    @Override
    public NamedComponent create() {
        return currentType.inspector().get();
    }


    public CompoundTag serialize() {
        return CompoundTagBuilder.create().withCompound("blp", serializeLinks())
                .withCompound("current_type", TYPE.serialize(currentType))
                .build();
    }

    public void deserialize(CompoundTag tag){
        setCurrentType(TYPE.deserialize(tag.getCompound("current_type")));
        deserializeLinks(tag.getCompound("blp"));
    }



}


