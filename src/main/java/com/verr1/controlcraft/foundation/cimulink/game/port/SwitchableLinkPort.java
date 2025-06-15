package com.verr1.controlcraft.foundation.cimulink.game.port;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.Inspectable;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.nbt.CompoundTag;

import java.util.function.Function;

public abstract class SwitchableLinkPort<T extends Enum<?>> extends BlockLinkPort {

    private final SerializeUtils.Serializer<T> TYPE;

    private T currentType;

    private final Function<T, NamedComponent> factory;

    protected SwitchableLinkPort(T defaultValue, Function<T, NamedComponent> factory) {
        super(factory.apply(defaultValue));
        TYPE = SerializeUtils.ofEnum(clazz());
        currentType = defaultValue;
        this.factory = factory;
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
        return factory.apply(currentType);
    }

    @Override
    public CompoundTag serialize() {
        return CompoundTagBuilder.create()
                .withCompound("blp", super.serialize())
                .withCompound("current_type", TYPE.serializeNullable(currentType))
                .build();
    }

    @Override
    public void deserialize(CompoundTag tag){
        setCurrentType(TYPE.deserialize(tag.getCompound("current_type")));
        super.deserialize(tag.getCompound("blp"));
    }



}


