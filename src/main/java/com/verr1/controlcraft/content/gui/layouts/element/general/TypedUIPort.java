package com.verr1.controlcraft.content.gui.layouts.element.general;

import com.verr1.controlcraft.content.gui.layouts.NetworkUIPort;
import com.verr1.controlcraft.foundation.api.delegate.INetworkHandle;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import net.minecraft.core.BlockPos;

import static com.verr1.controlcraft.content.gui.factory.GenericUIFactory.boundBlockEntity;

public abstract class TypedUIPort<T> extends NetworkUIPort<T> {

    private final NetworkKey key;
    private final BlockPos boundPos;
    private final Class<T> dataType;
    private final T defaultValue;

    public TypedUIPort(BlockPos boundPos, NetworkKey key, Class<T> dataType, T defaultValue) {
        this.boundPos = boundPos;
        this.dataType = dataType;
        this.defaultValue = defaultValue;
        this.key = key;
    }

    @Override
    protected void consume(T data) {
        boundBlockEntity(boundPos, INetworkHandle.class)
                .ifPresent(be -> be.handler().writeClientBuffer(key, data, dataType));
    }

    @Override
    protected T provide() {
        return boundBlockEntity(boundPos, INetworkHandle.class)
                .map(be -> be.handler().readClientBuffer(key, dataType))
                .orElse(defaultValue);
    }

    public NetworkKey key(){
        return key;
    }
}
