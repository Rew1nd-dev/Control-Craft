package com.verr1.controlcraft.content.gui.layouts.element;

import com.verr1.controlcraft.content.gui.layouts.NetworkUIPort;
import com.verr1.controlcraft.foundation.api.delegate.INetworkHandle;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import net.minecraft.client.gui.layouts.GridLayout;
import net.minecraft.core.BlockPos;

import java.util.List;
import java.util.function.Consumer;
import java.util.function.Supplier;

import static com.verr1.controlcraft.content.gui.factory.GenericUIFactory.boundBlockEntity;

public abstract class MultipleTypedUIPort extends NetworkUIPort<List<Object>> {

    public final List<KeyWithType> kwt;

    public MultipleTypedUIPort(
            BlockPos boundPos,
            List<KeyWithType> kTypes
    ) {
        super(obJs -> handleWrite(boundPos, kTypes, obJs), () -> handleRead(boundPos, kTypes));
        kwt = kTypes;
    }

    public static void handleWrite(BlockPos boundPos, List<KeyWithType> kTypes, List<Object> inputs){
        int size = Math.min(kTypes.size(), inputs.size());
        for (int i = 0; i < size; i++) {
            KeyWithType kType = kTypes.get(i);

            Object input = inputs.get(i);
            if(input == null || !input.getClass().isAssignableFrom(kType.type)) {
                input = kType.defaultValue;
            }
            final Object finalInput = input;

            boundBlockEntity(boundPos, INetworkHandle.class)
                    .ifPresent(be -> be.handler().writeClientBuffer(kType.key(), finalInput));
        }
    }

    public static List<Object> handleRead(BlockPos boundPos, List<KeyWithType> kTypes) {
        return kTypes.stream()
                .map(kType ->
                        boundBlockEntity(boundPos, INetworkHandle.class)
                        .map(be -> (Object)be.handler().readClientBuffer(kType.key(), kType.type))
                        .orElse(kType.defaultValue))
                .toList();
    }

    public List<NetworkKey> keys(){
        return kwt.stream().map(KeyWithType::key).toList();
    }

    @Override
    protected final void writeGUI(List<Object> value) {
        writeGUIWithType(
                value.stream().map(v -> new ValueWithType(v, v == null ? Object.class : v.getClass())).toList()
        );
    }

    protected abstract void writeGUIWithType(List<ValueWithType> vwt);


    public record ValueWithType(
            Object value,
            Class<?> type
    ) {

        public<T> T cast(Class<T> clazz){
            if(clazz.isAssignableFrom(type)){
                return clazz.cast(value);
            }
            throw new ClassCastException("Cannot cast " + value.getClass().getName() + " to " + clazz.getName());
        }

    }

    public record KeyWithType(
            NetworkKey key,
            Class<?> type,
            Object defaultValue
    ) {}

}
