package com.verr1.controlcraft.content.gui.layouts.element;

import com.verr1.controlcraft.content.gui.layouts.element.general.TypedUIPort;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import net.minecraft.core.BlockPos;

import java.util.List;
import java.util.function.Function;

public abstract class ListUIPort<V, T> extends TypedUIPort<T> {

    final Function<T, List<V>> extractor;
    final Function<List<V>, T> restorer;

    public ListUIPort(
            BlockPos boundPos,
            NetworkKey key,
            Class<T> listHolderType,
            T defaultValue,
            Function<T, List<V>> extractor,
            Function<List<V>, T> restorer
    ) {
        super(boundPos, key, listHolderType, defaultValue);
        this.extractor = extractor;
        this.restorer = restorer;
    }


    @Override
    protected final T readGUI() {
        return restorer.apply(readList());
    }

    @Override
    protected final void writeGUI(T value) {
        writeList(extractor.apply(value));
    }


    protected abstract List<V> readList();


    protected abstract void writeList(List<V> value);
}
