package com.verr1.controlcraft.foundation.cimulink.game.port.inout;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.sources.MultiSink;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.nbt.CompoundTag;

import java.util.List;

public class MultiOutputLinkPort extends BlockLinkPort {



    private int size;

    public MultiOutputLinkPort() {
        super(new MultiSink(1));
        size = 1;
    }

    public void setSize(int size){
        if(size == this.size || size <= 0)return;
        this.size = size;
        recreate();
    }

    public List<Double> peek(){
        return __raw().peekInput();
    }

    public int size() {
        return size;
    }

    public double peek(int index){
        return __raw().peekInput(index);
    }

    public void addChannel() {setSize(size + 1);}

    public void removeChannel() {setSize(size - 1);}

    @Override
    public NamedComponent create() {
        return new MultiSink(size);
    }

    public CompoundTag serialize(){
        return CompoundTagBuilder.create()
                .withCompound("blp", super.serialize())
                .withCompound("size", SerializeUtils.INT.serialize(size))
                .build();
    }

    @Override
    public void deserialize(CompoundTag tag) {
        setSize(SerializeUtils.INT.deserialize(tag.getCompound("size")));
        super.deserialize(tag.getCompound("blp"));
    }
}
