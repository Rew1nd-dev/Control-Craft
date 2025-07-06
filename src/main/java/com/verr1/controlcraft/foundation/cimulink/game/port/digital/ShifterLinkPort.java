package com.verr1.controlcraft.foundation.cimulink.game.port.digital;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.analog.AsyncShifter;
import com.verr1.controlcraft.foundation.cimulink.core.components.analog.Shifter;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.ISummarizable;
import com.verr1.controlcraft.foundation.cimulink.game.registry.CimulinkFactory;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.Summary;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.nbt.CompoundTag;

public class ShifterLinkPort extends BlockLinkPort implements ISummarizable {


    private boolean async = false;
    private int parallel = 1;
    private int delay = 0;

    public ShifterLinkPort() {
        super(new Shifter(0, 1));
    }


    public void setParallel(long p){
        p = Math.max(1, p);
        parallel = (int)p;
        recreate();
    }

    public void setDelay(long d){
        delay = (int)d;
        recreate();
    }

    public void setAsync(boolean async){
        this.async = async;
        recreate();
    }

    public boolean async(){
        return async;
    }

    public long parallel() {
        return parallel;
    }

    public long delay() {
        return delay;
    }

    @Override
    public NamedComponent create() {
        return async ? new AsyncShifter(delay, parallel) : new Shifter(delay, parallel);
    }

    @Override
    public CompoundTag serialize() {
        return CompoundTagBuilder.create()
                .withCompound("blp", super.serialize())
                .withCompound("parallel", SerializeUtils.INT.serialize(parallel))
                .withCompound("delay", SerializeUtils.INT.serialize(delay))
                .withCompound("async", SerializeUtils.BOOLEAN.serialize(async))
                .build();
    }

    @Override
    public void deserialize(CompoundTag tag) {
        if(tag.contains("parallel"))parallel = SerializeUtils.INT.deserialize(tag.getCompound("parallel"));
        if(tag.contains("delay"))delay = SerializeUtils.INT.deserialize(tag.getCompound("delay"));
        if(tag.contains("async"))async = SerializeUtils.BOOLEAN.deserialize(tag.getCompound("async"));
        recreate();
        super.deserialize(tag.getCompound("blp"));
    }



    @Override
    public Summary summary() {
        if(async){
            return CimulinkFactory.ASYNC_SHIFTER.summarize(__raw());
        }
        return CimulinkFactory.SHIFTER.summarize(__raw());
    }
}
