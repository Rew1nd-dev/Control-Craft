package com.verr1.controlcraft.foundation.cimulink.game.port.digital;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.digital.gates.FlexibleGate;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.Summary;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.ISummarizable;
import com.verr1.controlcraft.foundation.cimulink.game.registry.CimulinkFactory;
import com.verr1.controlcraft.foundation.data.links.StringBoolean;
import com.verr1.controlcraft.foundation.data.links.StringBooleans;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.MathUtils;
import net.minecraft.nbt.CompoundTag;

import java.util.List;
import java.util.stream.IntStream;

public class FlexibleGateLinkPort extends BlockLinkPort implements ISummarizable {
    public static final int MAX_SIZE = 6;

    private int cachedSize;

    public FlexibleGateLinkPort() {
        super(new FlexibleGate(2));
        cachedSize = 2;
    }

    public boolean outputMask() {
        return gate().outputMask;
    }

    public void setOutputMask(boolean outputMask) {
        gate().outputMask = outputMask;
    }

    public boolean isAndGate() {
        return gate().isAndGate();
    }

    public int size(){
        return gate().n();
    }

    public void setSize(int n){
        if(n < 2 || n > MAX_SIZE)return;
        if(n == size())return;
        cachedSize = n;
        recreate();
    }

    public void setAndGate(boolean isAndGate) {
        gate().setAndGate(isAndGate);
    }

    public void setMask(String inputName, boolean value) {
        gate().setMask(in(inputName), value);
    }

    public void setMask(StringBooleans mask) {
        mask.statuses().forEach(
                sb -> setMask(
                        sb.name(),
                        sb.enabled()
                )
        );
    }

    public StringBooleans getMask() {
        return new StringBooleans(
                IntStream.range(0, n()).mapToObj(
                        i -> new StringBoolean(
                                in(i),
                                gate().viewMask().get(i))
                )
                        .toList()
        );
    }




    public FlexibleGate gate(){
        return (FlexibleGate) __raw();
    }

    @Override
    public NamedComponent create() {
        return gate().convertTo(cachedSize);
    }

    @Override
    public Summary summary() {
        return CimulinkFactory.F_GATE.summarize(gate());
    }

    @Override
    public CompoundTag serialize() {
        return CompoundTagBuilder.create()
                .withCompound("blp", super.serialize())
                .withCompound("gate", gate().serialize())
                .build();
    }

    @Override
    public void deserialize(CompoundTag tag) {

        FlexibleGate de = FlexibleGate.deserialize(tag.getCompound("gate"));

        setSize(de.n());
        gate().setAndGate(de.isAndGate());
        gate().setOutputMask(de.outputMask());
        gate().setMask(de.viewMask());

        super.deserialize(tag.getCompound("blp"));
    }
}
