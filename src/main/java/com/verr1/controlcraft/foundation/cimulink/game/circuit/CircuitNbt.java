package com.verr1.controlcraft.foundation.cimulink.game.circuit;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.Circuit;
import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.CircuitConstructor;
import com.verr1.controlcraft.foundation.cimulink.game.registry.CimulinkFactory;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import net.minecraft.nbt.CompoundTag;

import java.io.IOException;
import java.util.List;

public class CircuitNbt {
    private static final Serializer<List<ComponentNbt>> SUMMARY_SERIALIZER =
            SerializeUtils.ofList(SerializeUtils.of(
                    ComponentNbt::serialize, ComponentNbt::deserialize
            ));

    private static final Serializer<List<ConnectionNbt>> CONNECTION_SERIALIZER =
            SerializeUtils.ofList(SerializeUtils.of(
                    ConnectionNbt::serialize, ConnectionNbt::deserialize
            ));

    private static final Serializer<List<IoNbt>> IO_SERIALIZER =
            SerializeUtils.ofList(SerializeUtils.of(
                    IoNbt::serialize, IoNbt::deserialize
            ));

    public static final CircuitNbt EMPTY_CONTEXT = new CircuitNbt(List.of(), List.of(), List.of());
    public static final Circuit EMPTY_CIRCUIT = EMPTY_CONTEXT.buildCircuit();

    List<ComponentNbt> componentSummaries;
    List<ConnectionNbt> connectionNbts;
    List<IoNbt> inOuts;


    CircuitNbt(
            List<ComponentNbt> componentSummaries,
            List<ConnectionNbt> connectionNbts,
            List<IoNbt> inOuts
    ){
        this.componentSummaries = componentSummaries;
        this.connectionNbts = connectionNbts;
        this.inOuts = inOuts;
    }

    public Circuit buildCircuit(){
        List<NamedComponent> components = componentSummaries.stream().map(
                s -> {
                    String name = s.componentName();
                    NamedComponent component = CimulinkFactory.restore(s.componentTag(), NamedComponent.class);
                    component.withName(name);
                    return component;
                }
        ).toList();

        CircuitConstructor constructor = new CircuitConstructor();

        constructor.addComponent(components.toArray(new NamedComponent[0]));

        for (ConnectionNbt connectionNbt : connectionNbts) {
            constructor.connect(
                    connectionNbt.outputName(),
                    connectionNbt.outputPortName(),
                    connectionNbt.inputName(),
                    connectionNbt.inputPortName()
            );
        }

        for (IoNbt io: inOuts){
            if(io.isInput()){
                constructor.defineInput(io.ioName(), io.componentName(), io.portName());
            }else{
                constructor.defineOutput(io.ioName(), io.componentName(), io.portName());
            }
        }

        return constructor.build().withBuildContext(this);

    }

    public CompoundTag serialize(){
        return CompoundTagBuilder.create()
                .withCompound("summary", SUMMARY_SERIALIZER.serialize(componentSummaries))
                .withCompound("connections", CONNECTION_SERIALIZER.serialize(connectionNbts))
                .withCompound("inOuts", IO_SERIALIZER.serialize(inOuts))
                .build();
    }

    public static CircuitNbt deserialize(CompoundTag tag) {
        return new CircuitNbt(
                SUMMARY_SERIALIZER.deserialize(tag.getCompound("summary")),
                CONNECTION_SERIALIZER.deserialize(tag.getCompound("connections")),
                IO_SERIALIZER.deserialize(tag.getCompound("inOuts"))
        );
    }

    public CompoundTag serializeCompressed() throws IOException {
        return CompoundTagBuilder.create()
                .withByteArray("compressed", SerializeUtils.compress(serialize()))
                .build();
    }

    public static CircuitNbt deserializeCompressed(CompoundTag tag) throws IOException {
        return deserialize(SerializeUtils.decompress(tag.getByteArray("compressed")));
    }


}
