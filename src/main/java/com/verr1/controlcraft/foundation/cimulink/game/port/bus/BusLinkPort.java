package com.verr1.controlcraft.foundation.cimulink.game.port.bus;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import net.minecraft.nbt.CompoundTag;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class BusLinkPort extends BlockLinkPort {

    public static final Serializer<Map<String, Set<String>>> SER0 = SerializeUtils.ofMap(
            SerializeUtils.STRING,
            SerializeUtils.ofSet(SerializeUtils.STRING)
    );
    public static final Serializer<List<String>> SER1 = SerializeUtils.ofList(SerializeUtils.STRING);
    public static final Serializer<Status> SER = SerializeUtils.of(
            Status::serialize,
            Status::deserialize
    );

    private final List<String> definedInputs = new ArrayList<>();
    private final List<String> definedOutputs = new ArrayList<>();
    private final IBusContext context;

    public BusLinkPort(IBusContext context) {
        super(new BusPort(
                List.of(),
                List.of(),
                context
        ));
        this.context = context;
    }

    private BusPort bus(){
        return (BusPort) __raw();
    }

    @Override
    public boolean isCombinational() {
        return false;
    }

    public void updateCache(){
        bus().updateCache();
    }

    public void updateStatus(Status status){
        if(!status.availableIn.isEmpty() && !status.availableOut.isEmpty()){
            // ControlCraft.LOGGER.info("Why are you sending status with non-empty available back?");
        }
        definedInputs.clear();
        definedOutputs.clear();
        definedInputs.addAll(status.definedInputs);
        definedOutputs.addAll(status.definedOutputs);
        recreate();
    }


    public Status getStatus(){
        return new Status(
                context.allInputPorts(),
                context.allOutputPorts(),
                List.copyOf(definedInputs),
                List.copyOf(definedOutputs)
        );
    }


    @Override
    public NamedComponent create() {
        return new BusPort(
                List.copyOf(definedInputs),
                List.copyOf(definedOutputs),
                context
        );
    }

    @Override
    public CompoundTag serialize() {
        return CompoundTagBuilder.create()
                .withCompound("blp", super.serialize())
                .withCompound("definedIn", SER1.serialize(definedInputs))
                .withCompound("definedOut", SER1.serialize(definedOutputs))
                .build();
    }

    @Override
    public void deserialize(CompoundTag tag) {
        definedInputs.clear();
        definedInputs.addAll(SER1.deserialize(tag.getCompound("definedIn")));
        definedOutputs.clear();
        definedOutputs.addAll(SER1.deserialize(tag.getCompound("definedOut")));
        recreate();
        super.deserialize(tag.getCompound("blp"));
    }

    public record Status(
            Map<String, Set<String>> availableIn,
            Map<String, Set<String>> availableOut,
            List<String> definedInputs,
            List<String> definedOutputs
    ){

        public static final Status EMPTY = new Status(Map.of(), Map.of(), List.of(), List.of());

        public CompoundTag serialize(){
            return new CompoundTagBuilder()
                    .withCompound("availableIn", SER0.serialize(availableIn))
                    .withCompound("availableOut", SER0.serialize(availableOut))
                    .withCompound("definedInputs", SER1.serialize(definedInputs))
                    .withCompound("definedOutputs", SER1.serialize(definedOutputs))
                    .build();
        }

        public static Status deserialize(CompoundTag tag){
            return new Status(
                    SER0.deserialize(tag.getCompound("availableIn")),
                    SER0.deserialize(tag.getCompound("availableOut")),
                    SER1.deserialize(tag.getCompound("definedInputs")),
                    SER1.deserialize(tag.getCompound("definedOutputs"))
            );
        }

    }

}
