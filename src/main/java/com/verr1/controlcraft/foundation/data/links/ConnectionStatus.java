package com.verr1.controlcraft.foundation.data.links;

import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.nbt.CompoundTag;

import java.util.*;

public class ConnectionStatus {
    public static final ConnectionStatus EMPTY = new ConnectionStatus();

    public static final SerializeUtils.Serializer<List<String>> NAMES =
            SerializeUtils.ofList(SerializeUtils.STRING);



    public final Map<String, Set<BlockPort>> outputPorts = new HashMap<>();
    public final Map<String, BlockPort> inputPorts = new HashMap<>();

    public final List<String> inputs = new ArrayList<>();
    public final List<String> outputs = new ArrayList<>();


    public ConnectionStatus(){}

    public ConnectionStatus(
            Map<String, Set<BlockPort>> outputPorts,
            Map<String, BlockPort> inputPorts,
            List<String> inputs,
            List<String> outputs
    ) {
        this.outputPorts.putAll(outputPorts);
        this.inputPorts.putAll(inputPorts);
        this.inputs.addAll(inputs);
        this.outputs.addAll(outputs);
    }


    public static CompoundTag summarize(BlockLinkPort blp){
        return CompoundTagBuilder.create()
                .withCompound("forward", blp.serializeForward())
                .withCompound("backward", blp.serializeBackward())
                .withCompound("inputs", NAMES.serialize(blp.inputsNames()))
                .withCompound("outputs", NAMES.serialize(blp.outputsNames()))
                .build();
    }

    public static ConnectionStatus deserialize(CompoundTag tag){
        return new ConnectionStatus(
                BlockLinkPort.deserializeForward(tag.getCompound("forward")),
                BlockLinkPort.deserializeBackward(tag.getCompound("backward")),
                NAMES.deserialize(tag.getCompound("inputs")),
                NAMES.deserialize(tag.getCompound("outputs"))
        );
    }


}
